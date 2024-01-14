use super::{ColMatrix, Felt, FieldElement, Vec, NUM_RAND_ROWS};
use crate::chiplets::Chiplets;
use core::slice;
use vm_core::utils::uninit_vector;

// TRACE FRAGMENT
// ================================================================================================

/// TODO: add docs
pub struct TraceFragment<'a> {
    data: Vec<&'a mut [Felt]>,
}

impl<'a> TraceFragment<'a> {
    /// Creates a new TraceFragment with its data allocated to the specified capacity.
    pub fn new(capacity: usize) -> Self {
        TraceFragment {
            data: Vec::with_capacity(capacity),
        }
    }

    // PUBLIC ACCESSORS
    // --------------------------------------------------------------------------------------------

    /// Returns the number of columns in this execution trace fragment.
    pub fn width(&self) -> usize {
        self.data.len()
    }

    /// Returns the number of rows in this execution trace fragment.
    pub fn len(&self) -> usize {
        self.data[0].len()
    }

    // DATA MUTATORS
    // --------------------------------------------------------------------------------------------

    /// Updates a single cell in this fragment with provided value.
    #[inline(always)]
    pub fn set(&mut self, row_idx: usize, col_idx: usize, value: Felt) {
        self.data[col_idx][row_idx] = value;
    }

    /// Returns a mutable iterator to the columns of this fragment.
    pub fn columns(&mut self) -> slice::IterMut<'_, &'a mut [Felt]> {
        self.data.iter_mut()
    }

    /// Adds a new column to this fragment by pushing a mutable slice with the first `len`
    /// elements of the provided column. Returns the rest of the provided column as a separate
    /// mutable slice.
    pub fn push_column_slice(&mut self, column: &'a mut [Felt], len: usize) -> &'a mut [Felt] {
        let (column_fragment, rest) = column.split_at_mut(len);
        self.data.push(column_fragment);
        rest
    }

    // TEST METHODS
    // --------------------------------------------------------------------------------------------

    #[cfg(test)]
    pub fn trace_to_fragment(trace: &'a mut [Vec<Felt>]) -> Self {
        let mut data = Vec::new();
        for column in trace.iter_mut() {
            data.push(column.as_mut_slice());
        }
        Self { data }
    }
}

// LOOKUP TABLES
// ================================================================================================

/// Defines a single row in a lookup table defined via multiset checks.
pub trait LookupTableRow {
    /// Returns a single element representing the row in the field defined by E. The value is
    /// computed using the provided random values.
    fn to_value<E: FieldElement<BaseField = Felt>>(
        &self,
        main_trace: &ColMatrix<Felt>,
        rand_values: &[E],
    ) -> E;
}

// v1 v2 v3   v4
// 1 v1 v1v2 v1v2v3 
// 1/v1   1/v2    1/v3 1/v4

/// Computes values as well as inverse value for all specified lookup table rows.
///
/// To compute the inverses of row values we use a modified version of batch inversion algorithm.
/// The main modification is that we don't need to check for ZERO values, because, assuming
/// random values are drawn from a large enough field, coming across a ZERO value should be
/// computationally infeasible.
pub fn build_lookup_table_row_values<E: FieldElement<BaseField = Felt>, R: LookupTableRow>(
    rows: &[R],
    main_trace: &ColMatrix<Felt>,
    rand_values: &[E],
) -> (Vec<E>, Vec<E>) {
    let mut row_values = unsafe { uninit_vector(rows.len()) };
    let mut inv_row_values = unsafe { uninit_vector(rows.len()) };

    // compute row values and compute their product
    let mut acc = E::ONE;
    for ((row, value), inv_value) in
        rows.iter().zip(row_values.iter_mut()).zip(inv_row_values.iter_mut())
    {
        *inv_value = acc;
        *value = row.to_value(main_trace, rand_values);
        debug_assert_ne!(*value, E::ZERO, "row value cannot be ZERO");

        acc *= *value;
    }

    // invert the accumulated product
    acc = acc.inv();

    // multiply the accumulated value by original values to compute inverses
    for i in (0..row_values.len()).rev() {
        inv_row_values[i] *= acc;
        acc *= row_values[i];
    }

    (row_values, inv_row_values)
}

// AUX COLUMN BUILDER
// ================================================================================================

/// Defines a builder responsible for building a single column in an auxiliary segment of the
/// execution trace.
pub trait AuxColumnBuilder<H: Clone, R: LookupTableRow, U: HintCycle> {
    // REQUIRED METHODS
    // --------------------------------------------------------------------------------------------

    /// Returns an exhaustive list of rows which are present in the table.
    fn get_table_rows(&self) -> &[R];

    /// Returns a sequence of hints which indicate how the table was updated. Each hint consists
    /// of a clock cycle at which the update happened as well as the hint describing the update.
    fn get_table_hints(&self) -> &[(U, H)];

    /// Returns a value by which the current value of the column should be multiplied to get the
    /// next value. It is expected that this value should never be ZERO in practice.
    fn get_multiplicand<E: FieldElement<BaseField = Felt>>(
        &self,
        hint: H,
        row_values: &[E],
        inv_row_values: &[E],
    ) -> E;

    // PROVIDED METHODS
    // --------------------------------------------------------------------------------------------

    /// Builds and returns the auxiliary trace column managed by this builder.
    fn build_aux_column<E>(&self, main_trace: &ColMatrix<Felt>, alphas: &[E]) -> Vec<E>
    where
        E: FieldElement<BaseField = Felt>,
    {
        // compute row values and their inverses for all rows that were added to the table
        let (row_values, inv_row_values) = self.build_row_values(main_trace, alphas);

        // allocate memory for the running product column and set its initial value
        let mut result = unsafe { uninit_vector(main_trace.num_rows()) };
        println!("main trace {}", main_trace.num_rows());
        result[0] = self.init_column_value(&row_values);

        // keep track of the last updated row in the running product column
        let mut result_idx = 0_usize;

        // iterate through the list of updates and apply them one by one
        for (clk, hint) in self.get_table_hints() {
            let clk = clk.as_index();

            // if we skipped some cycles since the last update was processed, values in the last
            // updated row should by copied over until the current cycle.
            if result_idx < clk {
                let last_value = result[result_idx];
                result[(result_idx + 1)..=clk].fill(last_value);
            }

            // move the result pointer to the next row
            result_idx = clk + 1;

            // apply the relevant updates to the column; since the multiplicand value should be
            // generated by "mixing-in" random values from a large field, the probability that we
            // get a ZERO should be negligible (i.e., it should never come up in practice).
            let multiplicand = self.get_multiplicand(hint.clone(), &row_values, &inv_row_values);
            debug_assert_ne!(E::ZERO, multiplicand);
            result[result_idx] = result[clk] * multiplicand;
        }

        // after all updates have been processed, the table should not change; we make sure that
        // the last value in the column is equal to the expected value, and fill in all the
        // remaining column values with the last value
        let last_value = result[result_idx];
        assert_eq!(last_value, self.final_column_value(&row_values));
        if result_idx < result.len() - 1 {
            result[(result_idx + 1)..].fill(last_value);
        }

        println!("aux trace: {}", result.len());
        for res in &result {
            print!("{} ", res);
        }
        println!();
        result
    }

    /// Builds and returns row values and their inverses for all rows which were added to the
    /// lookup table managed by this column builder.
    fn build_row_values<E>(&self, main_trace: &ColMatrix<Felt>, alphas: &[E]) -> (Vec<E>, Vec<E>)
    where
        E: FieldElement<BaseField = Felt>,
    {
        build_lookup_table_row_values(self.get_table_rows(), main_trace, alphas)
    }

    /// Returns the initial value in the auxiliary column. Default implementation of this method
    /// returns ONE.
    fn init_column_value<E: FieldElement<BaseField = Felt>>(&self, _row_values: &[E]) -> E {
        E::ONE
    }

    /// Returns the final value in the auxiliary column. Default implementation of this method
    /// returns ONE.
    fn final_column_value<E: FieldElement<BaseField = Felt>>(&self, _row_values: &[E]) -> E {
        E::ONE
    }
}

/// Defines a simple trait to recognize the possible types of clock cycles associated with auxiliary
/// column update hints.
pub trait HintCycle {
    /// Returns the cycle as a `usize` for indexing.
    fn as_index(&self) -> usize;
}

impl HintCycle for u32 {
    fn as_index(&self) -> usize {
        *self as usize
    }
}

impl HintCycle for u64 {
    fn as_index(&self) -> usize {
        *self as usize
    }
}

// TRACE LENGTH SUMMARY
// ================================================================================================

/// Contains the data about lengths of the trace parts.
///
/// - `main_trace_len` contains the length of the main trace.
/// - `range_trace_len` contains the length of the range checker trace.
/// - `chiplets_trace_len` contains the trace lengths of the all chiplets (hash, bitwise, memory,
/// kernel ROM)
#[derive(Debug, Default, Eq, PartialEq, Clone, Copy)]
pub struct TraceLenSummary {
    main_trace_len: usize,
    range_trace_len: usize,
    chiplets_trace_len: ChipletsLengths,
}

impl TraceLenSummary {
    pub fn new(
        main_trace_len: usize,
        range_trace_len: usize,
        chiplets_trace_len: ChipletsLengths,
    ) -> Self {
        TraceLenSummary {
            main_trace_len,
            range_trace_len,
            chiplets_trace_len,
        }
    }

    /// Returns length of the main trace
    pub fn main_trace_len(&self) -> usize {
        self.main_trace_len
    }

    /// Returns length of the range table
    pub fn range_trace_len(&self) -> usize {
        self.range_trace_len
    }

    /// Returns [ChipletsLengths] which contains trace lengths of all chilplets.
    pub fn chiplets_trace_len(&self) -> ChipletsLengths {
        self.chiplets_trace_len
    }

    /// Returns the maximum of all component lengths.
    pub fn trace_len(&self) -> usize {
        self.range_trace_len
            .max(self.main_trace_len)
            .max(self.chiplets_trace_len.trace_len())
    }

    /// Returns `trace_len` rounded up to the next power of two.
    pub fn padded_trace_len(&self) -> usize {
        (self.trace_len() + NUM_RAND_ROWS).next_power_of_two()
    }
}

/// Contains trace lengths of all chilplets: hash, bitwise, memory and kernel ROM trace
/// lengths.
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
pub struct ChipletsLengths {
    hash_chiplet_len: usize,
    bitwise_chiplet_len: usize,
    memory_chiplet_len: usize,
    kernel_rom_len: usize,
}

impl ChipletsLengths {
    pub fn new(chiplets: &Chiplets) -> Self {
        ChipletsLengths {
            hash_chiplet_len: chiplets.bitwise_start(),
            bitwise_chiplet_len: chiplets.memory_start() - chiplets.bitwise_start(),
            memory_chiplet_len: chiplets.kernel_rom_start() - chiplets.memory_start(),
            kernel_rom_len: chiplets.padding_start() - chiplets.kernel_rom_start(),
        }
    }

    pub fn from_parts(
        hash_len: usize,
        bitwise_len: usize,
        memory_len: usize,
        kernel_len: usize,
    ) -> Self {
        ChipletsLengths {
            hash_chiplet_len: hash_len,
            bitwise_chiplet_len: bitwise_len,
            memory_chiplet_len: memory_len,
            kernel_rom_len: kernel_len,
        }
    }

    /// Returns the length of the hash chiplet trace
    pub fn hash_chiplet_len(&self) -> usize {
        self.hash_chiplet_len
    }

    /// Returns the length of the bitwise trace
    pub fn bitwise_chiplet_len(&self) -> usize {
        self.bitwise_chiplet_len
    }

    /// Returns the length of the memory trace
    pub fn memory_chiplet_len(&self) -> usize {
        self.memory_chiplet_len
    }

    /// Returns the length of the kernel ROM trace
    pub fn kernel_rom_len(&self) -> usize {
        self.kernel_rom_len
    }

    /// Returns the length of the trace required to accommodate chiplet components and 1
    /// mandatory padding row required for ensuring sufficient trace length for auxiliary connector
    /// columns that rely on the memory chiplet.
    pub fn trace_len(&self) -> usize {
        self.hash_chiplet_len()
            + self.bitwise_chiplet_len()
            + self.memory_chiplet_len()
            + self.kernel_rom_len()
            + 1
    }
}

// TEST HELPERS
// ================================================================================================
#[cfg(test)]
use vm_core::{utils::ToElements, Operation};
#[cfg(test)]
pub fn build_span_with_respan_ops() -> (Vec<Operation>, Vec<Felt>) {
    let iv = [1, 3, 5, 7, 9, 11, 13, 15, 17].to_elements();
    let ops = vec![
        Operation::Push(iv[0]),
        Operation::Push(iv[1]),
        Operation::Push(iv[2]),
        Operation::Push(iv[3]),
        Operation::Push(iv[4]),
        Operation::Push(iv[5]),
        Operation::Push(iv[6]),
        // next batch
        Operation::Push(iv[7]),
        Operation::Push(iv[8]),
        Operation::Add,
        // drops to make sure stack overflow is empty on exit
        Operation::Drop,
        Operation::Drop,
        Operation::Drop,
        Operation::Drop,
        Operation::Drop,
        Operation::Drop,
        Operation::Drop,
        Operation::Drop,
    ];
    (ops, iv)
}
