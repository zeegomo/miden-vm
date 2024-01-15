use super::{EvaluationFrame, FieldElement, Vec};
use crate::constraints::MainFrameExt;
use crate::trace::chiplets::{
    MEMORY_ADDR_COL_IDX, MEMORY_BUS_COL_IDX, MEMORY_CLK_COL_IDX, MEMORY_CTX_COL_IDX,
    MEMORY_D0_COL_IDX, MEMORY_D1_COL_IDX, MEMORY_D_INV_COL_IDX, MEMORY_TRACE_OFFSET,
    MEMORY_V_COL_RANGE,
};
use crate::utils::{are_equal, binary_not, is_binary, EvaluationResult};
use crate::Felt;
use vm_core::ExtensionOf;
use winter_air::{Assertion, AuxTraceRandElements, TransitionConstraintDegree};

#[cfg(test)]
mod tests;

// CONSTANTS
// ================================================================================================

/// The number of constraints on the management of the memory chiplet.
pub const NUM_CONSTRAINTS: usize = 9;
/// The degrees of constraints on the management of the memory chiplet. All constraint degrees are
/// increased by 3 due to the selectors for the memory chiplet.
pub const CONSTRAINT_DEGREES: [usize; NUM_CONSTRAINTS] = [
    3, 3, // Enforce that the memory selectors are binary.
    5, 4, // Enforce s1 is set to 1 when reading existing memory and 0 otherwise.
    5, 4, // Constrain the values in the d inverse column.
    4, // Enforce values in ctx, addr, clk transition correctly.
    4, // Enforce correct memory initialization when reading from new memory.
    3, // Enforce correct memory copy when reading from existing memory
];

pub const NUM_AUX_CONSTRAINTS: usize = 1;

pub const AUX_CONSTRAINT_DEGREES: [usize; NUM_AUX_CONSTRAINTS] = [4];

// MEMORY TRANSITION CONSTRAINTS
// ================================================================================================

/// Builds the transition constraint degrees for the memory chiplet.
pub fn get_transition_constraint_degrees() -> Vec<TransitionConstraintDegree> {
    CONSTRAINT_DEGREES
        .iter()
        .map(|&degree| TransitionConstraintDegree::new(degree))
        .collect()
}

/// Returns the number of transition constraints for the memory chiplet.
pub fn get_transition_constraint_count() -> usize {
    NUM_CONSTRAINTS
}

/// Returns the range checker's boundary assertions for the main trace at the first step.
pub fn get_aux_assertions_first_step<E: FieldElement>(result: &mut Vec<Assertion<E>>) {
    let step = 0;
    result.push(Assertion::single(MEMORY_BUS_COL_IDX, step, E::ONE));
}

/// Returns the range checker's boundary assertions for the main trace at the last step.
pub fn get_aux_assertions_last_step<E: FieldElement>(result: &mut Vec<Assertion<E>>, step: usize) {
    result.push(Assertion::single(MEMORY_BUS_COL_IDX, step, E::ONE));
}

/// Enforces constraints for the memory chiplet.
pub fn enforce_constraints<E: FieldElement>(
    frame: &EvaluationFrame<E>,
    result: &mut [E],
    memory_flag: E,
) {
    // Constrain the operation selectors.
    let mut index = enforce_selectors(frame, result, memory_flag);

    // Constrain the values in the d inverse column.
    index += enforce_d_inv(frame, &mut result[index..], memory_flag);

    // Enforce values in ctx, addr, clk transition correctly.
    index += enforce_delta(frame, &mut result[index..], memory_flag);

    // Constrain the memory values.
    enforce_values(frame, &mut result[index..], memory_flag);
}

// --- AUXILIARY COLUMNS (FOR MULTISET CHECKS) ----------------------------------------------------

/// Returns the transition constraint degrees for the range checker's auxiliary columns, used for
/// multiset checks.
pub fn get_aux_transition_constraint_degrees() -> Vec<TransitionConstraintDegree> {
    AUX_CONSTRAINT_DEGREES
        .iter()
        .map(|&degree| TransitionConstraintDegree::new(degree))
        .collect()
}

/// Enforces constraints on the range checker's auxiliary columns.
pub fn enforce_aux_constraints<F, E>(
    main_frame: &EvaluationFrame<F>,
    aux_frame: &EvaluationFrame<E>,
    aux_rand_elements: &AuxTraceRandElements<E>,
    result: &mut [E],
) where
    F: FieldElement<BaseField = Felt>,
    E: FieldElement<BaseField = Felt> + ExtensionOf<F>,
{
    // Get the first random element for this segment.
    let alphas = aux_rand_elements.get_segment_elements(0);

    // Enforce b_range.
    enforce_b_memory(main_frame, aux_frame, alphas, result);
}

fn enforce_b_memory<E, F>(
    main_frame: &EvaluationFrame<F>,
    aux_frame: &EvaluationFrame<E>,
    alphas: &[E],
    result: &mut [E],
) where
    F: FieldElement<BaseField = Felt>,
    E: FieldElement<BaseField = Felt> + ExtensionOf<F>,
{
    let pc_lookup = main_frame.lookup_pc(alphas);
    let is_pc_lookup = main_frame.current()[201];

    // assert_ne!(is_pc_lookup, F::ZERO);
    // assert_ne!(is_pc_lookup, F::ONE);

    let pc_lookup_2 = E::from(is_pc_lookup) * pc_lookup + (E::ONE - is_pc_lookup.into());
    // let mem_loop = main_frame.lookup_mem(alphas);

    let mem_response = main_frame.mem_response(alphas);
    let is_mem_response = is_pc_lookup;
    let mem_response_2 =
        E::from(is_mem_response) * mem_response + (E::ONE - is_mem_response.into());

    result[0] = are_equal(
        aux_frame.next()[MEMORY_BUS_COL_IDX] * pc_lookup_2,
        aux_frame.current()[MEMORY_BUS_COL_IDX] * mem_response_2,
    ) * is_pc_lookup.into();
}

// TRANSITION CONSTRAINT HELPERS
// ================================================================================================

fn enforce_selectors<E: FieldElement>(
    frame: &EvaluationFrame<E>,
    result: &mut [E],
    memory_flag: E,
) -> usize {
    let mut index = 0;

    // s0 and s1 are binary.
    result[index] = memory_flag * is_binary(frame.selector(0));
    index += 1;
    result[index] = memory_flag * is_binary(frame.selector(1));
    index += 1;
    assert_eq!(frame.n0(), E::ZERO);
    // s1 is set to 1 when existing memory is being read. this happens when ctx and addr haven't
    // changed, and the next operation is a read (s0 is set).
    result[index] = memory_flag
        * frame.reaccess_flag()
        * frame.selector_next(0)
        * binary_not(frame.selector_next(1));
    index += 1;

    // s1 is set to 0 in all other cases. this happens when ctx changed, or ctx stayed the same but
    // addr changed, or the operation was a write.
    result[index] = memory_flag
        * (frame.n0() + frame.not_n0() * frame.n1() + binary_not(frame.selector_next(0)))
        * frame.selector_next(1);
    index += 1;

    index
}

/// A constraint evaluation function to enforce that the `d_inv` "delta inverse" column used to
/// constrain the delta between two consecutive contexts, addresses, or clock cycles is updated
/// correctly.
fn enforce_d_inv<E: FieldElement>(
    frame: &EvaluationFrame<E>,
    result: &mut [E],
    memory_flag: E,
) -> usize {
    let constraint_count = 2;

    // result.agg_constraint(0, memory_flag, is_binary(frame.n0()));
    // result.agg_constraint(1, memory_flag * frame.not_n0(), frame.ctx_change());
    result.agg_constraint(0, memory_flag, is_binary(frame.n1()));
    result.agg_constraint(1, memory_flag * frame.reaccess_flag(), frame.addr_change());

    constraint_count
}

/// A constraint evaluation function to enforce that the delta between two consecutive context IDs,
/// addresses, or clock cycles is updated and decomposed into the `d1` and `d0` columns correctly.
fn enforce_delta<E: FieldElement>(
    frame: &EvaluationFrame<E>,
    result: &mut [E],
    memory_flag: E,
) -> usize {
    let constraint_count = 1;

    // If the context changed, include the difference.
    // result.agg_constraint(0, memory_flag * frame.n0(), frame.ctx_change());
    // If the context is the same, include the address difference if it changed or else include the
    // clock change.
    result.agg_constraint(
        0,
        memory_flag,
        frame.n1() * frame.addr_change() + frame.not_n1() * frame.clk_change(),
    );
    // Always subtract the delta. It should offset the other changes.
    result[0] -= memory_flag * frame.delta_next();

    constraint_count
}

/// A constraint evaluation function to enforce that memory is initialized to zero when it is read
/// before being written and that when existing memory values are read they remain unchanged.
fn enforce_values<E: FieldElement>(
    frame: &EvaluationFrame<E>,
    result: &mut [E],
    memory_flag: E,
) -> usize {
    let mut index = 0;

    // initialize memory to zero when reading from new context and address pair.
    for i in 0..1 {
        result[index] = memory_flag * frame.init_read_flag() * frame.v(i);
        index += 1;
    }

    // copy previous values when reading memory that was previously accessed.
    for i in 0..1 {
        result[index] = memory_flag * frame.copy_read_flag() * (frame.v_next(i) - frame.v(i));
        index += 1;
    }

    index
}

// MEMORY FRAME EXTENSION TRAIT
// ================================================================================================

/// Trait to allow easy access to column values and intermediate variables used in constraint
/// calculations for the Memory chiplet.
trait EvaluationFrameExt<E: FieldElement> {
    // --- Column accessors -----------------------------------------------------------------------

    /// Gets the value of the specified selector column in the current row.
    fn selector(&self, idx: usize) -> E;
    /// Gets the value of the specified selector column in the next row.
    fn selector_next(&self, idx: usize) -> E;
    /// The current context value.
    fn ctx(&self) -> E;
    /// The current address.
    fn addr(&self) -> E;
    /// The current clock cycle.
    fn clk(&self) -> E;
    /// The next clock cycle.
    fn clk_next(&self) -> E;
    /// The value from the specified index of the values (0, 1, 2, 3) in the current row.
    fn v(&self, index: usize) -> E;
    /// The value from the specified index of the values (0, 1, 2, 3) in the next row.
    fn v_next(&self, index: usize) -> E;
    /// The next value of the lower 16-bits of the delta value being tracked between two consecutive
    /// context IDs, addresses, or clock cycles.
    fn d0_next(&self) -> E;
    /// The next value of the upper 16-bits of the delta value being tracked between two consecutive
    /// context IDs, addresses, or clock cycles.
    fn d1_next(&self) -> E;
    /// The next value of the column tracking the inverse delta used for constraint evaluations.
    fn d_inv_next(&self) -> E;

    // --- Intermediate variables & helpers -------------------------------------------------------

    /// The change between the current value in the specified column and the next value, calculated
    /// as `next - current`.
    fn change(&self, column: usize) -> E;
    /// An intermediate variable to help constrain context change updates in the delta inverse
    /// column.
    fn n0(&self) -> E;
    /// `1 - n0`
    fn not_n0(&self) -> E;
    /// An intermediate variable to help constrain address changes in the delta inverse column when
    /// the context doesn't change.
    fn n1(&self) -> E;
    /// `1 - n1`
    fn not_n1(&self) -> E;
    /// The difference between the next context and the current context.
    fn ctx_change(&self) -> E;
    /// The difference between the next address and the current address.
    fn addr_change(&self) -> E;
    /// The difference between the next clock value and the current one, minus 1.
    fn clk_change(&self) -> E;
    /// The delta between two consecutive context IDs, addresses, or clock cycles.
    fn delta_next(&self) -> E;

    // --- Flags ----------------------------------------------------------------------------------

    /// A flag to indicate that previously assigned memory is being accessed. In other words, the
    /// context and address have not changed.
    fn reaccess_flag(&self) -> E;

    /// A flag to indicate that there is a read in the current row which requires the values to be
    /// initialized to zero.
    fn init_read_flag(&self) -> E;

    /// A flag to indicate that the operation in the next row is a read which requires copying the
    /// values from the current row to the next row.
    fn copy_read_flag(&self) -> E;
}

impl<E: FieldElement> EvaluationFrameExt<E> for &EvaluationFrame<E> {
    // --- Column accessors -----------------------------------------------------------------------

    #[inline(always)]
    fn selector(&self, idx: usize) -> E {
        self.current()[MEMORY_TRACE_OFFSET + idx]
    }

    #[inline(always)]
    fn selector_next(&self, idx: usize) -> E {
        self.next()[MEMORY_TRACE_OFFSET + idx]
    }

    #[inline(always)]
    fn ctx(&self) -> E {
        self.current()[MEMORY_CTX_COL_IDX]
    }

    #[inline(always)]
    fn addr(&self) -> E {
        self.next()[MEMORY_ADDR_COL_IDX]
    }

    #[inline(always)]
    fn clk(&self) -> E {
        self.current()[MEMORY_CLK_COL_IDX]
    }

    #[inline(always)]
    fn clk_next(&self) -> E {
        self.next()[MEMORY_CLK_COL_IDX]
    }

    #[inline(always)]
    fn v(&self, index: usize) -> E {
        self.current()[MEMORY_V_COL_RANGE.start + index]
    }

    #[inline(always)]
    fn v_next(&self, index: usize) -> E {
        self.next()[MEMORY_V_COL_RANGE.start + index]
    }

    #[inline(always)]
    fn d0_next(&self) -> E {
        self.next()[MEMORY_D0_COL_IDX]
    }

    #[inline(always)]
    fn d1_next(&self) -> E {
        self.next()[MEMORY_D1_COL_IDX]
    }

    #[inline(always)]
    fn d_inv_next(&self) -> E {
        self.next()[MEMORY_D_INV_COL_IDX]
    }

    // --- Intermediate variables & helpers -------------------------------------------------------

    #[inline(always)]
    fn change(&self, column: usize) -> E {
        self.next()[column] - self.current()[column]
    }

    #[inline(always)]
    fn n0(&self) -> E {
        self.change(MEMORY_CTX_COL_IDX) * self.d_inv_next()
    }

    #[inline(always)]
    fn not_n0(&self) -> E {
        binary_not(self.n0())
    }

    #[inline(always)]
    fn n1(&self) -> E {
        self.change(MEMORY_ADDR_COL_IDX) * self.d_inv_next()
    }

    #[inline(always)]
    fn not_n1(&self) -> E {
        binary_not(self.n1())
    }

    #[inline(always)]
    fn ctx_change(&self) -> E {
        self.change(MEMORY_CTX_COL_IDX)
    }

    #[inline(always)]
    fn addr_change(&self) -> E {
        self.change(MEMORY_ADDR_COL_IDX)
    }

    #[inline(always)]
    fn clk_change(&self) -> E {
        self.change(MEMORY_CLK_COL_IDX) - E::ONE
    }

    #[inline(always)]
    fn delta_next(&self) -> E {
        E::from(2_u32.pow(16)) * self.d1_next() + self.d0_next()
    }

    // --- Flags ----------------------------------------------------------------------------------

    #[inline(always)]
    fn reaccess_flag(&self) -> E {
        self.not_n0() * self.not_n1()
    }

    #[inline(always)]
    fn init_read_flag(&self) -> E {
        self.selector(0) * binary_not(self.selector(1))
    }

    #[inline(always)]
    fn copy_read_flag(&self) -> E {
        self.selector_next(1)
    }
}
