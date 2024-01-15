use super::{EvaluationFrame, ExtensionOf, Felt, FieldElement};
use crate::trace::{
    chiplets::{
        memory::{MEMORY_READ_LABEL, MEMORY_WRITE_LABEL},
        MEMORY_ADDR_COL_IDX, MEMORY_CLK_COL_IDX, MEMORY_D0_COL_IDX, MEMORY_D1_COL_IDX,
        MEMORY_SELECTORS_COL_IDX, MEMORY_V_COL_RANGE,
    },
    decoder::{DECODER_OP_BITS_OFFSET, DECODER_USER_OP_HELPERS_OFFSET},
};
use crate::utils::binary_not;

pub mod chiplets;
pub mod range;
pub mod stack;

// ACCESSORS
// ================================================================================================
/// Trait to allow other processors to easily access the column values they need for constraint
/// calculations.
pub trait MainFrameExt<F, E>
where
    F: FieldElement<BaseField = Felt>,
    E: FieldElement<BaseField = Felt> + ExtensionOf<F>,
{
    /// Returns true when a u32 stack operation that requires range checks is being performed.
    fn u32_rc_op(&self) -> F;

    // --- Range check lookup accessors -----------------------------------------------------------------------

    /// The value required for the first memory lookup when the memory chiplet requests range
    /// checks. The value returned is the denominator used for including the value into the LogUp
    /// lookup: (alpha - d0). The value d0 which is being range-checked is the lower 16-bits of the
    /// delta value being tracked between two consecutive context IDs, addresses, or clock cycles in
    /// the current row.
    fn lookup_mv0(&self, alpha: E) -> E;
    /// The value required for the second memory lookup when the memory chiplet requests range
    /// checks. The value returned is the denominator used for including the value into the LogUp
    /// lookup: (alpha - d1). The value d1 which is being range-checked is the upper 16-bits of the
    /// delta value being tracked between two consecutive context IDs, addresses, or clock cycles in
    /// the current row.
    fn lookup_mv1(&self, alpha: E) -> E;
    /// The value required for the first stack lookup when the stack requests range checks. The
    /// value returned is the denominator used for including the value into the LogUp lookup:
    /// (alpha - h0). The value h0 which is being range checked by the stack operation is stored in
    /// the helper columns of the decoder section of the trace.
    fn lookup_sv0(&self, alpha: E) -> E;
    /// The value required for the second stack lookup when the stack requests range checks. The
    /// value returned is the denominator used for including the value into the LogUp lookup:
    /// (alpha - h1). The value h1 which is being range checked by the stack operation is stored in
    /// the helper columns of the decoder section of the trace.
    fn lookup_sv1(&self, alpha: E) -> E;
    /// The value required for the third stack lookup when the stack requests range checks. The
    /// value returned is the denominator used for including the value into the LogUp lookup:
    /// (alpha - h2). The value h2 which is being range checked by the stack operation is stored in
    /// the helper columns of the decoder section of the trace.
    fn lookup_sv2(&self, alpha: E) -> E;
    /// The value required for the fourth stack lookup when the stack requests range checks. The
    /// value returned is the denominator used for including the value into the LogUp lookup:
    /// (alpha - h3). The value h3 which is being range checked by the stack operation is stored in
    /// the helper columns of the decoder section of the trace.
    fn lookup_sv3(&self, alpha: E) -> E;

    fn lookup_pc(&self, alphas: &[E]) -> E;

    fn lookup_mem(&self, alphas: &[E]) -> E;

    fn mem_response(&self, alphas: &[E]) -> E;
}

impl<F, E> MainFrameExt<F, E> for EvaluationFrame<F>
where
    F: FieldElement<BaseField = Felt>,
    E: FieldElement<BaseField = Felt> + ExtensionOf<F>,
{
    /// Returns true when the stack operation is a u32 operation that requires range checks.
    /// TODO: this is also defined in the op flags. It's redefined here to avoid computing all of
    /// the op flags when this is the only one needed, but ideally this should only be defined once.
    #[inline(always)]
    fn u32_rc_op(&self) -> F {
        let not_4 = binary_not(self.current()[DECODER_OP_BITS_OFFSET + 4]);
        let not_5 = binary_not(self.current()[DECODER_OP_BITS_OFFSET + 5]);
        self.current()[DECODER_OP_BITS_OFFSET + 6].mul(not_5).mul(not_4)
    }

    // --- Intermediate values for LogUp lookups --------------------------------------------------

    #[inline(always)]
    fn lookup_mv0(&self, alpha: E) -> E {
        alpha - self.current()[MEMORY_D0_COL_IDX].into()
    }

    #[inline(always)]
    fn lookup_mv1(&self, alpha: E) -> E {
        alpha - self.current()[MEMORY_D1_COL_IDX].into()
    }

    #[inline(always)]
    fn lookup_sv0(&self, alpha: E) -> E {
        alpha - self.current()[DECODER_USER_OP_HELPERS_OFFSET].into()
    }

    #[inline(always)]
    fn lookup_sv1(&self, alpha: E) -> E {
        alpha - self.current()[DECODER_USER_OP_HELPERS_OFFSET + 1].into()
    }

    #[inline(always)]
    fn lookup_sv2(&self, alpha: E) -> E {
        alpha - self.current()[DECODER_USER_OP_HELPERS_OFFSET + 2].into()
    }

    #[inline(always)]
    fn lookup_sv3(&self, alpha: E) -> E {
        alpha - self.current()[DECODER_USER_OP_HELPERS_OFFSET + 3].into()
    }

    #[inline(always)]
    fn lookup_pc(&self, alphas: &[E]) -> E {
        let pc = self.current()[trace_defs::PC];
        let clk = self.current()[trace_defs::CYCLE];
        let word = [self.current()[trace_defs::PC_CONTENTS], F::ZERO, F::ZERO, F::ZERO];
        let is_write = self.current()[trace_defs::LOADING];
        let label = F::from(MEMORY_WRITE_LABEL) * is_write
            + F::from(MEMORY_READ_LABEL) * (F::ONE - is_write);
        let lookup = MemoryLookup::new(label, F::ZERO, pc, clk, word);
        lookup.to_value(alphas)
    }

    #[inline(always)]
    fn lookup_mem(&self, alphas: &[E]) -> E {
        todo!()
    }

    #[inline(always)]
    fn mem_response(&self, alphas: &[E]) -> E {
        let addr = self.current()[MEMORY_ADDR_COL_IDX];
        let clk = self.current()[MEMORY_CLK_COL_IDX];
        let ctx = F::ZERO;
        let word = self.current()[MEMORY_V_COL_RANGE.start];
        let word = [word, F::ZERO, F::ZERO, F::ZERO];
        let label = self.current()[MEMORY_SELECTORS_COL_IDX] * F::from(MEMORY_READ_LABEL)
            + F::from(MEMORY_WRITE_LABEL) * (F::ONE - self.current()[MEMORY_SELECTORS_COL_IDX]);
        let lookup = MemoryLookup::new(label, ctx, addr, clk, word);
        lookup.to_value(alphas)
    }
}

type Word<F> = [F; 4];

/// Contains the data required to describe a memory read or write.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MemoryLookup<E> {
    // unique label identifying the memory operation
    label: E,
    ctx: E,
    addr: E,
    clk: E,
    word: Word<E>,
}

impl<E> MemoryLookup<E> {
    pub fn new(label: E, ctx: E, addr: E, clk: E, word: Word<E>) -> Self {
        Self {
            label,
            ctx,
            addr,
            clk,
            word,
        }
    }
}

impl<F: FieldElement> MemoryLookup<F> {
    /// Reduces this row to a single field element in the field specified by E. This requires
    /// at least 9 alpha values.
    fn to_value<E: FieldElement + ExtensionOf<F>>(&self, alphas: &[E]) -> E {
        let word_value = self
            .word
            .iter()
            .enumerate()
            .fold(E::ZERO, |acc, (j, element)| acc + alphas[j + 5].mul_base(*element));

        alphas[0]
            + alphas[1].mul_base(self.label)
            + alphas[2].mul_base(self.ctx)
            + alphas[3].mul_base(self.addr)
            + alphas[4].mul_base(self.clk)
            + word_value
    }
}
