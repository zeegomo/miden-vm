use super::super::{
    EvaluationFrame, Felt, FieldElement, TransitionConstraintDegree, Vec, CHIPLETS_OFFSET,
};
use crate::utils::{are_equal, binary_not, is_binary};
use vm_core::ExtensionOf;

use winter_air::{Assertion, AuxTraceRandElements};

mod bitwise;
mod hasher;
mod memory;

// CONSTANTS
// ================================================================================================
/// The number of constraints on the management of the Chiplets module. This does not include
/// constraints for the individual chiplet components.
pub const NUM_CONSTRAINTS: usize = 0;
/// The degrees of constraints on the management of the Chiplets module. This does not include
/// constraint degrees for the individual chiplet components.
pub const CONSTRAINT_DEGREES: [usize; NUM_CONSTRAINTS] = [
    // 2, // Selector flags must be binary.
    // 2, // Selector flags can only change from 0 -> 1.
];

// PERIODIC COLUMNS
// ================================================================================================

/// Returns the set of periodic columns required by chiplets in the Chiplets module.
pub fn get_periodic_column_values() -> Vec<Vec<Felt>> {
    // let mut result = hasher::get_periodic_column_values();
    // result.append(&mut bitwise::get_periodic_column_values());
    // result
    Vec::new()
}

/// Returns the range checker's boundary assertions for the main trace at the first step.
pub fn get_aux_assertions_first_step<E: FieldElement>(result: &mut Vec<Assertion<E>>) {
    memory::get_aux_assertions_first_step(result);
}

/// Returns the range checker's boundary assertions for the main trace at the last step.
pub fn get_aux_assertions_last_step<E: FieldElement>(result: &mut Vec<Assertion<E>>, step: usize) {
    memory::get_aux_assertions_last_step(result, step);
}

// CHIPLETS TRANSITION CONSTRAINTS
// ================================================================================================

/// Builds the transition constraint degrees for the chiplets module and all chiplet components.
pub fn get_transition_constraint_degrees() -> Vec<TransitionConstraintDegree> {
    let mut degrees: Vec<TransitionConstraintDegree> = CONSTRAINT_DEGREES
        .iter()
        .map(|&degree| TransitionConstraintDegree::new(degree))
        .collect();

    // degrees.append(&mut hasher::get_transition_constraint_degrees());

    // degrees.append(&mut bitwise::get_transition_constraint_degrees());

    degrees.append(&mut memory::get_transition_constraint_degrees());

    degrees
}

/// Returns the number of transition constraints for the chiplets.
pub fn get_transition_constraint_count() -> usize {
    NUM_CONSTRAINTS
        // + hasher::get_transition_constraint_count()
        // + bitwise::get_transition_constraint_count()
        + memory::get_transition_constraint_count()
}

/// Enforces constraints for the chiplets module and all chiplet components.
pub fn enforce_constraints<E: FieldElement<BaseField = Felt>>(
    frame: &EvaluationFrame<E>,
    _periodic_values: &[E],
    result: &mut [E],
) {
    // chiplets transition constraints
    enforce_selectors(frame, result);
    let constraint_offset = NUM_CONSTRAINTS;

    // // hasher transition constraints
    // hasher::enforce_constraints(
    //     frame,
    //     &periodic_values[..hasher::NUM_PERIODIC_COLUMNS],
    //     &mut result[constraint_offset..],
    //     frame.hasher_flag(),
    // );
    // constraint_offset += hasher::get_transition_constraint_count();

    // // bitwise transition constraints
    // bitwise::enforce_constraints(
    //     frame,
    //     &periodic_values[hasher::NUM_PERIODIC_COLUMNS..],
    //     &mut result[constraint_offset..],
    //     frame.bitwise_flag(),
    // );
    // constraint_offset += bitwise::get_transition_constraint_count();

    // memory transition constraints
    memory::enforce_constraints(frame, &mut result[constraint_offset..], frame.memory_flag(false));
}

/// Returns the transition constraint degrees for the range checker's auxiliary columns, used for
/// multiset checks.
pub fn get_aux_transition_constraint_degrees() -> Vec<TransitionConstraintDegree> {
    memory::get_aux_transition_constraint_degrees()
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
    memory::enforce_aux_constraints::<F, E>(main_frame, aux_frame, aux_rand_elements, result)
}

// TRANSITION CONSTRAINT HELPERS
// ================================================================================================

/// Constraint evaluation function to enforce that the Chiplets module's selector columns must be
/// binary during the portion of the trace when they're being used as selectors.
fn enforce_selectors<E: FieldElement>(frame: &EvaluationFrame<E>, result: &mut [E]) {
    // --- Selector flags must be binary ----------------------------------------------------------

    // When selectors s0 and s1 are set, s2 is binary.
    result[0] = is_binary(frame.s(2));

    // --- Selector flags can only stay the same or change from 0 -> 1 ----------------------------

    // When selectors s0 and s1 are set, s2 must either be 0 in the current row or 1 in both rows.
    result[1] = frame.s(2) * are_equal(frame.s(2), frame.s_next(2));
}

// CHIPLETS FRAME EXTENSION TRAIT
// ================================================================================================

/// Trait to allow easy access to column values and intermediate variables used in constraint
/// calculations for the Chiplets module and its Hasher, Bitwise, and Memory chiplets.
trait EvaluationFrameExt<E: FieldElement> {
    // --- Column accessors -----------------------------------------------------------------------

    /// Returns the current value of the specified selector column. It assumes that the index is a
    /// valid selector index.
    fn s(&self, idx: usize) -> E;

    /// Returns the value of the specified selector column at the next row. It assumes that the
    /// index is a valid selector index.
    fn s_next(&self, idx: usize) -> E;

    // --- Chiplet selector flags -----------------------------------------------------------------

    /// Flag to indicate whether the frame is in the hasher portion of the Chiplets trace.
    fn hasher_flag(&self) -> E;

    /// Flag to indicate whether the frame is in the bitwise portion of the Chiplets trace.
    fn bitwise_flag(&self) -> E;

    /// Flag to indicate whether the frame is in the memory portion of the Chiplets trace.
    /// When `include_last_row` is true, the memory flag is true for every row where the memory
    /// selectors are set. When false, the last row is excluded. When this flag is used for
    /// transition constraints with `include_last_row = false`, they will not be applied to the
    /// final row of the memory trace.
    fn memory_flag(&self, include_last_row: bool) -> E;
}

impl<E: FieldElement> EvaluationFrameExt<E> for &EvaluationFrame<E> {
    // --- Column accessors -----------------------------------------------------------------------

    fn s(&self, idx: usize) -> E {
        self.current()[CHIPLETS_OFFSET + idx]
    }

    fn s_next(&self, idx: usize) -> E {
        self.next()[CHIPLETS_OFFSET + idx]
    }

    // --- Chiplet selector flags -----------------------------------------------------------------

    #[inline(always)]
    fn hasher_flag(&self) -> E {
        binary_not(self.s(0))
    }

    #[inline(always)]
    fn bitwise_flag(&self) -> E {
        self.s(0) * binary_not(self.s_next(1))
    }

    #[inline(always)]
    fn memory_flag(&self, include_last_row: bool) -> E {
        if include_last_row {
            self.s(0) * self.s(1) * binary_not(self.s(2))
        } else {
            self.s(0) * self.s(1) * binary_not(self.s_next(2))
        }
    }
}

// EXTERNAL ACCESSORS
// ================================================================================================
/// Trait to allow other processors to easily access the chiplet values they need for constraint
/// calculations.
pub trait ChipletsFrameExt<E: FieldElement> {
    /// Flag to indicate whether the frame is in the memory chiplet.
    fn chiplets_memory_flag(&self) -> E;
}

impl<E: FieldElement> ChipletsFrameExt<E> for &EvaluationFrame<E> {
    #[inline(always)]
    fn chiplets_memory_flag(&self) -> E {
        self.memory_flag(true)
    }
}
