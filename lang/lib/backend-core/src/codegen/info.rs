//! Externally-visible information about codegen characteristics, which is required for
//! cooperation with other compilation stages.

use delta_null_lang_backend::ir::{InstructionKind, VariableId};

/// Determines whether the given [InstructionKind] supports *in-place* usage - that is, whether the
/// output variable is permitted to be allocated to the same register as one of the instruction's
/// parameters, iff the parameter register is not live-out of this instruction. If so, returns the
/// variables whose allocations may be used for this purpose. If not, returns [None].
/// 
/// This isn't possible for some IR instructions which compile into a sequence of non-trivial
/// processor instructions. But for easy instructions like [InstructionKind::Add], not only is it
/// supported, but it may permit generating more optimised code.
/// 
/// For example:
/// 
/// ```ignore
/// $0 = read <local 0>
/// $1 = 5
/// $2 = $0 + $1
/// return $2
/// ```
/// 
/// `$2` does not need a brand new register, and the processor's `add` instruction is two-operand.
/// So by recycling the register allocated for `$0` or `$1`, we get better codegen:
/// 
/// ```ignore
/// read r0, ... ; $0 allocated to r0
/// .put r1, 5   ; $1 allocated to r1
/// add r0, r1   ; $2 also allocated to r0, so can mutate it without copy
/// ret
/// ```
/// 
/// Usually codegen requires a special case for in-place usage - this indicates whether such a
/// special case is implemented.
/// 
/// If an [InstructionKind] supports in-place usage, the register allocator will prefer to do this
/// if possible. (Read: supporting in-place usage for *worse* codegen is not worth doing!)
pub fn in_place_usage_variables(kind: &InstructionKind) -> Option<Vec<VariableId>> {
    // TODO: expand
    match kind {
        InstructionKind::Add(a, b) => Some(vec![*a, *b]),

        _ => None,
    }
}
