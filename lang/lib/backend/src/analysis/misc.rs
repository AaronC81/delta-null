use crate::ir::{Function, InstructionKind};

/// Determines whether the given function is a *leaf* function, meaning that it will never call
/// any other functions.
/// 
/// This can be used to facilitate optimisations when generating assembly code, such as not needing
/// to preserve the return pointer register.
/// 
/// Note that this may return `false` even for functions which are a leaf function _in practice_,
/// for example functions where a call is present but unreachable.
pub fn is_leaf_function(func: &Function) -> bool {
    !func.statements().any(|s| matches!(s.instruction.kind, InstructionKind::Call { .. }))
}
