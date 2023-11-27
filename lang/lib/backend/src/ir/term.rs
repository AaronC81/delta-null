use super::{Instruction, VariableId};

/// An instruction which terminates and diverges from a [BasicBlock], producing no result.
#[derive(Debug, Clone)]
pub struct TerminatorInstruction {
    pub kind: TerminatorInstructionKind,
}

/// All possible [StatementInstruction] operations.
#[derive(Debug, Clone)]
pub enum TerminatorInstructionKind {
    /// Returns from the enclosing function, with a value if it is non-void.
    Return(Option<VariableId>),
}

impl Instruction for TerminatorInstruction {
    fn referenced_variables(&self) -> Vec<VariableId> {
        match self.kind {
            TerminatorInstructionKind::Return(v) => v.into_iter().collect(),
        }
    }
}
