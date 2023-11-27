use super::{Instruction, VariableId};

/// An instruction which terminates and diverges from a [BasicBlock], producing no result.
#[derive(Debug, Clone)]
pub struct TerminatorInstruction {
    pub kind: TerminatorInstructionKind,
}

/// All possible [StatementInstruction] operations.
#[derive(Debug, Clone)]
pub enum TerminatorInstructionKind {
    // TODO
}

impl Instruction for TerminatorInstruction {
    fn referenced_variables(&self) -> Vec<VariableId> {
        todo!() // TODO
    }
}
