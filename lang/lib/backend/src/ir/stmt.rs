use super::{Variable, Instruction, VariableId};

/// Wraps a [StatementInstruction] and the [Variable] which it assigns to.
#[derive(Debug, Clone)]
pub struct Statement {
    pub result: Variable,
    pub instruction: StatementInstruction,
}

/// An instruction which performs some computation and produces a result, which the enclosing
/// [Statement] will assign to a [Variable].
#[derive(Debug, Clone)]
pub struct StatementInstruction {
    pub kind: StatementInstructionKind,
}

/// All possible [StatementInstruction] operations.
#[derive(Debug, Clone)]
pub enum StatementInstructionKind {
    // TODO
}

impl Instruction for StatementInstruction {
    fn referenced_variables(&self) -> Vec<VariableId> {
        todo!() // TODO
    }
}
