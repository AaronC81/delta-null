use super::{Variable, Instruction, VariableId, Type, IntegerSize};

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
    /// Evaluates to a constant value.
    Constant(ConstantValue),

    /// Adds together two integer values, of the same type.
    Add(VariableId, VariableId),
}

impl Instruction for StatementInstruction {
    fn referenced_variables(&self) -> Vec<VariableId> {
        match self.kind {
            StatementInstructionKind::Constant(_) => vec![],
            StatementInstructionKind::Add(l, r) => vec![l, r],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstantValue {
    U16(u16),
    I16(i16),
}

impl ConstantValue {
    pub fn ty(self) -> Type {
        match self {
            ConstantValue::U16(_) => Type::UnsignedInteger(IntegerSize::Bits16),
            ConstantValue::I16(_) => Type::SignedInteger(IntegerSize::Bits16),
        }
    }
}
