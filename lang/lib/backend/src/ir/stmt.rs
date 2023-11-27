use super::{Variable, VariableId, Type, IntegerSize};

/// Wraps an [Instruction] and the [Variable] which it assigns to.
#[derive(Debug, Clone)]
pub struct Statement {
    pub result: Variable,
    pub instruction: Instruction,
}

/// An instruction which performs some computation and produces a result, which the enclosing
/// [Statement] will assign to a [Variable].
#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
}

/// All possible [Instruction] operations.
#[derive(Debug, Clone)]
pub enum InstructionKind {
    /// Evaluates to a constant value.
    Constant(ConstantValue),

    /// Adds together two integer values, of the same type.
    Add(VariableId, VariableId),

    /// Returns from the enclosing function, with a value if it is non-void.
    Return(Option<VariableId>),
}

impl Instruction {
    /// Whether this instruction is a terminator.
    /// 
    /// Every basic block must end with a terminator, and conversely terminators can only appear at
    /// the end of basic blocks.
    pub fn is_terminator(&self) -> bool {
        match self.kind {
            InstructionKind::Return(_) => true,

            _ => false,
        }
    }

    /// Returns the IDs of any variables which are read by this instruction. Used to implement
    /// liveness analysis.
    /// 
    /// Should not include the result, if the instruction is a statement - only variables which are
    /// read as part of the instruction itself.
    pub fn referenced_variables(&self) -> Vec<VariableId> {
        match self.kind {
            InstructionKind::Constant(_) => vec![],
            InstructionKind::Add(l, r) => vec![l, r],
            InstructionKind::Return(r) => r.into_iter().collect(),
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