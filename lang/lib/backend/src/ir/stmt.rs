use std::{fmt::Display, error::Error, ops::Deref, collections::HashSet};

use maplit::hashset;

use super::{VariableId, Type, IntegerSize, BasicBlockId, VariableRepository, StatementId, PrintIR};

/// Wraps an [Instruction], and the [VariableId] which it assigns to, if any.
#[derive(Debug, Clone)]
pub struct Statement {
    pub id: StatementId,
    pub result: Option<VariableId>,
    pub instruction: Instruction,
}

impl PrintIR for Statement {
    fn print_ir(&self, options: &super::PrintOptions) -> String {
        if let Some(result) = self.result {
            format!("{} = {}", result.print_ir(options), self.instruction.print_ir(options))
        } else {
            self.instruction.print_ir(options)
        }
    }
}

/// An instruction which performs some computation and produces a result, which the enclosing
/// [Statement] will assign to a [Variable].
#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
}

impl Instruction {
    pub fn new(kind: InstructionKind) -> Self {
        Self { kind }
    }
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

    /// Branches unconditionally to another basic block.
    Branch(BasicBlockId),

    /// Branches to one basic block if the given boolean [VariableId] is true, or another if it is
    /// false.
    ConditionalBranch {
        condition: VariableId,
        true_block: BasicBlockId,
        false_block: BasicBlockId,
    },
}

impl Instruction {
    /// Whether this instruction is a terminator.
    /// 
    /// Every basic block must end with a terminator, and conversely terminators can only appear at
    /// the end of basic blocks.
    pub fn is_terminator(&self) -> bool {
        match self.kind {
            InstructionKind::Return(_)
            | InstructionKind::Branch(_)
            | InstructionKind::ConditionalBranch { .. }
                => true,

            _ => false,
        }
    }

    /// Returns the IDs of any basic blocks which this instruction could branch to.
    /// 
    /// Only ever returns items for terminators - non-terminators always return no items.
    pub fn branch_destinations(&self) -> Vec<BasicBlockId> {
        match self.kind {
            InstructionKind::Branch(b) => vec![b],
            InstructionKind::ConditionalBranch { true_block, false_block, .. } => vec![true_block, false_block],
            
            _ => vec![],
        }
    }

    /// Returns the IDs of any variables which are read by this instruction. Used to implement
    /// liveness analysis.
    /// 
    /// Should not include the result, if the instruction is a statement - only variables which are
    /// read as part of the instruction itself.
    pub fn referenced_variables(&self) -> HashSet<VariableId> {
        match self.kind {
            InstructionKind::Constant(_) => hashset!{},
            InstructionKind::Add(l, r) => hashset!{ l, r },
            InstructionKind::Return(r) => r.into_iter().collect(),
            InstructionKind::Branch(_) => hashset!{},
            InstructionKind::ConditionalBranch { condition, .. } => hashset!{ condition },
        }
    }

    /// Determines the type of the resulting variable for this instruction.
    /// 
    /// This type may be dependent on the types of other declared variables, so this takes a
    /// reference to a [VariableRepository] so that it can look up types.
    /// 
    /// - If the result type can be deduced to a specific type, returns `Ok(Some(ty))`.
    /// - If the result type was deduced as void (typically for a terminator), returns `Ok(None)`.
    /// - If the result type could not be deduced due to an invalid instruction, returns a
    ///   [TypeError].
    pub fn result_type(&self, vars: impl Deref<Target = impl VariableRepository>) -> Result<Option<Type>, TypeError> {
        match self.kind {
            InstructionKind::Constant(v) => Ok(Some(v.ty())),
            InstructionKind::Add(a, b) => {
                let a_ty = vars.get_variable(a).ty;
                let b_ty = vars.get_variable(b).ty;
                if a_ty != b_ty {
                    return Err(TypeError::new("both sides of `Add` must have the same type"));
                }
                
                Ok(Some(a_ty))
            },

            InstructionKind::Return(_)
            | InstructionKind::Branch(_)
            | InstructionKind::ConditionalBranch { .. } => Ok(None),
        }
    }
}

impl PrintIR for Instruction {
    fn print_ir(&self, options: &super::PrintOptions) -> String {
        match self.kind {
            InstructionKind::Constant(c) => c.print_ir(options),
            InstructionKind::Add(a, b) => format!("{} + {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::Return(r) =>
                if let Some(r) = r {
                    format!("return {}", r.print_ir(options))
                } else {
                    "return".to_string()
                },
            InstructionKind::Branch(b) => format!("branch {}", b.print_ir(options)),
            InstructionKind::ConditionalBranch { condition, true_block, false_block } =>
                format!("condbranch {} ? {} : {}", condition.print_ir(options), true_block.print_ir(options), false_block.print_ir(options))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstantValue {
    U16(u16),
    I16(i16),
    Boolean(bool),
}

impl ConstantValue {
    pub fn ty(self) -> Type {
        match self {
            ConstantValue::U16(_) => Type::UnsignedInteger(IntegerSize::Bits16),
            ConstantValue::I16(_) => Type::SignedInteger(IntegerSize::Bits16),
            ConstantValue::Boolean(_) => Type::Boolean,
        }
    }
}

impl PrintIR for ConstantValue {
    fn print_ir(&self, options: &super::PrintOptions) -> String {
        match self {
            ConstantValue::U16(u) => u.to_string(),
            ConstantValue::I16(v) => v.to_string(),
            ConstantValue::Boolean(b) => b.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeError(String);
impl TypeError {
    pub fn new(s: &str) -> Self {
        Self(s.to_string())
    }
}
impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Error for TypeError {}
