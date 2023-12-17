use std::{fmt::Display, error::Error, ops::Deref, collections::HashSet};

use maplit::hashset;

use super::{VariableId, Type, IntegerSize, BasicBlockId, VariableRepository, StatementId, PrintIR, LocalId, LocalRepository};

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
            // Check if we're given any additional info, and if so, if it contains this variable
            if let Some(info) = options.additional_variable_info.as_ref().and_then(|m| m.get(&result)) {
                format!("{} ({}) = {}", result.print_ir(options), info, self.instruction.print_ir(options))
            } else {
                format!("{} = {}", result.print_ir(options), self.instruction.print_ir(options))
            }
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

    /// Reads the current value of a local.
    ReadLocal(LocalId),

    /// Writes a new value to a local.
    WriteLocal(LocalId, VariableId),

    /// Adds together two integer values, of the same type.
    Add(VariableId, VariableId),

    /// Subtracts one integer value from another, of the same type.
    Subtract(VariableId, VariableId),

    /// Multiply two integer values with each other, of the same type.
    Multiply(VariableId, VariableId),

    /// Checks if two variables of the same type are equal.
    Equals(VariableId, VariableId),

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

    /// Chooses between multiple different variables, based on the block which branched to the
    /// current block.
    /// 
    /// Must appear as the first instruction of a block. Results in undefined behaviour if the block
    /// which branched to this block is not covered.
    Phi {
        choices: Vec<(BasicBlockId, VariableId)>,
    },

    /// A terminator used to mark a point which should never be reached.
    /// 
    /// Undefined behaviour occurs if this instruction is executed in a compiled program.
    Unreachable,
}

impl Instruction {
    /// Whether this instruction is a terminator.
    /// 
    /// Every basic block must end with a terminator, and conversely terminators can only appear at
    /// the end of basic blocks.
    pub fn is_terminator(&self) -> bool {
        matches!(self.kind, 
              InstructionKind::Return(_)
            | InstructionKind::Branch(_)
            | InstructionKind::ConditionalBranch { .. }
            | InstructionKind::Unreachable
        )
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
        match &self.kind {
            InstructionKind::Constant(_) => hashset!{},
            InstructionKind::ReadLocal(_) => hashset!{},
            InstructionKind::WriteLocal(_, v) => hashset!{ *v },
            InstructionKind::Add(l, r)
            | InstructionKind::Subtract(l, r)
            | InstructionKind::Multiply(l, r)
            | InstructionKind::Equals(l, r) => hashset!{ *l, *r },
            InstructionKind::Return(r) => r.iter().copied().collect(),
            InstructionKind::Branch(_) => hashset!{},
            InstructionKind::ConditionalBranch { condition, .. } => hashset!{ *condition },
            InstructionKind::Phi { choices } => choices.iter().map(|(_, var)| *var).collect(),
            InstructionKind::Unreachable => hashset!{},
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
    pub fn result_type(
        &self,
        vars: impl Deref<Target = impl VariableRepository>,
        locals: impl Deref<Target = impl LocalRepository>,
    ) -> Result<Option<Type>, TypeError> {
        match &self.kind {
            InstructionKind::Constant(v) => Ok(Some(v.ty())),

            InstructionKind::ReadLocal(l) => Ok(Some(locals.get_local(*l).ty)),
            InstructionKind::WriteLocal(_, _) => Ok(None),

            InstructionKind::Add(a, b)
            | InstructionKind::Subtract(a, b)
            | InstructionKind::Multiply(a, b) => {
                let a_ty = vars.get_variable(*a).ty;
                let b_ty = vars.get_variable(*b).ty;
                if a_ty != b_ty {
                    return Err(TypeError::new("both sides of `Add` must have the same type"));
                }
                
                Ok(Some(a_ty))
            },

            InstructionKind::Equals(_, _) => Ok(Some(Type::Boolean)),

            InstructionKind::Return(_)
            | InstructionKind::Branch(_)
            | InstructionKind::ConditionalBranch { .. } => Ok(None),

            InstructionKind::Phi { choices } => {
                let choice_tys = choices.iter()
                    .map(|(_, var)| vars.get_variable(*var).ty)
                    .collect::<Vec<_>>();

                // Check all types are the same
                let Some(first_ty) = choice_tys.first() else {
                    return Err(TypeError::new("`Phi` must have at least one choice"));
                };
                for other_ty in choice_tys.iter().skip(1) {
                    if other_ty != first_ty {
                        return Err(TypeError::new("mismatching types in `Phi`"));
                    }
                }

                Ok(Some(*first_ty))
            }

            InstructionKind::Unreachable => Ok(None),
        }
    }
}

impl PrintIR for Instruction {
    fn print_ir(&self, options: &super::PrintOptions) -> String {
        match &self.kind {
            InstructionKind::Constant(c) => c.print_ir(options),
            InstructionKind::ReadLocal(l) => format!("read {}", l.print_ir(options)),
            InstructionKind::WriteLocal(l, v) => format!("write {} = {}", l.print_ir(options), v.print_ir(options)),
            InstructionKind::Add(a, b) => format!("{} + {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::Subtract(a, b) => format!("{} - {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::Multiply(a, b) => format!("{} * {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::Equals(a, b) => format!("{} == {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::Return(r) =>
                if let Some(r) = r {
                    format!("return {}", r.print_ir(options))
                } else {
                    "return".to_string()
                },
            InstructionKind::Branch(b) => format!("branch {}", b.print_ir(options)),
            InstructionKind::ConditionalBranch { condition, true_block, false_block } =>
                format!("condbranch {} ? {} : {}", condition.print_ir(options), true_block.print_ir(options), false_block.print_ir(options)),
            InstructionKind::Phi { choices } =>
                format!(
                    "phi {}",
                    choices.iter()
                        .map(|(b, v)| format!("{} -> {}", b.print_ir(options), v.print_ir(options)))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            InstructionKind::Unreachable => "unreachable".to_owned(),
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
    fn print_ir(&self, _options: &super::PrintOptions) -> String {
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
