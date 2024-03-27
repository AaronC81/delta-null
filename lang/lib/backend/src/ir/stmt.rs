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

    /// Converts a value from one type to another type of the same size, by simply reinterpreting
    /// the bits without any extra processing.
    CastReinterpret {
        value: VariableId,
        ty: Type,
    },

    /// Acquires a [Type::FunctionReference], given the name of a function, and the type of the
    /// expected reference.
    FunctionReference {
        name: String,
        ty: Type,
    },

    /// Reads the current value of a local.
    ReadLocal(LocalId),

    /// Writes a new value to a local.
    WriteLocal(LocalId, VariableId),

    /// Retrieve the memory address of a local.
    AddressOfLocal(LocalId),

    /// Write a value to a memory address.
    WriteMemory {
        address: VariableId,
        value: VariableId,
    },

    /// Read a value of the given type from a memory address.
    ReadMemory {
        address: VariableId,
        ty: Type,
    },

    /// Adds together two integer values, of the same type.
    Add(VariableId, VariableId),

    /// Subtracts one integer value from another, of the same type.
    Subtract(VariableId, VariableId),

    /// Multiply two integer values with each other, of the same type.
    Multiply(VariableId, VariableId),

    /// Performs bitwise AND between two integer values, of the same type.
    BitwiseAnd(VariableId, VariableId),

    /// Performs bitwise XOR between two integer values, of the same type.
    BitwiseXor(VariableId, VariableId),

    /// Performs bitwise OR between two integer values, of the same type.
    BitwiseOr(VariableId, VariableId),

    /// Performs bitwise NOT on an integer value.
    BitwiseNot(VariableId),

    /// Checks if two variables of the same type are equal.
    Equals(VariableId, VariableId),

    /// Checks if the left variable is greater than the right variable of the same type.
    GreaterThan(VariableId, VariableId),

    /// Checks if the left variable is less than the right variable of the same type.
    LessThan(VariableId, VariableId),

    /// Calls a different function, using the given [VariableId], which should hold a
    /// [Type::FunctionReference].
    Call {
        target: VariableId,
        arguments: Vec<VariableId>,
    },

    /// Evaluates to the size, in machine words, of the given [Type].
    WordSize(Type),

    /// Given the [Type] of a structure, and an index into its list of fields, gets the word offset
    /// of that field from the beginning of the structure.
    FieldOffset {
        ty: Type,
        index: usize,
    },

    /// Include some target-specific inline assembly at this point in the compiled code.
    InlineAssembly(String),

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
            InstructionKind::CastReinterpret { value, ty: _ } => hashset!{ *value },
            InstructionKind::FunctionReference { .. } => hashset!{},
            InstructionKind::ReadLocal(_) => hashset!{},
            InstructionKind::WriteLocal(_, v) => hashset!{ *v },
            InstructionKind::AddressOfLocal(_) => hashset!{},
            InstructionKind::WriteMemory { address, value } => hashset!{ *address, *value },
            InstructionKind::ReadMemory { address, ty: _ } => hashset!{ *address },
            InstructionKind::Add(l, r)
            | InstructionKind::Subtract(l, r)
            | InstructionKind::Multiply(l, r)
            | InstructionKind::Equals(l, r)
            | InstructionKind::LessThan(l, r)
            | InstructionKind::GreaterThan(l, r)
            | InstructionKind::BitwiseAnd(l, r)
            | InstructionKind::BitwiseXor(l, r)
            | InstructionKind::BitwiseOr(l, r) => hashset!{ *l, *r },
            InstructionKind::BitwiseNot(v) => hashset!{ *v },
            InstructionKind::Call { target, arguments } =>
                arguments.iter().copied().chain([*target]).collect(),
            InstructionKind::WordSize(_) => hashset!{},
            InstructionKind::FieldOffset { .. } => hashset!{},
            InstructionKind::InlineAssembly(_) => hashset!{},
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
            InstructionKind::CastReinterpret { value: _, ty } => Ok(Some(ty.clone())),
            InstructionKind::FunctionReference { ty, .. } => Ok(Some(ty.clone())),

            InstructionKind::ReadLocal(l) => Ok(Some(locals.get_local(*l).ty.clone())),
            InstructionKind::WriteLocal(_, _) => Ok(None),

            InstructionKind::AddressOfLocal(_) => Ok(Some(Type::Pointer)),
            InstructionKind::WriteMemory { .. } => Ok(None),
            InstructionKind::ReadMemory { ty, .. } => Ok(Some(ty.clone())),

            InstructionKind::Add(a, b)
            | InstructionKind::Subtract(a, b)
            | InstructionKind::Multiply(a, b) => {
                let a_ty = vars.get_variable(*a).ty.clone();
                let b_ty = vars.get_variable(*b).ty.clone();

                let is_pointer_arithmetic = a_ty == Type::Pointer && b_ty.is_integral();
                if !is_pointer_arithmetic && a_ty != b_ty {
                    return Err(TypeError::new("both sides of arithmetic binop must either have the same type, or be `pointer` and `integral`"));
                }
                
                Ok(Some(a_ty))
            },

            InstructionKind::BitwiseAnd(a, b)
            | InstructionKind::BitwiseXor(a, b)
            | InstructionKind::BitwiseOr(a, b) => {
                let a_ty = vars.get_variable(*a).ty.clone();
                let b_ty = vars.get_variable(*b).ty.clone();

                if a_ty != b_ty {
                    return Err(TypeError::new("both sides of bitwise binop must either have the same type, or be `pointer` and `integral`"));
                }
                
                Ok(Some(a_ty))
            }

            InstructionKind::BitwiseNot(v) => {
                let ty = vars.get_variable(*v).ty.clone();

                if !ty.is_integral() {
                    return Err(TypeError::new("bitwise NOT can only be applied to an integral type"));
                }

                Ok(Some(ty))
            }

            InstructionKind::Equals(_, _)
            | InstructionKind::LessThan(_, _)
            | InstructionKind::GreaterThan(_, _) => Ok(Some(Type::Boolean)),

            InstructionKind::Call { target, arguments: _ } => {
                let target_ty = &vars.get_variable(*target).ty;
                let Type::FunctionReference { argument_types: _, return_type } = target_ty else {
                    return Err(TypeError::new("`Call` is only valid on a `FunctionReference`"));
                };

                Ok(Some(*return_type.clone()))
            }

            InstructionKind::WordSize(_) => Ok(Some(Type::UnsignedInteger(IntegerSize::Bits16))),

            InstructionKind::FieldOffset { ty, index } => {
                let Type::Struct(fields) = ty else {
                    return Err(TypeError::new("`FieldAccess` type must be a `Struct`"));
                };

                Ok(Some(fields[*index].clone()))
            }

            InstructionKind::InlineAssembly(_) => Ok(None),
            
            InstructionKind::Return(_)
            | InstructionKind::Branch(_)
            | InstructionKind::ConditionalBranch { .. } => Ok(None),

            InstructionKind::Phi { choices } => {
                let choice_tys = choices.iter()
                    .map(|(_, var)| vars.get_variable(*var).ty.clone())
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

                Ok(Some(first_ty.clone()))
            }

            InstructionKind::Unreachable => Ok(None),
        }
    }
}

impl PrintIR for Instruction {
    fn print_ir(&self, options: &super::PrintOptions) -> String {
        match &self.kind {
            InstructionKind::Constant(c) => c.print_ir(options),
            InstructionKind::CastReinterpret { value, ty } =>
                format!("cast (reinterpret) {} as {}", value.print_ir(options), ty),
            InstructionKind::FunctionReference { name, ty } =>
                format!("funcref `{name}` : {ty}"),

            InstructionKind::ReadLocal(l) => format!("read {}", l.print_ir(options)),
            InstructionKind::WriteLocal(l, v) => format!("write {} = {}", l.print_ir(options), v.print_ir(options)),
            InstructionKind::AddressOfLocal(l) => format!("addrof {}", l.print_ir(options)),

            InstructionKind::WriteMemory { address, value } => format!("write [{}] = {}", address.print_ir(options), value.print_ir(options)),
            InstructionKind::ReadMemory { address, ty } => format!("read [{}] (as {})", address.print_ir(options), ty),

            InstructionKind::Add(a, b) => format!("{} + {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::Subtract(a, b) => format!("{} - {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::Multiply(a, b) => format!("{} * {}", a.print_ir(options), b.print_ir(options)),

            InstructionKind::BitwiseAnd(a, b) => format!("{} & {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::BitwiseXor(a, b) => format!("{} ^ {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::BitwiseOr(a, b) => format!("{} | {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::BitwiseNot(v) => format!("~{}", v.print_ir(options)),

            InstructionKind::Equals(a, b) => format!("{} == {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::GreaterThan(a, b) => format!("{} > {}", a.print_ir(options), b.print_ir(options)),
            InstructionKind::LessThan(a, b) => format!("{} < {}", a.print_ir(options), b.print_ir(options)),

            InstructionKind::Call { target, arguments } => 
                format!(
                    "call {} ({})",
                    target.print_ir(options),
                    arguments.iter()
                        .map(|arg| arg.print_ir(options))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),

            InstructionKind::WordSize(ty) => format!("size of {ty}"),

            InstructionKind::FieldOffset { ty, index } => format!("field offset for type {}, index {}", ty, index),

            InstructionKind::InlineAssembly(_) => "<inline asm>".to_owned(),

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
