//! Encodes an intermediate representation for code, expressed in
//! [SSA form](https://en.wikipedia.org/wiki/Static_single-assignment_form).

mod stmt;
use std::collections::HashMap;

pub use stmt::*;

mod term;
pub use term::*;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub blocks: HashMap<BasicBlockId, BasicBlock>,
    pub variables: HashMap<VariableId, Variable>,
}

/// Uniquely identifies a [BasicBlock] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlockId(usize);

/// A block of [Statement]s which execute sequentially, ending with a [TerminatorInstruction] which
/// will branch to another [BasicBlock] or leave the enclosing [Function].
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub statements: Vec<Statement>,
    pub terminator: TerminatorInstruction,
}

/// Uniquely identifies a [Variable] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariableId(usize);

/// Contains the value produced by a [StatementInstruction]. As this IR is in SSA form, any variable
/// is assigned to exactly once.
#[derive(Debug, Clone)]
pub struct Variable {
    pub id: VariableId,
    pub ty: Type,
}

/// Breaks out shared functionality between [StatementInstruction] and [TerminatorInstruction].
pub trait Instruction {
    /// Returns the IDs of any variables which are read by this instruction. Used to implement
    /// liveness analysis.
    /// 
    /// Should not include the result, if the instruction is a statement - only variables which are
    /// read as part of the instruction itself.
    fn referenced_variables(&self) -> Vec<VariableId>;
}

/// Describes the type of an IR [Variable].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    UnsignedInteger(IntegerSize),
    SignedInteger(IntegerSize),
}

/// The supported sizes of [Type::UnsignedInteger] and [Type::SignedInteger].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntegerSize {
    Bits16,
}

