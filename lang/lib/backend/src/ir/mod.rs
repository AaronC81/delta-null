//! Encodes an intermediate representation for code, expressed in
//! [SSA form](https://en.wikipedia.org/wiki/Static_single-assignment_form).

use std::collections::HashMap;

mod stmt;
pub use stmt::*;

mod builder;
pub use builder::*;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub blocks: HashMap<BasicBlockId, BasicBlock>,
    pub variables: HashMap<VariableId, Variable>,
}

impl Function {
    pub fn get_statement(&self, id: StatementId) -> &Statement {
        let StatementId(block_id, index) = id;
        let block = self.get_basic_block(block_id);
        &block.statements[index]
    }

    pub fn get_basic_block(&self, id: BasicBlockId) -> &BasicBlock {
        self.blocks.get(&id).expect("missing basic block")
    }
}

/// Uniquely identifies a [BasicBlock] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlockId(usize);

/// A block of [Statement]s which execute sequentially, where the last is a terminator which will
/// branch to another [BasicBlock] or leave the enclosing [Function].
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub statements: Vec<Statement>,
}

impl BasicBlock {
    /// Returns this block's final statement, which must always be a terminator instruction.
    pub fn terminator(&self) -> &Statement {
        let term = self.statements.last().expect("block is empty");
        assert!(term.instruction.is_terminator(), "last statement of block must be a terminator");
        term
    }
}

/// Uniquely identifies a [Statement] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementId(BasicBlockId, usize);

/// Uniquely identifies a [Variable] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableId(usize);

/// Contains the value produced by an [Instruction]. As this IR is in SSA form, any variable is
/// assigned to exactly once.
#[derive(Debug, Clone)]
pub struct Variable {
    pub id: VariableId,
    pub ty: Type,
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

/// Models some variable container.
pub trait VariableRepository {
    fn get_variable(&self, id: VariableId) -> &Variable;
}

impl VariableRepository for Function {
    fn get_variable(&self, id: VariableId) -> &Variable {
        self.variables.get(&id).unwrap()
    }
}
