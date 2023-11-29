//! Encodes an intermediate representation for code, expressed in
//! [SSA form](https://en.wikipedia.org/wiki/Static_single-assignment_form).

use std::collections::HashMap;

mod stmt;
pub use stmt::*;

mod builder;
pub use builder::*;

/// A single function, which is composed of many basic blocks.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub blocks: HashMap<BasicBlockId, BasicBlock>,
    pub variables: HashMap<VariableId, Variable>,
}

impl Function {
    /// All statements across all basic blocks of the function.
    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.blocks.iter()
            .flat_map(|(_, block)| &block.statements)
    }

    /// Gets a single statement by its ID.
    pub fn get_statement(&self, id: StatementId) -> &Statement {
        let StatementId(block_id, index) = id;
        let block = self.get_basic_block(block_id);
        &block.statements[index]
    }

    /// Gets a single basic block by its ID.
    pub fn get_basic_block(&self, id: BasicBlockId) -> &BasicBlock {
        self.blocks.get(&id).expect("missing basic block")
    }

    /// Gets the successor statements which could be executed after the given statement.
    /// 
    /// If the statement is not a terminator, then this is the statement immediately following it
    /// inside its block.
    /// 
    /// If the statement is a terminator, then this is the set of first statements from the blocks
    /// which it could branch to, if any.
    pub fn statement_successors(&self, stmt: StatementId) -> Vec<StatementId> {
        let instr = &self.get_statement(stmt).instruction;

        // If the instruction is a terminator, its successors are the destinations
        if instr.is_terminator() {
            return instr.branch_destinations()
                .iter()
                .map(|bbid| bbid.first_statement_id())
                .collect();
        }

        // Otherwise, it's just the next instruction in the block
        vec![self.get_statement(StatementId(stmt.0, stmt.1 + 1)).id]
    }

    /// Finds the statement which assigns to the given variable.
    pub fn statement_assigning_to(&self, var: VariableId) -> &Statement {
        for block in self.blocks.values() {
            if let Some(stmt) = block.statement_assigning_to(var) {
                return stmt;
            }
        }

        unreachable!()
    }
}

/// Uniquely identifies a [BasicBlock] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlockId(usize);

impl BasicBlockId {
    pub fn first_statement_id(self) -> StatementId {
        StatementId(self, 0)
    }
}

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

    // Returns the first statement in the block.
    pub fn first_statement(&self) -> &Statement {
        &self.statements.first().unwrap()
    }

    /// Tries to find the statement in the block which assigns to the given variable ID, else
    /// returns `None` if it's not here.
    pub fn statement_assigning_to(&self, var: VariableId) -> Option<&Statement> {
        self.statements.iter().find(|stmt| stmt.result == Some(var))
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
    Boolean,
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
