//! Encodes an intermediate representation for code, expressed in
//! [SSA form](https://en.wikipedia.org/wiki/Static_single-assignment_form).

use std::{collections::{HashMap, hash_map::DefaultHasher}, fmt::Display, hash::{Hasher, Hash}};

mod stmt;
pub use stmt::*;

mod builder;
pub use builder::*;

mod util;

mod printer;
pub use printer::*;

/// A collection of functions which is compiled as a unit.
#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,

    /// If this module is executable, the name of the function which acts as an entry point.
    pub entry: Option<String>,
}

impl Module {
    pub fn new() -> Self {
        Module { functions: vec![], entry: None }
    }

    /// Outputs the GraphViz DOT source code for a `digraph` displaying each function's.
    pub fn print_ir_as_graph(&self, options: &PrintOptions) -> String {
        format!(
            "digraph module {{\n{}\n}}",
            self.functions.iter()
                .map(|f| f.print_ir_as_graph(options))
                .collect::<Vec<_>>()
                .join("\n\n")
        )
    }
}

/// A single function, which is composed of many basic blocks.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub blocks: HashMap<BasicBlockId, BasicBlock>,
    pub variables: HashMap<VariableId, Variable>,
    pub locals: HashMap<LocalId, Local>,

    /// An ordered list of arguments which this function accepts.
    /// The corresponding [Variable] instances are available in the `variables` field.
    pub arguments: Vec<VariableId>,
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

    /// Outputs the GraphViz DOT source code for a digraph's `subgraph` displaying this function's
    /// IR, with a node for each basic block, and an edge representing possible control flow between
    /// blocks.
    pub fn print_ir_as_graph(&self, options: &PrintOptions) -> String {
        // Calculate a numeric hash which we can use to represent this function.
        // The name might contain characters which GraphViz doesn't like.
        let mut hasher = DefaultHasher::new();
        self.name.hash(&mut hasher);
        let unique_id = hasher.finish();

        /// Escapes a string for use in a GraphViz label.
        fn escape(s: &str) -> String {
            // https://forum.graphviz.org/t/how-do-i-properly-escape-arbitrary-text-for-use-in-labels/1762/9
            s
                .replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('\n', "\\l") // Left-aligns lines
                .replace('\r', "\\r")
                .replace('\t', "\\t")
        }

        let mut source = format!(
            "subgraph cluster_func{} {{\n  label=\"{} ({})\";\n",
            unique_id,
            self.name,
            self.arguments.iter().map(|v| v.print_ir(options)).collect::<Vec<_>>().join(", ")
        );


        // Create node for each block
        for block in self.ordered_blocks() {
            let ir = block.print_ir(options);
            source.push_str(&format!("  func{}block{} [shape=box label=\"{}\"];\n", unique_id, block.id.0, escape(&ir)))
        }

        // Create edges between blocks
        for block in self.ordered_blocks() {
            for dest in block.terminator().instruction.branch_destinations() {
                source.push_str(&format!("  func{}block{} -> func{}block{};\n", unique_id, block.id.0, unique_id, dest.0))
            }
        }

        source.push_str("\n}");
        source
    }

    /// Returns an iterator over [BasicBlockId]s in ascending order.
    /// 
    /// The meaning of [BasicBlockId]s is not certain or guaranteed, but this is nice for debugging,
    /// where you'd typically expect to see blocks in the order they were created.
    pub fn ordered_blocks(&self) -> impl Iterator<Item = &BasicBlock> {
        let mut ids = self.blocks.keys().copied().collect::<Vec<_>>();
        ids.sort();
        ids.into_iter().map(|id| &self.blocks[&id])
    }

    /// Returns a map of [VariableId]s to the [StatementId]s which reference those variables.
    /// 
    /// (As with other uses of "references", this does not include results.)
    pub fn variable_references(&self) -> HashMap<VariableId, Vec<StatementId>> {
        let mut result = HashMap::new();

        for stmt in self.statements() {
            for var in stmt.instruction.referenced_variables() {
                result.entry(var).or_insert(vec![]).push(stmt.id);
            }
        }

        result
    }
}

impl PrintIR for Function {
    fn print_ir(&self, options: &PrintOptions) -> String {
        format!("=== fn {}({})\n\n{}\n",
            self.name,
            self.arguments.iter()
                .map(|b| b.print_ir(options))
                .collect::<Vec<_>>()
                .join(", "),
            self.ordered_blocks()
                .map(|b| b.print_ir(options))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

/// Uniquely identifies a [BasicBlock] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlockId(pub usize);

impl BasicBlockId {
    pub fn first_statement_id(self) -> StatementId {
        StatementId(self, 0)
    }
}

impl PrintIR for BasicBlockId {
    fn print_ir(&self, _options: &PrintOptions) -> String {
        format!("%{}", self.0)
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
        self.statements.first().unwrap()
    }

    /// Tries to find the statement in the block which assigns to the given variable ID, else
    /// returns `None` if it's not here.
    pub fn statement_assigning_to(&self, var: VariableId) -> Option<&Statement> {
        self.statements.iter().find(|stmt| stmt.result == Some(var))
    }
}

impl PrintIR for BasicBlock {
    fn print_ir(&self, options: &PrintOptions) -> String {
        format!("{}:\n{}",
            self.id.print_ir(options),
            self.statements.iter()
                .map(|s| format!("  {}", s.print_ir(options)))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

/// Uniquely identifies a [Statement] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StatementId(BasicBlockId, usize);

/// Uniquely identifies a [Variable] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableId(usize);

impl VariableId {
    pub const ERROR: VariableId = VariableId(usize::MAX);
}

/// Uniquely identifies a [Local] within a [Function].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(usize);

impl PrintIR for VariableId {
    fn print_ir(&self, _options: &PrintOptions) -> String {
        format!("${}", self.0)
    }
}

impl PrintIR for LocalId {
    fn print_ir(&self, _options: &PrintOptions) -> String {
        format!("<local {}>", self.0)
    }
}

/// Contains the value produced by an [Instruction]. As this IR is in SSA form, any variable is
/// assigned to exactly once.
#[derive(Debug, Clone)]
pub struct Variable {
    pub id: VariableId,
    pub ty: Type,
}

impl PrintIR for Variable {
    fn print_ir(&self, _options: &PrintOptions) -> String {
        format!("${}", self.id.0)
    }
}

/// A local variable, always stored on the stack. Unlike a [Variable], which refers to the SSA form
/// definition of a variable, a local can be reassigned.
#[derive(Debug, Clone)]
pub struct Local {
    pub id: LocalId,
    pub ty: Type,
    pub name: String,
}

impl PrintIR for Local {
    fn print_ir(&self, _options: &PrintOptions) -> String {
        format!("<local {} ({})>", self.id.0, self.name)
    }
}

/// Describes the type of an IR [Variable].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UnsignedInteger(IntegerSize),
    SignedInteger(IntegerSize),
    Boolean,
    Void,
    Pointer,
    Array(Box<Type>, usize),
    Struct(Vec<Type>),
    FunctionReference {
        argument_types: Vec<Type>,
        return_type: Box<Type>,
    }
}

impl Type {
    /// The number of words this type occupies.
    pub fn word_size(&self) -> usize {
        match self {
            Type::UnsignedInteger(_) => 1,
            Type::SignedInteger(_) => 1,
            Type::Boolean => 1,
            Type::Void => 0,
            Type::Pointer => 1,
            Type::Array(ty, size) => ty.word_size() * size,
            Type::Struct(tys) => tys.iter().map(|ty| ty.word_size()).sum(),
            Type::FunctionReference { .. } => 1,
        }
    }

    /// Determines whether this type can be [InstructionKind::CastReinterpret]'ed to another type.
    pub fn is_reinterpret_castable_to(&self, other: &Type) -> bool {
        match (self, other) {
            // Identically-sized integer types
            (Type::SignedInteger(s1), Type::SignedInteger(s2))
            | (Type::UnsignedInteger(s1), Type::UnsignedInteger(s2))
            | (Type::SignedInteger(s1), Type::UnsignedInteger(s2))
            | (Type::UnsignedInteger(s1), Type::SignedInteger(s2))
                if s1 == s2 => true,

            // Integer/pointer conversions
            (Type::UnsignedInteger(IntegerSize::Bits16), Type::Pointer) => true,
            (Type::Pointer, Type::UnsignedInteger(IntegerSize::Bits16)) => true,

            // Simply the same type
            (t1, t2) if t1 == t2 => true,

            _ => false,
        }
    }

    /// Returns whether this is a scalar type, meaning that values of the type are readable in their
    /// entirety with [InstructionKind::ReadLocal].
    pub fn is_scalar(&self) -> bool {
        match self {
            Type::UnsignedInteger(_)
            | Type::SignedInteger(_)
            | Type::Boolean
            | Type::Void
            | Type::Pointer
            | Type::FunctionReference { .. } => true,

            Type::Array(_, _)
            | Type::Struct(_) => false,
        }
    }

    /// Returns whether this type represents an integer.
    pub fn is_integral(&self) -> bool {
        matches!(self, Type::UnsignedInteger(_) | Type::SignedInteger(_))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::UnsignedInteger(s) => write!(f, "u{}", s),
            Type::SignedInteger(s) => write!(f, "i{}", s),
            Type::Boolean => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Pointer => write!(f, "ptr"),
            Type::Array(ty, size) => write!(f, "[{size}]{ty}"),
            Type::Struct(tys) => 
                write!(f, "{{ {} }}",
                    tys.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")),
            Type::FunctionReference { argument_types, return_type } =>
                write!(f, "fn({}) -> {return_type}",
                    argument_types.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))
        }
    }
}

/// The supported sizes of [Type::UnsignedInteger] and [Type::SignedInteger].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntegerSize {
    Bits16,
}

impl Display for IntegerSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerSize::Bits16 => write!(f, "16"),
        }
    }
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

/// Models some local container.
pub trait LocalRepository {
    fn get_local(&self, id: LocalId) -> &Local;
}

impl LocalRepository for Function {
    fn get_local(&self, id: LocalId) -> &Local {
        self.locals.get(&id).unwrap()
    }
}

