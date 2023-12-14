use crate::source::SourceLocation;

/// An item which may appear at the top-level of a module (file), such as a function definition.
#[derive(Clone, Debug)]
pub struct TopLevelItem {
    pub kind: TopLevelItemKind,
    pub loc: SourceLocation,
}

impl TopLevelItem {
    pub fn new(kind: TopLevelItemKind, loc: SourceLocation) -> Self {
        TopLevelItem { kind, loc }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TopLevelItemKind {
    FunctionDefinition {
        name: String,
        // TODO: parameters
        // TODO: return type
        body: Statement,
    }
}

/// A statement which appears within a function body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement {
    pub kind: StatementKind,
    pub loc: SourceLocation,
}

impl Statement {
    pub fn new(kind: StatementKind, loc: SourceLocation) -> Self {
        Statement { kind, loc }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind {
    Block {
        body: Vec<Statement>,
        trailing_return: bool,
    },
    Expression(Expression),
    VariableDeclaration {
        name: String,
        ty: Type,
        value: Option<Expression>,
    },
    Assignment {
        name: String,
        value: Expression,
    },
    Return(Option<Expression>),
    Loop(Box<Statement>),
    If {
        condition: Expression,
        body: Box<Statement>,
    }
}

/// An expression which calculates part of the value of a statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub loc: SourceLocation,
}

impl Expression {
    pub fn new(kind: ExpressionKind, loc: SourceLocation) -> Self {
        Expression { kind, loc }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    Identifier(String),
    Integer(String),

    Add(Box<Expression>, Box<Expression>),
    Equals(Box<Expression>, Box<Expression>),
}

/// A parsed type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub kind: TypeKind,
    pub loc: SourceLocation,
}

impl Type {
    pub fn new(kind: TypeKind, loc: SourceLocation) -> Self {
        Type { kind, loc }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Name(String),
}
