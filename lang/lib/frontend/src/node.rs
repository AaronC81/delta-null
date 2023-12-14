/// An item which may appear at the top-level of a module (file), such as a function definition.
#[derive(Clone, Debug)]
pub struct TopLevelItem {
    pub kind: TopLevelItemKind,
}

impl TopLevelItem {
    pub fn new(kind: TopLevelItemKind) -> Self {
        TopLevelItem { kind }
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
}

impl Statement {
    pub fn new(kind: StatementKind) -> Self {
        Statement { kind }
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
}

impl Expression {
    pub fn new(kind: ExpressionKind) -> Self {
        Expression { kind }
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
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Type { kind }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Name(String),
}
