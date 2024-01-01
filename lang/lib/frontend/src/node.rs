//! Describes the node types produced by the parser.
//! 
//! The node types here take a type parameter, and expressions store a value of that type. This
//! enables association of additional information with each expression, which can be used later in
//! the compilation process.

use std::fmt::Display;

use crate::source::SourceLocation;

/// An item which may appear at the top-level of a module (file), such as a function definition.
#[derive(Clone, Debug)]
pub struct TopLevelItem<D = ()> {
    pub kind: TopLevelItemKind<D>,
    pub loc: SourceLocation,
}

impl<D> TopLevelItem<D> {
    pub fn new(kind: TopLevelItemKind<D>, loc: SourceLocation) -> Self {
        TopLevelItem { kind, loc }
    }

    pub fn map<O>(self, func: impl FnOnce(TopLevelItemKind<D>) -> TopLevelItemKind<O>) -> TopLevelItem<O> {
        TopLevelItem {
            kind: func(self.kind),
            loc: self.loc,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TopLevelItemKind<D> {
    FunctionDefinition {
        name: String,
        parameters: Vec<FunctionParameter>,
        return_type: Type,
        body: Statement<D>,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionParameter {
    pub name: String,
    pub ty: Type,
}

/// A statement which appears within a function body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement<D = ()> {
    pub kind: StatementKind<D>,
    pub loc: SourceLocation,
}

impl<D> Statement<D> {
    pub fn new(kind: StatementKind<D>, loc: SourceLocation) -> Self {
        Self { kind, loc }
    }

    pub fn map<O>(self, func: impl FnOnce(StatementKind<D>) -> StatementKind<O>) -> Statement<O> {
        Statement {
            kind: func(self.kind),
            loc: self.loc,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind<D = ()> {
    Block {
        body: Vec<Statement<D>>,
        trailing_return: bool,
    },
    Expression(Expression<D>),
    VariableDeclaration {
        name: String,
        ty: Type,
        value: Option<Expression<D>>,
    },
    Assignment {
        target: Expression<D>,
        value: Expression<D>,
    },
    Return(Option<Expression<D>>),
    Loop(Box<Statement<D>>),
    Break,
    If {
        condition: Expression<D>,
        true_body: Box<Statement<D>>,
        false_body: Option<Box<Statement<D>>>,
    },
    InlineAssembly(String),
}

/// An expression which calculates part of the value of a statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression<D = ()> {
    pub kind: ExpressionKind<D>,
    pub loc: SourceLocation,
    pub data: D,
}

impl<D> Expression<D> {
    pub fn new_with_data(kind: ExpressionKind<D>, loc: SourceLocation, data: D) -> Self {
        Expression { kind, loc, data }
    }

    pub fn map<O>(self, func: impl FnOnce(ExpressionKind<D>, D) -> (ExpressionKind<O>, O)) -> Expression<O> {
        let (kind, data) = func(self.kind, self.data);
        Expression {
            kind,
            data,
            loc: self.loc,
        }
    }
}

impl<D: Default> Expression<D> {
    pub fn new(kind: ExpressionKind<D>, loc: SourceLocation) -> Self {
        Self::new_with_data(kind, loc, D::default())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind<ED> {
    Identifier(String),
    
    /// No base specifier in the string - see [crate::tokenizer::TokenKind::Integer].
    Integer(String, u32),

    Boolean(bool),

    Call {
        target: Box<Expression<ED>>,
        arguments: Vec<Expression<ED>>,
    },
    Index {
        target: Box<Expression<ED>>,
        index: Box<Expression<ED>>,
    },

    Cast(Box<Expression<ED>>, Type),

    PointerTake(Box<Expression<ED>>),
    PointerDereference(Box<Expression<ED>>),
    BitwiseNot(Box<Expression<ED>>),

    ArithmeticBinOp(ArithmeticBinOp, Box<Expression<ED>>, Box<Expression<ED>>),
    ComparisonBinOp(ComparisonBinOp, Box<Expression<ED>>, Box<Expression<ED>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArithmeticBinOp {
    Add,
    Subtract,
    Multiply,

    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
}

impl Display for ArithmeticBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ArithmeticBinOp::Add => "+",
            ArithmeticBinOp::Subtract => "-",
            ArithmeticBinOp::Multiply => "*",

            ArithmeticBinOp::BitwiseAnd => "&",
            ArithmeticBinOp::BitwiseXor => "^",
            ArithmeticBinOp::BitwiseOr => "|",
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComparisonBinOp {
    Equals,
    GreaterThan,
    LessThan,
}

impl Display for ComparisonBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ComparisonBinOp::Equals => "==",
            ComparisonBinOp::GreaterThan => ">",
            ComparisonBinOp::LessThan => "<",
        })
    }
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
    Void,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
}
