//! Describes the node types produced by the parser.
//! 
//! The node types here take up to two type parameters:
//!   - `D` - Arbitrary expression data. Expressions store a value of this type. This enables
//!           association of additional information with each expression, which can be used later in
//!           the compilation process.
//!   - `Ty` - The type representing a _type_ in the current stage of the compilation process.
//!            Defaults to an AST node, but after type-checking, this can be replaced with something
//!            holding more semantic meaning. 

use std::fmt::Display;

use crate::source::SourceLocation;

/// An item which may appear at the top-level of a module (file), such as a function definition.
#[derive(Clone, Debug)]
pub struct TopLevelItem<D = (), Ty = crate::node::Type> {
    pub kind: TopLevelItemKind<D, Ty>,
    pub loc: SourceLocation,
}

impl<D, Ty> TopLevelItem<D, Ty> {
    pub fn new(kind: TopLevelItemKind<D, Ty>, loc: SourceLocation) -> Self {
        TopLevelItem { kind, loc }
    }

    pub fn map<OD, OTy>(self, func: impl FnOnce(TopLevelItemKind<D, Ty>) -> TopLevelItemKind<OD, OTy>) -> TopLevelItem<OD, OTy> {
        TopLevelItem {
            kind: func(self.kind),
            loc: self.loc,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TopLevelItemKind<D, Ty> {
    FunctionDefinition {
        name: String,
        parameters: Vec<FunctionParameter<Ty>>,
        return_type: Ty,
        body: Statement<D, Ty>,
    },
    TypeAlias {
        name: String,
        ty: Ty,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionParameter<Ty> {
    pub name: String,
    pub ty: Ty,
}

impl<Ty> FunctionParameter<Ty> {
    pub fn map_type<OTy>(self, func: impl FnOnce(Ty) -> OTy) -> FunctionParameter<OTy> {
        FunctionParameter {
            ty: func(self.ty),
            ..self
        }
    }
}

/// A statement which appears within a function body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement<D = (), Ty = crate::node::Type> {
    pub kind: StatementKind<D, Ty>,
    pub loc: SourceLocation,
}

impl<D, Ty> Statement<D, Ty> {
    pub fn new(kind: StatementKind<D, Ty>, loc: SourceLocation) -> Self {
        Self { kind, loc }
    }

    pub fn map<OD, OTy>(self, func: impl FnOnce(StatementKind<D, Ty>) -> StatementKind<OD, OTy>) -> Statement<OD, OTy> {
        Statement {
            kind: func(self.kind),
            loc: self.loc,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind<D = (), Ty = crate::node::Type> {
    Block {
        body: Vec<Statement<D, Ty>>,
        trailing_return: bool,
    },
    Expression(Expression<D, Ty>),
    VariableDeclaration {
        name: String,
        ty: Ty,
        value: Option<Expression<D, Ty>>,
    },
    Assignment {
        target: Expression<D, Ty>,
        value: Expression<D, Ty>,
    },
    Return(Option<Expression<D, Ty>>),
    Loop(Box<Statement<D, Ty>>),
    Break,
    If {
        condition: Expression<D, Ty>,
        true_body: Box<Statement<D, Ty>>,
        false_body: Option<Box<Statement<D, Ty>>>,
    },
    InlineAssembly(String),
}

/// An expression which calculates part of the value of a statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression<D = (), Ty = crate::node::Type> {
    pub kind: ExpressionKind<D, Ty>,
    pub loc: SourceLocation,
    pub data: D,
}

impl<D, Ty> Expression<D, Ty> {
    pub fn new_with_data(kind: ExpressionKind<D, Ty>, loc: SourceLocation, data: D) -> Self {
        Expression::<D, Ty> { kind, loc, data }
    }

    pub fn map<OD, OTy>(self, func: impl FnOnce(ExpressionKind<D, Ty>, D) -> (ExpressionKind<OD, OTy>, OD)) -> Expression<OD, OTy> {
        let (kind, data) = func(self.kind, self.data);
        Expression::<OD, OTy> {
            kind,
            data,
            loc: self.loc,
        }
    }
}

impl<D: Default, Ty> Expression<D, Ty> {
    pub fn new(kind: ExpressionKind<D, Ty>, loc: SourceLocation) -> Self {
        Self::new_with_data(kind, loc, D::default())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind<D, Ty> {
    Identifier(String),
    
    /// No base specifier in the string - see [crate::tokenizer::TokenKind::Integer].
    Integer(String, u32),

    Boolean(bool),

    Call {
        target: Box<Expression<D, Ty>>,
        arguments: Vec<Expression<D, Ty>>,
    },
    Index {
        target: Box<Expression<D, Ty>>,
        index: Box<Expression<D, Ty>>,
    },

    Cast(Box<Expression<D, Ty>>, Ty),

    PointerTake(Box<Expression<D, Ty>>),
    PointerDereference(Box<Expression<D, Ty>>),
    BitwiseNot(Box<Expression<D, Ty>>),

    ArithmeticBinOp(ArithmeticBinOp, Box<Expression<D, Ty>>, Box<Expression<D, Ty>>),
    ComparisonBinOp(ComparisonBinOp, Box<Expression<D, Ty>>, Box<Expression<D, Ty>>),
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
