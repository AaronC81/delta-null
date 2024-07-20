//! Describes the node types produced by the parser.
//! 
//! The node types here take up to two type parameters:
//!   - `D` - Arbitrary expression data. Expressions store a value of this type. This enables
//!           association of additional information with each expression, which can be used later in
//!           the compilation process.
//!   - `Ty` - The type representing a _type_ in the current stage of the compilation process.
//!            Defaults to an AST node, but after type-checking, this can be replaced with something
//!            holding more semantic meaning. 

use std::{fmt::Display, path::PathBuf};

use crate::source::{SourceInputType, SourceLocation};

/// A collection of the items parsed from the top-level of a file.
#[derive(Clone, Debug)]
pub struct Module<D = (), Ty = crate::node::Type> {
    /// The source from which this module was loaded.
    pub input_type: SourceInputType,

    /// The items inside this module.
    pub items: Vec<TopLevelItem<D, Ty>>,
}

impl<D, Ty> Module<D, Ty> {
    pub fn new(input_type: SourceInputType) -> Self {
        Module { input_type, items: vec![] }
    }

    /// Return canonical paths for all files which are `use`d by this module.
    pub fn used_files(&self) -> Vec<PathBuf> {
        let mut paths = vec![];
    
        for item in &self.items {
            if let TopLevelItemKind::Use { path } = &item.kind {
                let path = PathBuf::from(path);
                if path.is_absolute() {
                    paths.push(path.canonicalize().unwrap());
                } else {
                    // Build absolute path, assuming that the path is relative to the current file's
                    // containing directory
                    let relative_to = match &self.input_type {
                        SourceInputType::File(f) => f.join(".."),
                        SourceInputType::Buffer { fake_directory: Some(f) } => f.clone(),
                        SourceInputType::Buffer { fake_directory: None } =>
                            panic!("found relative `use` but don't know directory to use as root - pass `--fake-directory` to specify one"),
                    };
                    paths.push(relative_to.join(path).canonicalize().unwrap());
                }
            }
        }
    
        paths
    }

    /// Produces a new [Module] by applying a function to each item in this one.
    pub fn map_items<OD, OTy>(self, func: impl FnMut(TopLevelItem<D, Ty>) -> TopLevelItem<OD, OTy>) -> Module<OD, OTy> {
        Module {
            input_type: self.input_type,
            items: self.items.into_iter().map(func).collect()
        }
    }
}

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
        body: FunctionBody<D, Ty>,
    },
    TypeAlias {
        name: String,
        ty: Ty,
        distinct: bool,
        internal: bool,
    },
    Use {
        path: String,
    },
    VariableDeclaration {
        name: String,
        ty: Ty,
        value: Option<Expression<D, Ty>>,
    },
    InlineAssembly(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionBody<D = (), Ty = crate::node::Type> {
    /// This function's body is provided in the source code, as a statement.
    Statement(Statement<D, Ty>),

    /// This function's body has been omitted, using the `extern` keyword. A definition will be
    /// assumed to be provided elsewhere.
    Extern,
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
    CompoundAssignment {
        target: Expression<D, Ty>,
        value: Compound<D, Ty>,
    },
    Return(Option<Expression<D, Ty>>),
    Loop(Box<Statement<D, Ty>>),
    While {
        condition: Expression<D, Ty>,
        body: Box<Statement<D, Ty>>,
    },
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
    String(String),

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
    FieldAccess {
        target: Box<Expression<D, Ty>>,
        field: String,
    },

    BitwiseNot(Box<Expression<D, Ty>>),
    BooleanNot(Box<Expression<D, Ty>>),
    BooleanAnd(Box<Expression<D, Ty>>, Box<Expression<D, Ty>>),
    ArithmeticBinOp(ArithmeticBinOp, Box<Expression<D, Ty>>, Box<Expression<D, Ty>>),
    ComparisonBinOp(ComparisonBinOp, Box<Expression<D, Ty>>, Box<Expression<D, Ty>>),

    Sizeof(Ty),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArithmeticBinOp {
    Add,
    Subtract,
    Multiply,

    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,

    LeftShift,
    RightShift,
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

            ArithmeticBinOp::LeftShift => "<<",
            ArithmeticBinOp::RightShift => ">>",
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComparisonBinOp {
    Equals,
    GreaterThan,
    GreaterThanOrEquals,
    LessThan,
    LessThanOrEquals,
}

impl Display for ComparisonBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ComparisonBinOp::Equals => "==",
            ComparisonBinOp::GreaterThan => ">",
            ComparisonBinOp::GreaterThanOrEquals => ">=",
            ComparisonBinOp::LessThan => "<",
            ComparisonBinOp::LessThanOrEquals => "<=",
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
    Struct(Vec<(String, Type)>),
}

/// A parsed compound expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Compound<D = (), Ty = crate::node::Type> {
    pub kind: CompoundKind<D, Ty>,
    pub loc: SourceLocation,
}

impl<D, Ty> Compound<D, Ty> {
    pub fn new(kind: CompoundKind<D, Ty>, loc: SourceLocation) -> Self {
        Self { kind, loc }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompoundKind<D = (), Ty = crate::node::Type> {
    Array(Vec<Expression<D, Ty>>),
    Struct(Vec<CompoundField<D, Ty>>),
}

/// A single field assigned as part of a [CompoundKind::Struct].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompoundField<D = (), Ty = crate::node::Type> {
    pub name: String,
    pub value: Expression<D, Ty>,
    pub loc: SourceLocation,
}

impl<D, Ty> CompoundField<D, Ty> {
    pub fn new(name: String, value: Expression<D, Ty>, loc: SourceLocation) -> Self {
        Self { name, value, loc }
    }
}
