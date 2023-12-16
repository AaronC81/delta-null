use std::{fmt::Display, error::Error, collections::HashMap};

use delta_null_lang_backend::ir;

use crate::{source::SourceLocation, node::{Statement, Expression, StatementKind, self, ExpressionKind, TopLevelItem, TopLevelItemKind}, fallible::Fallible};

/// Describes the type of an IR expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type which maps directly onto an IR-native [ir::Type].
    Direct(ir::Type),

    /// The type of this expression couldn't be determined. This will come along with some type
    /// errors.
    Unknown,
}

/// Describes the context within a particular scope of a function.
#[derive(Debug, Clone)]
pub struct LocalContext {
    /// All defined, accessible local variables.
    variables: HashMap<String, Type>,

    /// The return type of the enclosing function.
    return_type: Type,
}

pub fn type_check_module(items: Vec<TopLevelItem>) -> Fallible<Vec<TopLevelItem<Type>>, TypeError> {
    let mut errors = Fallible::new(());
    let result = items.into_iter()
        .map(|i| {
            i.map(|kind| match kind {
                TopLevelItemKind::FunctionDefinition { name, body } => {
                    let mut ctx = LocalContext {
                        variables: HashMap::new(),
                        return_type: Type::Direct(ir::Type::UnsignedInteger(ir::IntegerSize::Bits16)), // TODO
                    };
                    let body = type_check_statement(body, &mut ctx);
                    body.map(|body| TopLevelItemKind::FunctionDefinition { name, body })
                        .propagate(&mut errors)
                },
            })
        })
        .collect();

    errors.map(|_| result)
}

pub fn type_check_statement(stmt: Statement<()>, ctx: &mut LocalContext) -> Fallible<Statement<Type>, TypeError> {
    let mut errors = Fallible::new(());
    let loc = stmt.loc.clone();
    let result = stmt.map(|kind| {
        match kind {
            StatementKind::Block { body, trailing_return } =>
                StatementKind::Block {
                    body: body.into_iter()
                        .map(|child| type_check_statement(child, ctx))
                        .collect::<Fallible<_, _>>()
                        .propagate(&mut errors),
                    trailing_return
                },
            
            StatementKind::Expression(e) =>
                StatementKind::Expression(type_check_expression(e, ctx).propagate(&mut errors)),
            
            StatementKind::VariableDeclaration { name, ty, value } => {
                // Create key in context, which shouldn't exist already
                let converted_ty = convert_node_type(&ty).propagate(&mut errors);
                if ctx.variables.contains_key(&name) {
                    errors.push_error(TypeError::new(
                        &format!("redefinition of local variable `{name}`"), loc
                    ));
                } else {
                    ctx.variables.insert(name.clone(), converted_ty.clone());
                }

                // Check type matches initial value, if specified
                let value = value.map(|e| type_check_expression(e, ctx).propagate(&mut errors));
                if let Some(ref value) = value {
                    check_types_are_assignable(&converted_ty, &value.data, value.loc.clone()).propagate(&mut errors);
                }

                StatementKind::VariableDeclaration { name, ty, value }
            },

            StatementKind::Assignment { name, value } => {
                let value = type_check_expression(value, ctx).propagate(&mut errors);

                if let Some(local_ty) = ctx.variables.get(&name) {
                    check_types_are_assignable(local_ty, &value.data, loc).propagate(&mut errors);
                } else {
                    errors.push_error(TypeError::new(
                        &format!("missing local variable `{name}`"), loc
                    ));
                }

                StatementKind::Assignment { name, value }
            }

            StatementKind::Return(value) => {
                let value = value.map(|e| type_check_expression(e, ctx).propagate(&mut errors));

                if let Some(ref value) = value {
                    check_types_are_assignable(&ctx.return_type, &value.data, loc).propagate(&mut errors);
                } else {
                    todo!("valueless return")
                }

                StatementKind::Return(value)
            }

            StatementKind::Loop(body) =>
                StatementKind::Loop(Box::new(
                    type_check_statement(*body, ctx).propagate(&mut errors)
                )),

            StatementKind::If { condition, body } => {
                let condition = type_check_expression(condition, ctx).propagate(&mut errors);

                // Condition should be a boolean
                check_types_are_assignable(&Type::Direct(ir::Type::Boolean), &condition.data, loc).propagate(&mut errors);

                let body = type_check_statement(*body, ctx).propagate(&mut errors);

                StatementKind::If { condition, body: Box::new(body) }
            },
        }
    });

    errors.map(|_| result)
}

pub fn type_check_expression(expr: Expression<()>, ctx: &mut LocalContext) -> Fallible<Expression<Type>, TypeError> {
    let mut errors = Fallible::new(());
    let loc = expr.loc.clone();
    let result = expr.map(|kind, _| {
        match kind {
            ExpressionKind::Identifier(id) => {
                if let Some(local_ty) = ctx.variables.get(&id) {
                    (ExpressionKind::Identifier(id), local_ty.clone())
                } else {
                    errors.push_error(TypeError::new(
                        &format!("unknown identifier `{id}`"), loc
                    ));
                    (ExpressionKind::Identifier(id), Type::Unknown)
                }
            },

            ExpressionKind::Integer(int) => {
                if int.parse::<u16>().is_err() {
                    errors.push_error(TypeError::new(
                        &format!("integer literal '{int}' out of range"), loc
                    ))
                }

                (ExpressionKind::Integer(int), Type::Direct(ir::Type::UnsignedInteger(ir::IntegerSize::Bits16)))
            },

            ExpressionKind::Add(l, r) => {
                let l = type_check_expression(*l, ctx).propagate(&mut errors);
                let r = type_check_expression(*r, ctx).propagate(&mut errors);
                let ty = arithmetic_binop_result_type(&l.data, &r.data, "+", loc).propagate(&mut errors);
                (ExpressionKind::Add(Box::new(l), Box::new(r)), ty)
            },

            ExpressionKind::Equals(l, r) => {
                let l = type_check_expression(*l, ctx).propagate(&mut errors);
                let r = type_check_expression(*r, ctx).propagate(&mut errors);
                arithmetic_binop_result_type(&l.data, &r.data, "==", loc).propagate(&mut errors);
                let ty = Type::Direct(ir::Type::Boolean);
                (ExpressionKind::Equals(Box::new(l), Box::new(r)), ty)
            },
        }
    });

    errors.map(|_| result)
}

#[must_use]
pub fn types_are_assignable(target: &Type, source: &Type) -> bool {
    target == source
}

#[must_use]
pub fn check_types_are_assignable(target: &Type, source: &Type, loc: SourceLocation) -> Fallible<(), TypeError> {
    if types_are_assignable(target, source) {
        Fallible::new(()) 
    } else {
        Fallible::new_with_errors((), vec![
            TypeError::new(&format!("type `{source:?}` is not assignable to `{target:?}`"), loc)
        ])
    }
}

#[must_use]
pub fn arithmetic_binop_result_type(left: &Type, right: &Type, op: &str, loc: SourceLocation) -> Fallible<Type, TypeError> {
    if left == right {
        Fallible::new(left.clone()) 
    } else {
        Fallible::new_with_errors(left.clone(), vec![
            TypeError::new(&format!("operator `{op}` is not compatible with types `{left:?}` and `{right:?}`"), loc)
        ])
    }
}

/// Translates a [node::Type] to an [Type].
#[must_use]
pub fn convert_node_type(ty: &node::Type) -> Fallible<Type, TypeError> {
    match &ty.kind {
        node::TypeKind::Name(t) => match t.as_ref() {
            "u16" => Fallible::new(Type::Direct(ir::Type::UnsignedInteger(ir::IntegerSize::Bits16))),
            "i16" => Fallible::new(Type::Direct(ir::Type::SignedInteger(ir::IntegerSize::Bits16))),
            "bool" => Fallible::new(Type::Direct(ir::Type::Boolean)),
            _ => Fallible::new_with_errors(
                Type::Unknown,
                vec![TypeError::new(&format!("unknown type `{t}`"), ty.loc.clone())]
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    description: String,
    loc: SourceLocation,
}

impl TypeError {
    pub fn new(description: &str, loc: SourceLocation) -> Self {
        TypeError { description: description.to_owned(), loc }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "type error: {}: {}", self.loc.describe(), self.description)
    }
}
impl Error for TypeError {}

