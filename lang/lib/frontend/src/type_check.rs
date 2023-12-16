use std::{fmt::Display, error::Error, collections::HashMap};

use delta_null_lang_backend::ir;

use crate::{source::SourceLocation, node::{Statement, Expression, StatementKind, self, ExpressionKind, TopLevelItem, TopLevelItemKind}, fallible::Fallible, frontend_error};

/// Describes the type of an IR expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type which maps directly onto an IR-native [ir::Type].
    Direct(ir::Type),

    /// The type of this expression couldn't be determined. This will come along with some type
    /// errors.
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Direct(d) => write!(f, "{d}"),
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
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
            let loc = i.loc.clone();
            i.map(|kind| match kind {
                TopLevelItemKind::FunctionDefinition { name, return_type, body } => {
                    // Create context
                    let mut ctx = LocalContext {
                        variables: HashMap::new(),
                        return_type: convert_node_type(&return_type).propagate(&mut errors),
                    };

                    // Type-check body
                    let body = type_check_statement(body, &mut ctx)
                        .propagate(&mut errors);

                    // Check that all control paths diverge (return something or loop forever)
                    if ctx.return_type != Type::Direct(ir::Type::Void) {
                        if !do_all_paths_diverge(&body) {
                            errors.push_error(TypeError::new(
                                &format!("not all control-flow paths of `{name}` return a value"), loc
                            ))
                        }
                    }

                    TopLevelItemKind::FunctionDefinition { name, return_type, body }
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

            StatementKind::If { condition, true_body, false_body } => {
                let condition = type_check_expression(condition, ctx).propagate(&mut errors);

                // Condition should be a boolean
                check_types_are_assignable(&Type::Direct(ir::Type::Boolean), &condition.data, loc).propagate(&mut errors);

                let true_body = type_check_statement(*true_body, ctx).propagate(&mut errors);
                let false_body = false_body.map(|b| type_check_statement(*b, ctx).propagate(&mut errors));

                StatementKind::If {
                    condition,
                    true_body: Box::new(true_body),
                    false_body: false_body.map(Box::new),
                }
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
            TypeError::new(&format!("type `{source}` is not assignable to `{target}`"), loc)
        ])
    }
}

#[must_use]
pub fn arithmetic_binop_result_type(left: &Type, right: &Type, op: &str, loc: SourceLocation) -> Fallible<Type, TypeError> {
    if left == right {
        Fallible::new(left.clone()) 
    } else {
        Fallible::new_with_errors(left.clone(), vec![
            TypeError::new(&format!("operator `{op}` is not compatible with types `{left}` and `{right}`"), loc)
        ])
    }
}

/// Returns a boolean indicating whether all paths through the given statement will diverge from the
/// enclosing function.
#[must_use]
pub fn do_all_paths_diverge(body: &Statement<Type>) -> bool {
    match &body.kind {
        StatementKind::Block { body, trailing_return: _ } =>
            body.iter().any(|s| do_all_paths_diverge(s)),

        // TODO: when `else` exists, if both branches diverge, this does too
        StatementKind::If { .. } => false,

        StatementKind::Return(_) => true,
        StatementKind::Loop(_) => false, // TODO: when `break` is added, if the loop contains any, this is false!

        StatementKind::VariableDeclaration { .. } 
        | StatementKind::Assignment { .. }
        | StatementKind::Expression(_) => true,
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
        },
        node::TypeKind::Void => Fallible::new(Type::Direct(ir::Type::Void)),
    }
}

frontend_error!(TypeError, "type");

#[cfg(test)]
mod test {
    use crate::{parser::Parser, tokenizer::tokenize, node::TopLevelItem};

    use super::type_check_module;

    fn parse(code: &str) -> Vec<TopLevelItem> {
        let (tokens, errors) = tokenize(code, "<test>");
        if !errors.is_empty() {
            panic!("{:?}", errors)
        }
        Parser::new(tokens.into_iter().peekable()).parse_module().unwrap()
    }

    fn assert_ok(module: Vec<TopLevelItem>) {
        let tc = type_check_module(module);
        assert!(!tc.has_errors(), "type-checking raised errors when none were expected: {:?}", tc.errors());
    }

    fn assert_errors(module: Vec<TopLevelItem>, containing: &str) {
        let tc = type_check_module(module);
        assert!(tc.has_errors(), "type-checking raised no errors, but expected one containing '{containing}'");

        for error in tc.errors() {
            if error.to_string().contains(containing) {
                return;
            }
        }
        panic!("no raised error contains '{containing}': {:?}", tc.errors())
    }

    #[test]
    fn test_control_flow_check() {
        // OK - needs to return a value, and does
        assert_ok(parse("
            fn main() -> u16 {
                return 1;
            }
        "));

        // OK - no need to return a value
        assert_ok(parse("
            fn main() { }
        "));

        // OK - always returns in the end, but sometimes early
        assert_ok(parse("
            fn main() -> u16 {
                if 1 == 2 {
                    return 2;
                }
                return 0;
            }
        "));

        // Error - false path doesn't return
        assert_errors(parse("
            fn main() -> u16 {
                if 1 == 2 {
                    return 2;
                }
            }
        "), "not all control-flow paths");
    }
}
