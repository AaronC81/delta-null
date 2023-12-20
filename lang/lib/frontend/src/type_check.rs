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

/// A bundle of both module-level and local context.
#[derive(Debug, Clone)]
pub struct Context<'m> {
    module: &'m ModuleContext,
    local: LocalContext,
}

impl<'m> Context<'m> {
    pub fn resolve_identifier(&self, id: &str) -> Option<&Type> {
        self.local.variables.get(id).or_else(|| self.module.globals.get(id))
    }
}

/// Describes the context within a module.
#[derive(Debug, Clone)]
pub struct ModuleContext {
    /// All defined globals.
    globals: HashMap<String, Type>,
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

    // Build module-level context
    let mut module_ctx = ModuleContext { globals: HashMap::new() };
    for item in &items {
        match &item.kind {
            TopLevelItemKind::FunctionDefinition { name, return_type, body: _ } => {
                let Type::Direct(return_type) = convert_node_type(return_type).propagate(&mut errors) else {
                    panic!("more advanced return type than anticipated!");
                };
                module_ctx.globals.insert(
                    name.clone(),
                    Type::Direct(ir::Type::FunctionReference { return_type: Box::new(return_type) }),
                );
            },
        }
    }

    // Type-check items
    let result = items.into_iter()
        .map(|i| {
            let loc = i.loc.clone();
            i.map(|kind| match kind {
                TopLevelItemKind::FunctionDefinition { name, return_type, body } => {
                    // Create context
                    let mut ctx = Context {
                        module: &module_ctx,
                        local: LocalContext {
                            variables: HashMap::new(),
                            return_type: convert_node_type(&return_type).propagate(&mut errors),
                        },
                    };

                    // Type-check body
                    let body = type_check_statement(body, &mut ctx)
                        .propagate(&mut errors);

                    // Check that all control paths diverge (return something or loop forever)
                    if ctx.local.return_type != Type::Direct(ir::Type::Void) && !do_all_paths_diverge(&body) {
                        errors.push_error(TypeError::new(
                            &format!("not all control-flow paths of `{name}` return a value"), loc
                        ))
                    }

                    TopLevelItemKind::FunctionDefinition { name, return_type, body }
                },
            })
        })
        .collect();

    errors.map(|_| result)
}

pub fn type_check_statement(stmt: Statement<()>, ctx: &mut Context) -> Fallible<Statement<Type>, TypeError> {
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
                if ctx.local.variables.contains_key(&name) {
                    errors.push_error(TypeError::new(
                        &format!("redefinition of local variable `{name}`"), loc
                    ));
                } else {
                    ctx.local.variables.insert(name.clone(), converted_ty.clone());
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

                if let Some(local_ty) = ctx.local.variables.get(&name) {
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
                    check_types_are_assignable(&ctx.local.return_type, &value.data, loc).propagate(&mut errors);
                } else {
                    todo!("valueless return")
                }

                StatementKind::Return(value)
            }

            StatementKind::Loop(body) =>
                StatementKind::Loop(Box::new(
                    type_check_statement(*body, ctx).propagate(&mut errors)
                )),

            StatementKind::Break => StatementKind::Break,

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

pub fn type_check_expression(expr: Expression<()>, ctx: &mut Context) -> Fallible<Expression<Type>, TypeError> {
    let mut errors = Fallible::new(());
    let loc = expr.loc.clone();
    let result = expr.map(|kind, _| {
        match kind {
            ExpressionKind::Identifier(id) => {
                if let Some(local_ty) = ctx.resolve_identifier(&id) {
                    (ExpressionKind::Identifier(id), local_ty.clone())
                } else {
                    errors.push_error(TypeError::new(
                        &format!("unknown identifier `{id}`"), loc
                    ));
                    (ExpressionKind::Identifier(id), Type::Unknown)
                }
            },

            ExpressionKind::Call { target, arguments } => {
                if !arguments.is_empty() { panic!("arguments nyi") }

                let target = type_check_expression(*target, ctx).propagate(&mut errors);

                match &target.data {
                    Type::Direct(ir::Type::FunctionReference { return_type }) => {
                        let ty = Type::Direct(*return_type.clone());
                        (ExpressionKind::Call { target: Box::new(target), arguments: todo!() }, ty)
                    },
                    _ => {
                        errors.push_error(TypeError::new(
                            &format!("cannot call non-function value of type `{}`", target.data), loc
                        ));
                        (ExpressionKind::Call { target: Box::new(target), arguments: todo!() }, Type::Unknown)
                    },
                }
            }

            ExpressionKind::Integer(int) => {
                if int.parse::<u16>().is_err() {
                    errors.push_error(TypeError::new(
                        &format!("integer literal '{int}' out of range"), loc
                    ))
                }

                (ExpressionKind::Integer(int), Type::Direct(ir::Type::UnsignedInteger(ir::IntegerSize::Bits16)))
            },

            ExpressionKind::Boolean(b) =>
                (ExpressionKind::Boolean(b), Type::Direct(ir::Type::Boolean)),

            ExpressionKind::ArithmeticBinOp(op, l, r) => {
                let l = type_check_expression(*l, ctx).propagate(&mut errors);
                let r = type_check_expression(*r, ctx).propagate(&mut errors);
                let ty = arithmetic_binop_result_type(&l.data, &r.data, &op.to_string(), loc).propagate(&mut errors);
                (ExpressionKind::ArithmeticBinOp(op, Box::new(l), Box::new(r)), ty)
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
            body.iter().any(do_all_paths_diverge),

        StatementKind::If { true_body, false_body, condition: _ } => {
            if let Some(false_body) = false_body {
                do_all_paths_diverge(true_body) && do_all_paths_diverge(false_body)
            } else {
                false
            }
        },

        StatementKind::Return(_) => true,
        StatementKind::Loop(body) =>
            match find_statement(body, &|s| s.kind == StatementKind::Break) {
                // If the loop contains a `break`, then it might not be infinite.
                // But if the body definitely diverges (e.g. always executes a `return`) then this
                // does too. Admittedly this is useless - the loop would only ever execute once -
                // but it makes sense to check.
                Some(_) => do_all_paths_diverge(body),

                // If it doesn't `break`, then it's an infinite loop!
                // The enclosing function could only leave this loop by validating returning, so
                // no statements need to follow this.
                None => true,
            },

        StatementKind::VariableDeclaration { .. } 
        | StatementKind::Assignment { .. }
        | StatementKind::Break
        | StatementKind::Expression(_) => false,
    }
}

/// Walks the tree of statements to find one matching the given predicate, returning it if found.
#[must_use]
pub fn find_statement<'s, T>(stmt: &'s Statement<T>, predicate: &impl Fn(&Statement<T>) -> bool) -> Option<&'s Statement<T>> {
    if predicate(stmt) {
        return Some(stmt)
    }

    match &stmt.kind {
        StatementKind::Block { body, .. } => body.iter().find(|s| find_statement(s, predicate).is_some()),
        StatementKind::If { true_body, false_body, .. } =>
            find_statement(true_body, predicate)
                .or_else(|| false_body.as_ref().and_then(|b| find_statement(b, predicate))),

        StatementKind::VariableDeclaration { .. }
        | StatementKind::Assignment { .. }
        | StatementKind::Expression(_)
        | StatementKind::Return(_)
        | StatementKind::Loop(_)
        | StatementKind::Break => None,
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
    fn test_flow_simple() {
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
    }

    #[test]
    fn test_flow_branching() {
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

        // OK - all branches return
        assert_ok(parse("
            fn main() -> u16 {
                if 1 == 2 {
                    if 2 == 3 { 
                        if 3 == 4 {
                            return 1;
                        } else {
                            return 3;
                        }
                    } else {
                        if 4 == 5 {
                            return 1;
                        } else {
                            return 2;
                        }
                    }
                } else {
                    return 0;
                }
            }
        "));

        // Error - missing a branch
        assert_errors(parse("
            fn main() -> u16 {
                if 1 == 2 {
                    if 2 == 3 { 
                        if 3 == 4 {
                            return 1;
                        } else {
                            return 3;
                        }
                    } else {
                        if 4 == 5 {
                            
                        } else {
                            return 2;
                        }
                    }
                } else {
                    return 0;
                }
            }
        "), "not all control-flow paths");
    }

    #[test]
    fn test_flow_looping() {
        // OK - body of loop always returns
        assert_ok(parse("
            fn main() -> u16 {
                loop {
                    return 1;
                }
            }
        "));

        // OK - loop could break, but this leads to a return
        assert_ok(parse("
            fn main() -> u16 {
                loop {
                    if 1 == 2 {
                        break;
                    } else {
                        return 2;
                    }
                }
                return 1;
            }
        "));

        // Error - loop could break, no return after
        assert_errors(parse("
            fn main() -> u16 {
                loop {
                    if 1 == 2 {
                        break;
                    } else {
                        return 2;
                    }
                }
                // Falls off
            }
        "), "not all control-flow paths");

        // OK - infinite loop
        assert_ok(parse("
            fn main() -> u16 {
                var x: u16 = 0;
                loop {
                    x = x + 1;
                }
            }
        "));
    }
}
