use std::{fmt::Display, error::Error, collections::HashMap};

use delta_null_lang_backend::ir::{self, IntegerSize};

use crate::{source::SourceLocation, node::{Statement, Expression, StatementKind, self, ExpressionKind, TopLevelItem, TopLevelItemKind}, fallible::Fallible, frontend_error};

/// Describes the type of an IR expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A type which maps directly onto an IR-native [ir::Type].
    Direct(ir::Type),

    /// A more specific version of [ir::Type::FunctionReference]j which retains additional detail
    /// found in [Type].
    FunctionReference {
        argument_types: Vec<Type>,
        return_type: Box<Type>,
    },

    /// A pointer to another [Type].
    /// 
    /// [ir::Type] also has a `Pointer` type, but it does not store the type of the pointee, so this
    /// more-specific type is used instead.
    Pointer(Box<Type>),

    /// A fixed-size array of instances of another [Type].
    Array(Box<Type>, usize),

    /// The type of this expression couldn't be determined. This will come along with some type
    /// errors.
    Unknown,
}

impl Type {
    pub fn to_ir_type(&self) -> ir::Type {
        match self {
            Type::Direct(ty) => ty.clone(),
            Type::FunctionReference { argument_types, return_type } =>
                ir::Type::FunctionReference {
                    argument_types: argument_types.iter().map(|a| a.to_ir_type()).collect(),
                    return_type: Box::new(return_type.to_ir_type()),
                },
            Type::Pointer(_) => ir::Type::Pointer,
            Type::Array(ty, size) => ir::Type::Array(Box::new(ty.to_ir_type()), *size),
            Type::Unknown => panic!(
                "tried to convert `Unknown` type checker type to IR type; this only happens if something else went wrong which should've been caught!"
            ),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Direct(d) => write!(f, "{d}"),
            Type::FunctionReference { argument_types, return_type } =>
                write!(f, "fn({}) -> {return_type}",
                    argument_types.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", ")),
            Type::Pointer(ty) => write!(f, "*{ty}"),
            Type::Array(ty, size) => write!(f, "[{size}]{ty}"),
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
    pub fn resolve_identifier(&self, id: &str, for_write: bool) -> Option<&Type> {
        // Locals
        if let Some(s) = self.local.variables.get(id) { return Some(s) };
        
        // Argument are only readable, not writeable
        if let Some(s) = self.local.arguments.get(id) {
            if for_write {
                panic!("TODO - arguments aren't writable") // TODO - proper error
            }
            return Some(s);
        }

        // Globals
        if let Some(s) = self.module.globals.get(id) { return Some(s); }

        None
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

    /// All defined, accessible arguments.
    arguments: HashMap<String, Type>,

    /// The return type of the enclosing function.
    return_type: Type,
}

pub fn type_check_module(items: Vec<TopLevelItem>) -> Fallible<Vec<TopLevelItem<Type>>, TypeError> {
    let mut errors = Fallible::new(());

    // Build module-level context
    let mut module_ctx = ModuleContext { globals: HashMap::new() };
    for item in &items {
        match &item.kind {
            TopLevelItemKind::FunctionDefinition { name, parameters, return_type, body: _ } => {
                let return_type = convert_node_type(return_type).propagate(&mut errors);
                module_ctx.globals.insert(
                    name.clone(),
                    Type::FunctionReference {
                        argument_types: parameters.iter()
                            .map(|p| convert_node_type(&p.ty).propagate(&mut errors))
                            .collect(),
                        return_type: Box::new(return_type)
                    },
                );
            },
        }
    }

    // Type-check items
    let result = items.into_iter()
        .map(|i| {
            let loc = i.loc.clone();
            i.map(|kind| match kind {
                TopLevelItemKind::FunctionDefinition { name, parameters, return_type, body } => {
                    // Create context
                    let mut ctx = Context {
                        module: &module_ctx,
                        local: LocalContext {
                            variables: HashMap::new(),
                            arguments: parameters.iter()
                                .map(|p| {
                                    let ty = convert_node_type(&p.ty).propagate(&mut errors);
                                    (p.name.clone(), ty)
                                })
                                .collect(),
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

                    TopLevelItemKind::FunctionDefinition { name, parameters, return_type, body }
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

            StatementKind::Assignment { target, value } => {
                let target = type_check_expression(target, ctx).propagate(&mut errors);
                let value = type_check_expression(value, ctx).propagate(&mut errors);

                check_types_are_assignable(&target.data, &value.data, loc).propagate(&mut errors);

                StatementKind::Assignment { target, value }
            }

            StatementKind::InlineAssembly(contents) => StatementKind::InlineAssembly(contents),

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
                if let Some(local_ty) = ctx.resolve_identifier(&id, false) {
                    (ExpressionKind::Identifier(id), local_ty.clone())
                } else {
                    errors.push_error(TypeError::new(
                        &format!("unknown identifier `{id}`"), loc
                    ));
                    (ExpressionKind::Identifier(id), Type::Unknown)
                }
            },

            ExpressionKind::Call { target, arguments } => {
                let target = type_check_expression(*target, ctx).propagate(&mut errors);
                let arguments = arguments.into_iter()
                    .map(|arg| type_check_expression(arg, ctx).propagate(&mut errors))
                    .collect::<Vec<_>>();

                match &target.data {
                    Type::FunctionReference { argument_types, return_type } => {
                        // Check argument count
                        if argument_types.len() != arguments.len() {
                            errors.push_error(TypeError::new(
                                &format!("wrong number of arguments - expected {}, got {}", argument_types.len(), arguments.len()), loc.clone()
                            ));
                        }

                        // Check argument types
                        for (i, (ty, arg)) in argument_types.iter().zip(arguments.iter()).enumerate() {
                            if !types_are_assignable(ty, &arg.data) {
                                errors.push_error(TypeError::new(
                                    &format!("invalid type for argument {} - expected `{}`, got `{}`",
                                        i + 1, ty, &arg.data), loc.clone()
                                ));
                            }
                        }

                        let ty = return_type.clone();
                        (ExpressionKind::Call { target: Box::new(target), arguments }, *ty)
                    },
                    _ => {
                        errors.push_error(TypeError::new(
                            &format!("cannot call non-function value of type `{}`", target.data), loc
                        ));
                        (ExpressionKind::Call { target: Box::new(target), arguments }, Type::Unknown)
                    },
                }
            }

            ExpressionKind::Index { target, index } => {
                let target = type_check_expression(*target, ctx).propagate(&mut errors);
                let index = type_check_expression(*index, ctx).propagate(&mut errors);

                // Check index type
                if let Type::Direct(ir::Type::UnsignedInteger(_)) = index.data {
                    // All good :)
                } else {
                    errors.push_error(TypeError::new(
                        &format!("cannot use `{}` to index an array", index.data), index.loc.clone()
                    ));
                }

                // Check that target is an array, and pull out element type if so
                let ty =
                    if let Type::Array(ty, _) = &target.data {
                        *ty.clone()
                    } else {
                        Type::Unknown
                    };

                (ExpressionKind::Index { target: Box::new(target), index: Box::new(index) }, ty)
            }

            ExpressionKind::Cast(value, ty) => {
                let value = type_check_expression(*value, ctx).propagate(&mut errors);
                let target_ty = convert_node_type(&ty).propagate(&mut errors);

                // Is a cast possible?
                let is_castable = match (&value.data, &target_ty) {
                    // The backend already has a utility method to see if two IR types are
                    // reinterpret-castable. Recycle that for conversions like `u16 -> i16`
                    (Type::Direct(src), Type::Direct(tgt))
                        if src.is_reinterpret_castable_to(tgt) => true,

                    // Pointers are castable to any other type of pointer, regardless of the
                    // pointee type
                    (Type::Pointer(_), Type::Pointer(_)) => true,

                    (Type::Pointer(_), Type::Direct(ir::Type::UnsignedInteger(IntegerSize::Bits16)))
                    | (Type::Direct(ir::Type::UnsignedInteger(IntegerSize::Bits16)), Type::Pointer(_))
                        => true,

                    _ => false,
                };

                if !is_castable {
                    errors.push_error(TypeError::new(
                        &format!("cannot cast `{}` to `{}`", value.data, target_ty), loc
                    ));
                }
                
                (ExpressionKind::Cast(Box::new(value), ty), target_ty)
            }

            ExpressionKind::PointerTake(target) => {
                let target = type_check_expression(*target, ctx).propagate(&mut errors);

                let ty = Type::Pointer(Box::new(target.data.clone()));
                (ExpressionKind::PointerTake(Box::new(target)), ty)
            }

            ExpressionKind::PointerDereference(ptr) => {
                let ptr = type_check_expression(*ptr, ctx).propagate(&mut errors);

                let ty =
                    if let Type::Pointer(pointee) = &ptr.data {
                        *pointee.clone()
                    } else {
                        errors.push_error(TypeError::new(
                            &format!("cannot dereference non-pointer type `{}`", ptr.data), loc
                        ));
                        Type::Unknown
                    };

                (ExpressionKind::PointerDereference(Box::new(ptr)), ty)
            }

            ExpressionKind::BitwiseNot(v) => {
                let v = type_check_expression(*v, ctx).propagate(&mut errors);
                let ty = v.data.clone();

                // Check that inner type is integral
                if let Type::Direct(ref ir) = ty && ir.is_integral() {
                    // Good!
                } else {
                    errors.push_error(TypeError::new(
                        &format!("operator `~` is not compatible with types `{}`", v.data), loc
                    ))
                }

                (ExpressionKind::BitwiseNot(Box::new(v)), ty)
            }

            ExpressionKind::Integer(int, base) => {
                if u16::from_str_radix(&int, base).is_err() {
                    errors.push_error(TypeError::new(
                        &format!("integer literal '{int}' out of range"), loc
                    ))
                }

                (ExpressionKind::Integer(int, base), Type::Direct(ir::Type::UnsignedInteger(ir::IntegerSize::Bits16)))
            },

            ExpressionKind::Boolean(b) =>
                (ExpressionKind::Boolean(b), Type::Direct(ir::Type::Boolean)),

            ExpressionKind::ArithmeticBinOp(op, l, r) => {
                let l = type_check_expression(*l, ctx).propagate(&mut errors);
                let r = type_check_expression(*r, ctx).propagate(&mut errors);
                let ty = arithmetic_binop_result_type(&l.data, &r.data, &op.to_string(), loc).propagate(&mut errors);
                (ExpressionKind::ArithmeticBinOp(op, Box::new(l), Box::new(r)), ty)
            },

            ExpressionKind::ComparisonBinOp(op, l, r) => {
                let l = type_check_expression(*l, ctx).propagate(&mut errors);
                let r = type_check_expression(*r, ctx).propagate(&mut errors);
                arithmetic_binop_result_type(&l.data, &r.data, "==", loc).propagate(&mut errors);
                let ty = Type::Direct(ir::Type::Boolean);
                (ExpressionKind::ComparisonBinOp(op, Box::new(l), Box::new(r)), ty)
            },
        }
    });

    errors.map(|_| result)
}

#[must_use]
pub fn types_are_assignable(target: &Type, source: &Type) -> bool {
    // Pointers to arrays can "decay" into a pointer to the array's element type.
    //
    // Note that our arrays **do not** work like C arrays. The array local is not itself a pointer 
    // to the first element - rather, the array local is *an array*, a distinct type sized as
    // `element_size * count`.
    // A pointer to the array, however, is equivalent to a pointer to its first element.
    // Our notion of decay is `*[4]u16 -> *u16`, **not** `[4]u16 -> *u16` like C would model it.
    if let Type::Pointer(box Type::Array(source_element_ty, _)) = source {
        if let Type::Pointer(target_element_ty) = target {
            if types_are_assignable(&target_element_ty, source_element_ty) {
                return true;
            }
        }
    }

    // Otherwise, they just need to be the same type
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
    if let Type::Direct(left_ir) = left && left_ir.is_integral() && left == right {
        // Arithmetic between two identical, integral types (e.g. `u16`)
        Fallible::new(left.clone())
    } else if let Type::Pointer(_) = left && let Type::Direct(right_ir) = right && right_ir.is_integral() {
        // Pointer LHS, integral RHS - this is pointer arithmetic!
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
        | StatementKind::Expression(_)
        | StatementKind::InlineAssembly(_) => false,
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
        | StatementKind::Break
        | StatementKind::InlineAssembly(_) => None,
    }
}

/// Translates a [node::Type] to an [Type].
#[must_use]
fn convert_node_type(ty: &node::Type) -> Fallible<Type, TypeError> {
    match &ty.kind {
        node::TypeKind::Name(t) => 
            match primitive_type_name_to_ir_type(t) {
                Some(v) => Fallible::new(Type::Direct(v)),
                None => Fallible::new_with_errors(Type::Unknown,
                    vec![TypeError::new(&format!("unknown type `{t}`"), ty.loc.clone())])
            },
        node::TypeKind::Pointer(ty) =>
            convert_node_type(ty).map(|ty| Type::Pointer(Box::new(ty))),
        node::TypeKind::Array(ty, size) =>
            convert_node_type(ty).map(|ty| Type::Array(Box::new(ty), *size)),
        node::TypeKind::Void => Fallible::new(Type::Direct(ir::Type::Void)),
    }
}

/// Returns an [ir::Type] for a given primitive type name `u16`, if one exists.
pub fn primitive_type_name_to_ir_type(name: &str) -> Option<ir::Type> {
    match name {
        "u16" => Some(ir::Type::UnsignedInteger(ir::IntegerSize::Bits16)),
        "i16" => Some(ir::Type::SignedInteger(ir::IntegerSize::Bits16)),
        "bool" => Some(ir::Type::Boolean),
        _ => None,
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
