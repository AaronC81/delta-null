use std::{collections::HashMap, fmt::Display, error::Error};

use delta_null_lang_backend::ir::{Module, FunctionBuilder, LocalId, BasicBlockBuilder, VariableId, self, Instruction, BasicBlockId};

use crate::{node::{TopLevelItem, TopLevelItemKind, self, Type, ExpressionKind, TypeKind, Statement, ComparisonBinOp}, fallible::{Fallible, MaybeFatal}, type_check::{primitive_type_name_to_ir_type, self}};

type ExpressionData = crate::type_check::Type;

/// Translates [TopLevelItem]s into an IR [Module].
pub struct ModuleTranslator {
    module: Module,
}

impl ModuleTranslator {
    pub fn new() -> Self {
        ModuleTranslator {
            module: Module::new(),
        }
    }

    pub fn translate_items(&mut self, items: &[TopLevelItem<ExpressionData>]) -> Fallible<MaybeFatal<()>, TranslateError> {
        let mut errors = Fallible::new_ok(());
        let mut functions = HashMap::new();

        // Build up list of functions
        for item in items {
            if let TopLevelItemKind::FunctionDefinition { name, parameters, return_type, body: _ } = &item.kind {
                // TODO: crap that we're still converting here
                let return_type = node_type_to_ir_type(return_type).propagate(&mut errors);
                functions.insert(
                    name.to_owned(),
                    ir::Type::FunctionReference {
                        argument_types: parameters.iter()
                            .map(|p| node_type_to_ir_type(&p.ty).propagate(&mut errors))
                            .collect(),
                        return_type: Box::new(return_type),
                    }
                );
            }
        }

        for item in items {
            match &item.kind {
                TopLevelItemKind::FunctionDefinition { name, parameters, return_type, body } => {
                    // Setup
                    let mut func_trans = FunctionTranslator::new(
                        FunctionBuilder::new(
                            name,
                            &parameters.iter()
                                .map(|p| {
                                    let ty = node_type_to_ir_type(&p.ty).propagate(&mut errors);
                                    (p.name.clone(), ty)
                                })
                                .collect::<Vec<_>>(),
                        ),
                        &functions,
                        parameters.iter()
                            .map(|p| {
                                let ty = node_type_to_ir_type(&p.ty).propagate(&mut errors);
                                (p.name.clone(), ty)
                            })
                            .collect(),
                    );
                    func_trans.populate_locals(body)?;

                    // If the function returns `void`, then it's permitted not to have a `return` at
                    // the end. But for consistent codegen, we'll insert one here ourselves.
                    let mut body = body.clone();
                    if return_type.kind == TypeKind::Void {
                        let loc = body.loc.clone();
                        body = Statement::new(node::StatementKind::Block {
                            body: vec![
                                body,
                                Statement::new(node::StatementKind::Return(None), loc.clone()),
                            ],
                            trailing_return: false,
                        }, loc.clone());
                    }

                    // Translate
                    let (_, start_block) = func_trans.func.new_basic_block();
                    func_trans.target = Some(start_block);
                    func_trans.translate_statement(&body)?;
                    func_trans.finalize_target();

                    // Add to module
                    let func = func_trans.func.finalize();
                    self.module.functions.push(func);
                },
            }
        }

        Fallible::new_ok(())
    }

    pub fn finalize(self) -> Module {
        self.module
    }
}

/// Translates the contents of a function into an IR function, using the [FunctionBuilder]
/// interface.
pub struct FunctionTranslator<'c> {
    /// The function currently being built.
    func: FunctionBuilder,

    /// A map of local variables to their ID. Currently, this is pregenerated and static throughout
    /// the entire function, even if certain locals are only defined in certain branches.
    locals: HashMap<String, LocalId>,

    /// The basic block which IR instructions are currently being generated onto the end of.
    /// 
    /// For simple sequential statements, this will stay the same, but any statements which
    /// introduce control flow (like `if` or `loop`) could change this multiple times during their
    /// translation.
    /// 
    /// Should never become [None] for any significant period of time, during usage - this is mainly
    /// here to enable usage of `Option::take`.
    target: Option<BasicBlockBuilder>,

    /// The basic block to jump to if a `break` statement is executed.
    /// 
    /// Breakable constructs can be nested, so when introducing a new one, the implementation should
    /// take care to preserve the current one and restore it afterwards.
    /// 
    /// If [None], a `break` is not valid here.
    break_target: Option<BasicBlockId>,

    /// The types of defined functions.
    functions: &'c HashMap<String, ir::Type>,

    /// The types of defined arguments.
    arguments: HashMap<String, ir::Type>,
}

impl<'c> FunctionTranslator<'c> {
    pub fn new(
        func: FunctionBuilder,
        functions: &'c HashMap<String, ir::Type>,
        arguments: HashMap<String, ir::Type>,
    ) -> Self {
        Self {
            func,
            locals: HashMap::new(),
            target: None,
            break_target: None,
            functions,
            arguments,
        }
    }

    /// Builds a mapping of local variables to their IR [LocalId]s.
    /// 
    /// Call this only once, and before doing any translation.
    #[must_use]
    pub fn populate_locals(&mut self, stmt: &node::Statement<ExpressionData>) -> Fallible<MaybeFatal<()>, TranslateError> {
        let mut result = Fallible::new_ok(());

        match &stmt.kind {
            // We're looking for these!
            node::StatementKind::VariableDeclaration { name, ty, .. } => {
                let ty = node_type_to_ir_type(ty).propagate(&mut result);
                let id = self.func.new_local(name, ty);
                self.locals.insert(name.to_owned(), id);
            },

            // Contain other statements, so recursed into
            node::StatementKind::Block { body, .. } => {
                for s in body {
                    self.populate_locals(s).propagate(&mut result);
                }
            },
            node::StatementKind::Loop(body) => {
                self.populate_locals(body).propagate(&mut result);
            },
            node::StatementKind::If { true_body: body, .. } => {
                self.populate_locals(body).propagate(&mut result);
            }

            // Nothing to do
            node::StatementKind::Return(_)
            | node::StatementKind::Expression(_)
            | node::StatementKind::Assignment { .. }
            | node::StatementKind::Break
            | node::StatementKind::InlineAssembly(_) => (),
        }

        result
    }

    /// Translates a parsed language statement into a set of IR instructions.
    #[must_use]
    pub fn translate_statement(&mut self, stmt: &node::Statement<ExpressionData>) -> Fallible<MaybeFatal<()>, TranslateError> {
        match &stmt.kind {
            node::StatementKind::Block { body, .. } => {
                for s in body {
                    self.translate_statement(s)?;
                    if self.target_mut().has_terminator() {
                        break;
                    }
                }
            },
            
            node::StatementKind::Expression(e) => {
                self.translate_expression(e)?;
            },

            node::StatementKind::VariableDeclaration { name, ty: _, value } => {
                // Creating the local was already handled by `populate_locals`.
                let local = *self.locals.get(name).unwrap();

                // If there's an initial value, generate its assignment here.
                if let Some(value) = value {
                    return self.translate_expression(value)?
                        .map(|v| {
                            self.target.as_mut().unwrap().add_void_instruction(
                                ir::Instruction::new(ir::InstructionKind::WriteLocal(local, v))
                            );
                            ().into()
                        });
                }
            }

            node::StatementKind::Assignment { target, value } => {
                match &target.kind {
                    ExpressionKind::Identifier(name) => {
                        if let Some(local) = self.locals.get(name).copied() {
                            self.translate_expression(value)?
                                .map(|v| {
                                    self.target.as_mut().unwrap().add_void_instruction(
                                        ir::Instruction::new(ir::InstructionKind::WriteLocal(local, v))
                                    );
                                    
                                });
                        } else {
                            return Fallible::new_fatal(vec![
                                TranslateError::new(&format!("unknown item `{name}`")),
                            ])
                        }
                    }

                    ExpressionKind::PointerDereference(target) => {
                        self.translate_expression(value)?
                            .combine(self.translate_expression(target)?)
                            .map(|(value, target)| {
                                self.target.as_mut().unwrap().add_void_instruction(
                                    ir::Instruction::new(ir::InstructionKind::WriteMemory {
                                        address: target,
                                        value,
                                    })
                                )
                            });
                    }

                    ExpressionKind::Index { target, index } => {
                        self.translate_expression(value)?
                            .combine(self.generate_array_element_pointer_expression(&*target, &*index)?)
                            .map(|(value, target)| {
                                self.target.as_mut().unwrap().add_void_instruction(
                                    ir::Instruction::new(ir::InstructionKind::WriteMemory {
                                        address: target,
                                        value,
                                    })
                                )
                            });
                    }

                    _ => return Fallible::new_fatal(vec![
                        TranslateError::new(&format!("unsupported assignment target")),
                    ])
                }
            }

            node::StatementKind::Return(value) => {
                if let Some(value) = value {
                    return self.translate_expression(value)?
                        .map(|v| {
                            self.target.as_mut().unwrap().add_terminator(
                                ir::Instruction::new(ir::InstructionKind::Return(Some(v)))
                            );
                            ().into()
                        });
                } else {
                    self.target.as_mut().unwrap().add_terminator(ir::Instruction::new(ir::InstructionKind::Return(None)));
                }
            },

            node::StatementKind::Loop(body) => {
                let (new_id, new_block) = self.func.new_basic_block();
                self.target_mut().add_terminator_if_none(ir::Instruction::new(ir::InstructionKind::Branch(new_id)));

                // Create block for following statements - if `break` is executed, go there!
                let (cont_id, cont_block) = self.func.new_basic_block();
                let old_break_target = self.break_target;
                self.break_target = Some(cont_id);

                // Generate instructions within loop
                self.replace_target(new_block);
                let errors = self.translate_statement(body)?;
                
                // Add infinite-looping terminator
                self.target_mut().add_terminator_if_none(ir::Instruction::new(ir::InstructionKind::Branch(new_id)));

                // Restore old break target
                self.break_target = old_break_target;

                // Place any new instructions in the continuation block
                self.replace_target(cont_block);

                return errors.map(|f| f.into());
            },

            node::StatementKind::Break => {
                if let Some(break_target) = self.break_target {
                    // Insert break terminator
                    self.target_mut().add_terminator(Instruction::new(ir::InstructionKind::Branch(break_target)));

                    // In case there are (unreachable) statements after this, generate a new block for them
                    let (_, cont_block) = self.func.new_basic_block();
                    self.replace_target(cont_block);
                } else {
                    return Fallible::new_with_errors(MaybeFatal::Fatal, vec![
                        TranslateError::new("`break` is not valid here")
                    ])
                }
            }

            node::StatementKind::If { condition, true_body, false_body } => {
                let mut errors = Fallible::new(());

                let condition = self.translate_expression(condition)?
                    .propagate(&mut errors);
                
                // Create blocks for truth
                let (true_id, true_block) = self.func.new_basic_block();
                let (false_id, false_block) =
                    if false_body.is_some() {
                        let (i, b) = self.func.new_basic_block();
                        (Some(i), Some(b))
                    } else {
                        (None, None)
                    };
                let (cont_id, cont_block) = self.func.new_basic_block();

                // Set up conditional branch
                self.target_mut().add_terminator_if_none(Instruction::new(ir::InstructionKind::ConditionalBranch {
                    condition,
                    true_block: true_id,
                    false_block: false_id.unwrap_or(cont_id),
                }));

                // Populate true block
                self.replace_target(true_block);
                self.translate_statement(true_body)?.propagate(&mut errors);
                self.target_mut().add_terminator_if_none(Instruction::new(ir::InstructionKind::Branch(cont_id)));

                // If we have a false block, populate it too
                if let Some(false_block) = false_block {
                    self.replace_target(false_block);
                    self.translate_statement(false_body.as_ref().unwrap())?.propagate(&mut errors);
                    self.target_mut().add_terminator_if_none(Instruction::new(ir::InstructionKind::Branch(cont_id)));    
                }

                // Replace target with continuation block
                self.replace_target(cont_block);

                return errors.map(|f| f.into());
            }

            node::StatementKind::InlineAssembly(contents) => {
                self.target_mut().add_void_instruction(Instruction::new(ir::InstructionKind::InlineAssembly(contents.clone())))
            }
        }

        Fallible::new_ok(())
    }

    /// Translates an expression into a set of IR instructions, and return the [VariableId]
    /// describing the final result of the expression.
    #[must_use]
    pub fn translate_expression(&mut self, expr: &node::Expression<ExpressionData>) -> Fallible<MaybeFatal<VariableId>, TranslateError> {
        match &expr.kind {
            node::ExpressionKind::Identifier(id) => {
                if let Some(local) = self.locals.get(id).copied() {
                    Fallible::new_ok(
                        self.target_mut().add_instruction(
                            ir::Instruction::new(ir::InstructionKind::ReadLocal(local))
                        )
                    )
                } else if let Some(ty) = self.functions.get(id) {
                    Fallible::new_ok(
                        self.target_mut().add_instruction(
                            ir::Instruction::new(ir::InstructionKind::FunctionReference {
                                name: id.clone(),
                                ty: ty.clone(),
                            })
                        )
                    )
                } else if let Some(ty) = self.arguments.get(id) {
                    Fallible::new_ok(self.func.get_argument(id).unwrap())
                } else {
                    Fallible::new_fatal(vec![
                        TranslateError::new(&format!("unknown item `{id}`")),
                    ])
                }
            },

            node::ExpressionKind::PointerTake(target) =>
                self.generate_pointer_take(target),

            node::ExpressionKind::PointerDereference(ptr) => {
                // Assuming a `PointerDereference` in this position is a read.
                // A write would be inside an assignment statement instead.
                let type_check::Type::Pointer(pointee_ty) = &ptr.data else {
                    panic!("dereferencing non-pointer in translation");
                };
                self.translate_expression(ptr)?
                    .map(|ptr|
                        self.target_mut().add_instruction(Instruction::new(ir::InstructionKind::ReadMemory {
                            address: ptr,
                            ty: pointee_ty.to_ir_type(),
                        })).into()
                    )
            }

            node::ExpressionKind::BitwiseNot(v) => {
                self.translate_expression(v)?
                    .map(|v|
                        self.target_mut().add_instruction(
                            Instruction::new(ir::InstructionKind::BitwiseNot(v))
                        ).into()
                    )
            }

            // TODO: what about other types?
            node::ExpressionKind::Integer(i, base) => Fallible::new_ok(
                self.target_mut().add_constant(ir::ConstantValue::U16(u16::from_str_radix(i, *base).unwrap()))
            ),

            node::ExpressionKind::Boolean(b) => Fallible::new_ok(
                self.target_mut().add_constant(ir::ConstantValue::Boolean(*b))
            ),

            node::ExpressionKind::ArithmeticBinOp(op, l, r) => {
                let ir_kind = match op {
                    node::ArithmeticBinOp::Add => ir::InstructionKind::Add,
                    node::ArithmeticBinOp::Subtract => ir::InstructionKind::Subtract,
                    node::ArithmeticBinOp::Multiply => ir::InstructionKind::Multiply,

                    node::ArithmeticBinOp::BitwiseAnd => ir::InstructionKind::BitwiseAnd,
                    node::ArithmeticBinOp::BitwiseXor => ir::InstructionKind::BitwiseXor,
                    node::ArithmeticBinOp::BitwiseOr => ir::InstructionKind::BitwiseOr,
                };

                let parts = self.translate_expression(l)?
                    .combine(self.translate_expression(r)?);

                // Is this pointer arithmetic?
                // If so, we multiply the RHS by the size of the pointee type
                // (Currently pointer arithmetic is not commutative - `pointer + integral`)
                if let type_check::Type::Pointer(pointee) = &l.data {
                    parts
                        .map(|(l, r)| {
                            let size = self.target_mut().add_instruction(
                                ir::Instruction::new(ir::InstructionKind::WordSize(pointee.to_ir_type()))
                            );
                            let scaled_r = self.target_mut().add_instruction(
                                ir::Instruction::new(ir::InstructionKind::Multiply(r, size))
                            );
                            self.target_mut().add_instruction(
                                ir::Instruction::new(ir_kind(l, scaled_r))
                            ).into()
                        })
                } else {
                    parts
                        .map(|(l, r)|
                            self.target_mut().add_instruction(
                                ir::Instruction::new(ir_kind(l, r))
                            ).into())
                }
            }

            node::ExpressionKind::ComparisonBinOp(op, l, r) =>
                self.translate_expression(l)?
                    .combine(self.translate_expression(r)?)
                    .map(|(l, r)| {
                        let instr = match op {
                            ComparisonBinOp::Equals => ir::InstructionKind::Equals,
                            ComparisonBinOp::GreaterThan => ir::InstructionKind::GreaterThan,
                            ComparisonBinOp::LessThan => ir::InstructionKind::LessThan,
                        };
                        self.target_mut().add_instruction(
                            ir::Instruction::new(instr(l, r))
                        ).into()
                    }),

            node::ExpressionKind::Call { target, arguments } => {
                let target = self.translate_expression(target)?;

                let arguments = arguments.iter()
                    .map(|arg| self.translate_expression(arg))
                    .collect::<Fallible<Vec<_>, _>>();
                
                target
                    .combine(arguments)
                    .map(|(target, arguments)|
                        self.target_mut().add_instruction(
                            ir::Instruction::new(ir::InstructionKind::Call {
                                target,
                                arguments: arguments.into_iter()
                                    .map(|arg| arg.unwrap_or(VariableId::ERROR))
                                    .collect()
                            })
                        ).into())
            },

            node::ExpressionKind::Index { target, index } => {
                let type_check::Type::Array(pointee_ty, _) = &target.data else {
                    panic!("translating index to non-array")
                };

                self.generate_array_element_pointer_expression(&*target, &*index)?
                    .map(|v| self.target_mut().add_instruction(
                        Instruction::new(ir::InstructionKind::ReadMemory {
                            address: v,
                            ty: pointee_ty.to_ir_type()
                        })
                    ).into())
            }

            node::ExpressionKind::Cast(value, ty) => {
                self.translate_expression(&value)?.map(|source| {
                    let source_ty = self.func.get_variable_type(source);
                    let target_ty = node_type_to_ir_type(ty).unwrap();

                    if source_ty.is_reinterpret_castable_to(&target_ty) {
                        self.target_mut().add_instruction(Instruction::new(ir::InstructionKind::CastReinterpret {
                            value: source,
                            ty: target_ty,
                        })).into()
                    } else {
                        panic!("cannot cast {source_ty} to {target_ty}")
                    }
                })
            }
        }
    }

    /// Gets a reference to the current basic block where instructions are being generated.
    #[must_use]
    pub fn target_mut(&mut self) -> &mut BasicBlockBuilder {
        self.target.as_mut().unwrap()
    }

    /// Finalises the current basic block.
    fn finalize_target(&mut self) {
        let mut old_target = self.target.take().unwrap();

        // The code generator will sometimes generate empty blocks when it's trying to do a good
        // job, for example...
        // 
        // ```
        // if something {
        //     return 2;
        // } else {
        //     return 3;
        // }
        // ```
        // 
        // ...generates a blank IR block for after the `if` statement, but this will never be
        // reached, and therefore it is valid for no statements to exist in this case.
        // 
        // To handle this, if the block is empty, insert an `Unreachable` instruction.
        if old_target.statement_count() == 0{
            old_target.add_terminator(Instruction::new(ir::InstructionKind::Unreachable))
        }

        old_target.finalize();
    }

    /// Finalises the current basic block, and replaces it with a different one.
    pub fn replace_target(&mut self, new: BasicBlockBuilder) {
        self.finalize_target();
        self.target = Some(new);
    }

    fn generate_pointer_take(&mut self, target: &node::Expression<ExpressionData>) -> Fallible<MaybeFatal<VariableId>, TranslateError> {
        match &target.kind {
            node::ExpressionKind::Identifier(name) => {
                if let Some(local) = self.locals.get(name).copied() {
                    Fallible::new_ok(self.target.as_mut().unwrap().add_instruction(
                        ir::Instruction::new(ir::InstructionKind::AddressOfLocal(local))
                    ))
                } else {
                    return Fallible::new_fatal(vec![
                        TranslateError::new(&format!("unknown item `{name}`")),
                    ])
                }
            }

            _ => return Fallible::new_fatal(vec![
                TranslateError::new(&format!("cannot take pointer to: {target:?}")),
            ])
        }
    }

    /// Given a target expression of type [type_check::Type::Array], and an index expression of
    /// integral type, generates an expression which evaluates to a pointer to the given element
    /// of the target array.
    /// 
    /// This can then be used with [ir::InstructionKind::ReadMemory] or
    /// [ir::InstructionKind::WriteMemory] to retrieve or update the array element.
    fn generate_array_element_pointer_expression(
        &mut self,
        target: &node::Expression<ExpressionData>,
        index: &node::Expression<ExpressionData>
    ) -> Fallible<MaybeFatal<VariableId>, TranslateError> {
        let type_check::Type::Array(pointee_ty, _) = &target.data else {
            panic!("translating index to non-array")
        };

        self.translate_expression(&node::Expression::new_with_data(
            node::ExpressionKind::ArithmeticBinOp(
                node::ArithmeticBinOp::Add,
                Box::new(node::Expression::new_with_data(
                    node::ExpressionKind::PointerTake(Box::new(target.clone())),
                    target.loc.clone(),
                    type_check::Type::Pointer(pointee_ty.clone()),
                )),
                Box::new(index.clone()),
            ),
            target.loc.clone(),
            type_check::Type::Pointer(pointee_ty.clone()),
        ))
    }
}


/// Translates a [Type] to an [ir::Type].
#[must_use]
pub fn node_type_to_ir_type(ty: &Type) -> Fallible<ir::Type, TranslateError> {
    match &ty.kind {
        node::TypeKind::Name(t) => Fallible::new(
            primitive_type_name_to_ir_type(t)
                .expect("undefined type in translation")
        ),
        node::TypeKind::Pointer(_) => Fallible::new(ir::Type::Pointer),
        node::TypeKind::Void => Fallible::new(ir::Type::Void),

        node::TypeKind::Array(ty, size) =>
            node_type_to_ir_type(&ty).map(|ty| ir::Type::Array(Box::new(ty), *size)),
    }
}

#[derive(Debug, Clone)]
pub struct TranslateError {
    description: String,
}

impl TranslateError {
    pub fn new(description: &str) -> Self {
        TranslateError { description: description.to_owned() }
    }
}

impl Display for TranslateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "translate error: {}", self.description)
    }
}
impl Error for TranslateError {}
