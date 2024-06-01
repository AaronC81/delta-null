use std::{collections::HashMap, fmt::Display, error::Error};

use delta_null_lang_backend::ir::{self, BasicBlockBuilder, BasicBlockId, Data, FunctionBuilder, Instruction, LocalId, Module, VariableId};

use crate::{fallible::{Fallible, MaybeFatal}, node::{self, ComparisonBinOp, Statement, TopLevelItemKind}, type_check::{self, Type}};

type ExpressionData = crate::type_check::Type;

/// Translates [TopLevelItem]s into an IR [Module].
pub struct ModuleTranslator {
    module: Module,
}

impl ModuleTranslator {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        ModuleTranslator {
            module: Module::new(),
        }
    }

    pub fn translate_items(&mut self, module: &node::Module<ExpressionData, Type>) -> Fallible<MaybeFatal<()>, TranslateError> {
        let mut functions = HashMap::new();
        let mut data = HashMap::new();

        // Build up list of functions and data items
        for item in &module.items {
            if let TopLevelItemKind::FunctionDefinition { name, parameters, return_type, body: _ } = &item.kind {
                // TODO: crap that we're still converting here
                let return_type = return_type.to_ir_type();
                functions.insert(
                    name.to_owned(),
                    ir::Type::FunctionReference {
                        argument_types: parameters.iter()
                            .map(|p| p.ty.to_ir_type())
                            .collect(),
                        return_type: Box::new(return_type),
                    }
                );
            }

            if let TopLevelItemKind::VariableDeclaration { name, ty, value } = &item.kind {
                data.insert(name.to_owned(), ty.to_ir_type());
            }
        }

        for item in &module.items {
            match &item.kind {
                TopLevelItemKind::FunctionDefinition { name, parameters, return_type, body } => {
                    // Setup
                    let mut func_trans = FunctionTranslator::new(
                        FunctionBuilder::new(
                            name,
                            &parameters.iter()
                                .map(|p| {
                                    (p.name.clone(), p.ty.to_ir_type())
                                })
                                .collect::<Vec<_>>(),
                        ),
                        &functions,
                        &data,
                        parameters.iter()
                            .map(|p| {
                                (p.name.clone(), p.ty.to_ir_type())
                            })
                            .collect(),
                    );
                    func_trans.populate_locals(body)?;

                    // If the function returns `void`, then it's permitted not to have a `return` at
                    // the end. But for consistent codegen, we'll insert one here ourselves.
                    let mut body = body.clone();
                    if return_type.is_void() {
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

                // No translation required for type aliases - type-checker did that already
                TopLevelItemKind::TypeAlias { .. } => {},

                // No translation required for imports - they were already resolved
                TopLevelItemKind::Use { .. } => {},

                TopLevelItemKind::VariableDeclaration { name, ty, value } => {
                    if value.is_some() {
                        // TODO
                        panic!("globals with initial values aren't yet supported");
                    }

                    // Add data to module
                    self.module.data.push(Data {
                        name: name.clone(),
                        ty: ty.to_ir_type(),
                    })
                }
            }
        }

        // Right now, this process doesn't error!
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

    /// The types of defined data items.
    data: &'c HashMap<String, ir::Type>,

    /// The types of defined arguments.
    arguments: HashMap<String, ir::Type>,
}

/// Represents possible usages of a value returned by an expression.
/// 
/// This is a higher-level abstraction around a [VariableId]. Depending on whether a value is going
/// to be used for a read or a write, a different set of IR instructions might need to be generated.
/// 
/// A [Value] can be _consumed_ to produce:
///   - A **read**, giving a [VariableId] holding the underlying value
///   - A **write**, taking a [VariableId] which is written as the underlying value
///   - A **pointer**, giving a [VariableId] which is a pointer to the underlying value
/// 
/// This doubles as an optimisation technique - expressions with no side effects may choose not
/// to generate any instructions unless they're consumed, avoiding generation of instructions which
/// ultimately don't do anything.
pub struct Value {
    /// A function which generates the necessary instructions to read this value, and returns the
    /// [VariableId] with the read value. If [None], this value does not support reading.
    read: Option<Box<dyn FnOnce(&mut BasicBlockBuilder) -> VariableId>>,

    /// A function which generates the necessary instructions to write the given [VariableId] to
    /// this value. If [None], this value does not support writing.
    write: Option<Box<dyn FnOnce(&mut BasicBlockBuilder, VariableId)>>,

    /// A function which generates the necessary instructions to obtain a pointer to this value,
    /// and returns the [VariableId] with the pointer. If [None], this value doesn't have a stable
    /// address.   
    pointer: Option<Box<dyn FnOnce(&mut BasicBlockBuilder) -> VariableId>>,
}

impl Value {
    /// Creates a new read-only [Value] given an instruction builder.
    fn new_read_only(read: impl FnOnce(&mut BasicBlockBuilder) -> VariableId + 'static) -> Value {
        Self {
            read: Some(Box::new(read)),
            write: None,
            pointer: None,
        }
    }

    /// Creates a new readable and writable [Value], given instruction builders for each.
    fn new_read_write(
        read: impl FnOnce(&mut BasicBlockBuilder) -> VariableId + 'static,
        write: impl FnOnce(&mut BasicBlockBuilder, VariableId) + 'static
    ) -> Value {
        Self {
            read: Some(Box::new(read)),
            write: Some(Box::new(write)),
            pointer: None,
        }
    }

    /// Creates a new readable, writable, and "pointer-gettable"(!?) [Value], given instruction
    /// builders for each.
    fn new_read_write_pointer(
        read: impl FnOnce(&mut BasicBlockBuilder) -> VariableId + 'static,
        write: impl FnOnce(&mut BasicBlockBuilder, VariableId) + 'static,
        pointer: impl FnOnce(&mut BasicBlockBuilder) -> VariableId + 'static,
    ) -> Value {
        Self {
            read: Some(Box::new(read)),
            write: Some(Box::new(write)),
            pointer: Some(Box::new(pointer)),
        }
    }

    /// Consumes this [Value], generating IR instructions on the given [BasicBlockBuilder] to read
    /// it into a [VariableId].
    fn consume_read(self, target: &mut BasicBlockBuilder) -> VariableId {
        (self.read.expect("value is not supported for read"))(target)
    }
    
    /// Consumes this [Value], generating IR instructions on the given [BasicBlockBuilder] to write
    /// a [VariableId] into it.
    fn consume_write(self,  target: &mut BasicBlockBuilder, value: VariableId) {
        (self.write.expect("value is not supported for write"))(target, value)
    }

    /// Consumes this [Value], generating IR instructions on the given [BasicBlockBuilder] to get
    /// its pointer.
    fn consume_pointer(self,  target: &mut BasicBlockBuilder) -> VariableId {
        (self.pointer.expect("value does not have a pointer"))(target)
    }
}

impl<'c> FunctionTranslator<'c> {
    pub fn new(
        func: FunctionBuilder,
        functions: &'c HashMap<String, ir::Type>,
        data: &'c HashMap<String, ir::Type>,
        arguments: HashMap<String, ir::Type>,
    ) -> Self {
        Self {
            func,
            locals: HashMap::new(),
            target: None,
            break_target: None,
            functions,
            data,
            arguments,
        }
    }

    /// Builds a mapping of local variables to their IR [LocalId]s.
    /// 
    /// Call this only once, and before doing any translation.
    #[must_use]
    pub fn populate_locals(&mut self, stmt: &node::Statement<ExpressionData, Type>) -> Fallible<MaybeFatal<()>, TranslateError> {
        let mut result = Fallible::new_ok(());

        match &stmt.kind {
            // We're looking for these!
            node::StatementKind::VariableDeclaration { name, ty, .. } => {
                let id = self.func.new_local(name, ty.to_ir_type());
                self.locals.insert(name.to_owned(), id);
            },

            // Contain other statements, so recursed into
            node::StatementKind::Block { body, .. } => {
                for s in body {
                    self.populate_locals(s).propagate(&mut result);
                }
            },
            node::StatementKind::Loop(body) 
            | node::StatementKind::While { condition: _, body } => {
                self.populate_locals(body).propagate(&mut result);
            },
            node::StatementKind::If { condition: _, true_body, false_body } => {
                self.populate_locals(true_body).propagate(&mut result);
                if let Some(false_body) = false_body {
                    self.populate_locals(false_body).propagate(&mut result);
                }
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
    pub fn translate_statement(&mut self, stmt: &node::Statement<ExpressionData, Type>) -> Fallible<MaybeFatal<()>, TranslateError> {
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
                            let v = v.consume_read(self.target_mut());
                            self.target.as_mut().unwrap().add_void_instruction(
                                ir::Instruction::new(ir::InstructionKind::WriteLocal(local, v))
                            );
                            ().into()
                        });
                }
            }

            node::StatementKind::Assignment { target, value } => {
                return self.translate_expression(target)?
                    .combine(self.translate_expression(value)?)
                    .map(|(target, value)| {
                        let value = value.consume_read(self.target_mut());
                        target.consume_write(self.target_mut(), value);
                        ().into()
                    })
            }

            node::StatementKind::Return(value) => {
                if let Some(value) = value {
                    return self.translate_expression(value)?
                        .map(|v| {
                            let v = v.consume_read(self.target_mut());
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

            node::StatementKind::While { condition, body } => {
                let mut errors = Fallible::new_ok(());

                // Create block for checking the condition
                let (condition_id, condition_block) = self.func.new_basic_block();
                self.target_mut().add_terminator_if_none(ir::Instruction::new(ir::InstructionKind::Branch(condition_id)));

                // Create block for the body of the loop, executed if the condition is true
                let (body_id, body_block) = self.func.new_basic_block();

                // Create block for following statements - if the condition turns false, or `break`
                // is executed, go there!
                let (cont_id, cont_block) = self.func.new_basic_block();
                let old_break_target = self.break_target;
                self.break_target = Some(cont_id);

                // Populate condition code
                self.replace_target(condition_block);
                let condition = self
                    .translate_expression(condition)?
                    .propagate(&mut errors)
                    .consume_read(self.target_mut());
                self.target_mut().add_terminator_if_none(Instruction::new(ir::InstructionKind::ConditionalBranch {
                    condition,
                    true_block: body_id,
                    false_block: cont_id,
                }));

                // Populate body block
                self.replace_target(body_block);
                self.translate_statement(body)?.propagate(&mut errors);

                // Body block should loop back to the condition block
                self.target_mut().add_terminator_if_none(Instruction::new(ir::InstructionKind::Branch(condition_id)));

                // Restore old break target
                self.break_target = old_break_target;

                // Place any new instructions in the continuation block
                self.replace_target(cont_block);

                return errors;                
            }

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
                let condition = condition.consume_read(self.target_mut());
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
    pub fn translate_expression(&mut self, expr: &node::Expression<ExpressionData, Type>) -> Fallible<MaybeFatal<Value>, TranslateError> {
        match &expr.kind {
            node::ExpressionKind::Identifier(id) => {
                if let Some(local) = self.locals.get(id).copied() {
                    Fallible::new_ok(
                        Value::new_read_write_pointer(
                            move |target| target.add_instruction(
                                ir::Instruction::new(ir::InstructionKind::ReadLocal(local))
                            ),
                            move |target, v| target.add_void_instruction(
                                ir::Instruction::new(ir::InstructionKind::WriteLocal(local, v))
                            ),
                            move |target| target.add_instruction(
                                ir::Instruction::new(ir::InstructionKind::AddressOfLocal(local))
                            ),
                        )
                    )
                } else if let Some(ty) = self.functions.get(id) {
                    let name = id.clone();
                    let ty = ty.clone();
                    Fallible::new_ok(
                        Value::new_read_only(|target: &mut BasicBlockBuilder| target.add_instruction(
                            ir::Instruction::new(ir::InstructionKind::FunctionReference {
                                name,
                                ty,
                            })
                        ))
                    )
                } else if self.arguments.get(id).is_some() {
                    let arg = self.func.get_argument(id).unwrap();
                    Fallible::new_ok(Value::new_read_only(move |_| arg))
                } else if let Some(ty) = self.data.get(id) {
                    // Fighting with the borrow checker, and losing. Very badly
                    let name_1 = id.clone();
                    let name_2 = id.clone();
                    let name_3 = id.clone();
                    let ty = ty.clone();
                    Fallible::new_ok(
                        Value::new_read_write_pointer(
                            move |target| {
                                let address = target.add_instruction(ir::Instruction::new(ir::InstructionKind::DataReference {
                                    name: name_1.clone(),
                                }));
                                target.add_instruction(ir::Instruction::new(ir::InstructionKind::ReadMemory {
                                    address,
                                    ty,
                                }))
                            },
                            move |target, value| {
                                let address = target.add_instruction(ir::Instruction::new(ir::InstructionKind::DataReference {
                                    name: name_2.clone(),
                                }));
                                target.add_void_instruction(ir::Instruction::new(ir::InstructionKind::WriteMemory {
                                    address,
                                    value,
                                }));
                            },
                            move |target| {
                                target.add_instruction(ir::Instruction::new(ir::InstructionKind::DataReference {
                                    name: name_3.clone(),
                                }))
                            },
                        )
                    )
                } else {
                    Fallible::new_fatal(vec![
                        TranslateError::new(&format!("unknown item `{id}`")),
                    ])
                }
            },

            node::ExpressionKind::PointerTake(target) =>
                return self.translate_expression(target)?
                    .map(|target| {
                        let ptr = target.consume_pointer(self.target_mut());
                        Value::new_read_only(move |_| ptr).into()
                    }),

            node::ExpressionKind::PointerDereference(ptr) => {
                // Assuming a `PointerDereference` in this position is a read.
                // A write would be inside an assignment statement instead.
                let type_check::Type::Pointer(pointee_ty) = &ptr.data else {
                    panic!("dereferencing non-pointer in translation");
                };
                self.translate_expression(ptr)?
                    .map(|ptr| {
                        let ptr = ptr.consume_read(self.target_mut());
                        let ty = pointee_ty.to_ir_type();
                        Value::new_read_write_pointer(
                            move |target| target.add_instruction(
                                Instruction::new(ir::InstructionKind::ReadMemory {
                                    address: ptr,
                                    ty,
                                })
                            ),
                            move |target, v| target.add_void_instruction(
                                Instruction::new(ir::InstructionKind::WriteMemory {
                                    address: ptr,
                                    value: v,
                                })
                            ),

                            // To get a pointer to the dereferenced item, we can just return the
                            // pointer itself
                            move |_| ptr,
                        ).into()
                    })
            }

            node::ExpressionKind::FieldAccess { target, field } => {
                // Get index of field being accessed
                let type_check::Type::Struct(fields) = target.data.desugar() else {
                    unreachable!("access on non-struct")
                };
                let Some((index, (_, ty))) = fields.iter().enumerate().find(|(_, (name, _))| name == field) else {
                    unreachable!("missing field {field}")
                };
                let ty = ty.clone();

                self.translate_expression(target)?
                    .map(|strct| {
                        // Get pointer to structure
                        let ptr = strct.consume_pointer(self.target_mut());

                        // Calculate an index into the structure
                        let offset_var = self.target_mut().add_instruction(
                            Instruction::new(ir::InstructionKind::FieldOffset {
                                ty: target.data.desugar().to_ir_type(),
                                index,
                            })
                        );
                        let field_address = self.target_mut().add_instruction(
                            Instruction::new(ir::InstructionKind::Add(ptr, offset_var))
                        );

                        Value::new_read_write_pointer(
                            move |target| target.add_instruction(
                                Instruction::new(ir::InstructionKind::ReadMemory {
                                    address: field_address,
                                    ty: ty.to_ir_type(),
                                })
                            ),
                            move |target, value| target.add_void_instruction(
                                Instruction::new(ir::InstructionKind::WriteMemory {
                                    address: field_address,
                                    value,
                                })
                            ),
                            move |_| field_address,
                        ).into()
                    })
            }

            node::ExpressionKind::BitwiseNot(v) => {
                self.translate_expression(v)?
                    .map(|v| {
                        let v = v.consume_read(self.target_mut());
                        Value::new_read_only(move |target| target.add_instruction(
                            Instruction::new(ir::InstructionKind::BitwiseNot(v))
                        )).into()
                    })
            }

            // TODO: what about other types?
            node::ExpressionKind::Integer(i, base) => {
                let i = i.clone();
                let base = *base;
                Fallible::new_ok(
                    Value::new_read_only(move |target|
                        target.add_constant(ir::ConstantValue::U16(u16::from_str_radix(&i, base).unwrap()))
                    )
                )
            },

            node::ExpressionKind::Boolean(b) => {
                let b = *b;
                Fallible::new_ok(
                    Value::new_read_only(move |target|
                        target.add_constant(ir::ConstantValue::Boolean(b))
                    )
                )
            },

            node::ExpressionKind::ArithmeticBinOp(op, l, r) => {
                let ir_kind = match op {
                    node::ArithmeticBinOp::Add => ir::InstructionKind::Add,
                    node::ArithmeticBinOp::Subtract => ir::InstructionKind::Subtract,
                    node::ArithmeticBinOp::Multiply => ir::InstructionKind::Multiply,

                    node::ArithmeticBinOp::BitwiseAnd => ir::InstructionKind::BitwiseAnd,
                    node::ArithmeticBinOp::BitwiseXor => ir::InstructionKind::BitwiseXor,
                    node::ArithmeticBinOp::BitwiseOr => ir::InstructionKind::BitwiseOr,

                    node::ArithmeticBinOp::LeftShift => ir::InstructionKind::LeftShift,
                    node::ArithmeticBinOp::RightShift => ir::InstructionKind::RightShift,
                };

                let parts = self.translate_expression(l)?
                    .combine(self.translate_expression(r)?);

                // Is this pointer arithmetic?
                // If so, we multiply the RHS by the size of the pointee type
                // (Currently pointer arithmetic is not commutative - `pointer + integral`)
                if let type_check::Type::Pointer(pointee) = &l.data {
                    let pointee = pointee.clone();
                    parts
                        .map(|(l, r)| {
                            Value::new_read_only(move |target| {
                                let l = l.consume_read(target);
                                let r = r.consume_read(target);
                                let size = target.add_instruction(
                                    ir::Instruction::new(ir::InstructionKind::WordSize(pointee.to_ir_type()))
                                );
                                let scaled_r = target.add_instruction(
                                    ir::Instruction::new(ir::InstructionKind::Multiply(r, size))
                                );
    
                                target.add_instruction(
                                    ir::Instruction::new(ir_kind(l, scaled_r))
                                )
                            }).into()
                        })
                } else {
                    parts
                        .map(|(l, r)| {
                            Value::new_read_only(move |target| {
                                let l = l.consume_read(target);
                                let r = r.consume_read(target);    
                                target.add_instruction(
                                    ir::Instruction::new(ir_kind(l, r))
                                )
                            }).into()
                        })
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
                        Value::new_read_only(move |target| {
                            let l = l.consume_read(target);
                            let r = r.consume_read(target);
                            target.add_instruction(
                                ir::Instruction::new(instr(l, r))
                            )
                        }).into()
                    }),

            node::ExpressionKind::Call { target, arguments } => {
                let target = self.translate_expression(target)?;

                let arguments = arguments.iter()
                    .map(|arg| self.translate_expression(arg))
                    .collect::<Fallible<Vec<_>, _>>();
                
                target
                    .combine(arguments)
                    .map(|(call_target, arguments)| {
                        // Even if the result isn't used, we have to execute a function call,
                        // because it might have side effects.
                        // As a result, the `add_instruction` happens outside the `Value` operation.
                        let call_target = call_target.consume_read(self.target_mut());
                        let arguments = arguments.into_iter()
                            .map(|arg| arg.unwrap_or(Value::new_read_only(|_| VariableId::ERROR)).consume_read(self.target_mut()))
                            .collect();    
                        let result = self.target_mut().add_instruction(
                            ir::Instruction::new(ir::InstructionKind::Call {
                                target: call_target,
                                arguments,
                            })
                        );

                        Value::new_read_only(move |_| result).into()
                    })
            },

            node::ExpressionKind::Index { target, index } => {
                let type_check::Type::Array(pointee_ty, _) = &target.data else {
                    panic!("translating index to non-array")
                };
                let pointee_ty = pointee_ty.clone();

                self.generate_array_element_pointer_expression(target, index)?
                    .map(|v| {
                        let v = v.consume_read(self.target_mut());
                        Value::new_read_write(
                            move |target| {
                                target.add_instruction(
                                    Instruction::new(ir::InstructionKind::ReadMemory {
                                        address: v,
                                        ty: pointee_ty.to_ir_type()
                                    })
                                )
                            },
                            move |target, value| {
                                target.add_void_instruction(
                                    Instruction::new(ir::InstructionKind::WriteMemory {
                                        address: v,
                                        value,
                                    })
                                )
                            },
                        ).into()
                    })
            }

            node::ExpressionKind::Cast(value, ty) => {
                self.translate_expression(value)?.map(|source| {
                    let source = source.consume_read(self.target_mut());
                    let source_ty = self.func.get_variable_type(source);
                    let target_ty = ty.to_ir_type();

                    if source_ty.is_reinterpret_castable_to(&target_ty) {
                        Value::new_read_only(move |target| target.add_instruction(Instruction::new(ir::InstructionKind::CastReinterpret {
                            value: source,
                            ty: target_ty,
                        }))).into()
                    } else {
                        panic!("cannot cast {source_ty} to {target_ty}")
                    }
                })
            }

            node::ExpressionKind::Sizeof(ty) => {
                let ty = ty.clone();
                Fallible::new_ok(
                    Value::new_read_only(move |target| target.add_instruction(
                        Instruction::new(ir::InstructionKind::WordSize(ty.to_ir_type()))
                    ))
                )
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

    /// Given a target expression of type [type_check::Type::Array], and an index expression of
    /// integral type, generates an expression which evaluates to a pointer to the given element
    /// of the target array.
    /// 
    /// This can then be used with [ir::InstructionKind::ReadMemory] or
    /// [ir::InstructionKind::WriteMemory] to retrieve or update the array element.
    fn generate_array_element_pointer_expression(
        &mut self,
        target: &node::Expression<ExpressionData, Type>,
        index: &node::Expression<ExpressionData, Type>
    ) -> Fallible<MaybeFatal<Value>, TranslateError> {
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
