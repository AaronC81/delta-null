use std::{collections::HashMap, fmt::Display, error::Error};

use delta_null_lang_backend::ir::{Module, FunctionBuilder, Local, LocalId, BasicBlockBuilder, VariableId, self, Instruction};

use crate::{node::{TopLevelItem, TopLevelItemKind, self, Type}, fallible::{Fallible, MaybeFatal}};

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

    pub fn translate_item(&mut self, item: &TopLevelItem) -> Fallible<MaybeFatal<()>, TranslateError> {
        match &item.kind {
            TopLevelItemKind::FunctionDefinition { name, return_type, body } => {
                // Setup
                let mut func_trans = FunctionTranslator::new(
                    FunctionBuilder::new(name),
                );
                func_trans.populate_locals(body)?;

                // Translate
                let (_, start_block) = func_trans.func.new_basic_block();
                func_trans.target = Some(start_block);
                func_trans.translate_statement(body)?;
                func_trans.target.unwrap().finalize();

                // Add to module
                let func = func_trans.func.finalize();
                self.module.functions.push(func);

                Fallible::new_ok(())
            },
        }
    }

    pub fn finalize(self) -> Module {
        self.module
    }
}

/// Translates the contents of a function into an IR function, using the [FunctionBuilder]
/// interface.
pub struct FunctionTranslator {
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
}

impl FunctionTranslator {
    pub fn new(func: FunctionBuilder) -> Self {
        Self {
            func,
            locals: HashMap::new(),
            target: None,
        }
    }

    /// Builds a mapping of local variables to their IR [LocalId]s.
    /// 
    /// Call this only once, and before doing any translation.
    #[must_use]
    pub fn populate_locals(&mut self, stmt: &node::Statement) -> Fallible<MaybeFatal<()>, TranslateError> {
        let mut result = Fallible::new_ok(());

        match &stmt.kind {
            // We're looking for these!
            node::StatementKind::VariableDeclaration { name, ty, .. } => {
                let ty = self.node_type_to_ir_type(ty)?.propagate(&mut result);
                let id = self.func.new_local(name, ty);
                self.locals.insert(name.to_owned(), id);
            },

            // Contain other statements, so recursed into
            node::StatementKind::Block { body, .. } => {
                for s in body {
                    self.populate_locals(&s).propagate(&mut result);
                }
            },
            node::StatementKind::Loop(body) => {
                self.populate_locals(&body).propagate(&mut result);
            },
            node::StatementKind::If { body, .. } => {
                self.populate_locals(&body).propagate(&mut result);
            }

            // Nothing to do
            node::StatementKind::Return(_)
            | node::StatementKind::Expression(_)
            | node::StatementKind::Assignment { .. } => (),
        }

        result
    }

    /// Translates a parsed language statement into a set of IR instructions.
    #[must_use]
    pub fn translate_statement(&mut self, stmt: &node::Statement) -> Fallible<MaybeFatal<()>, TranslateError> {
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

            node::StatementKind::VariableDeclaration { name, ty, value } => {
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

            node::StatementKind::Assignment { name, value } => {
                if let Some(local) = self.locals.get(name).copied() {
                    self.translate_expression(value)?
                        .map(|v| {
                            self.target.as_mut().unwrap().add_void_instruction(
                                ir::Instruction::new(ir::InstructionKind::WriteLocal(local, v))
                            );
                            ()
                        });
                } else {
                    return Fallible::new_fatal(vec![
                        TranslateError::new(&format!("unknown item `{name}`")),
                    ])
                }
            }

            node::StatementKind::Return(value) => {
                if let Some(value) = value {
                    return self.translate_expression(value)?
                        .map(|v| {
                            self.target.as_mut().unwrap().add_terminator(
                                ir::Instruction::new(ir::InstructionKind::Return(Some(v)))
                            ).into()
                        });
                } else {
                    self.target.as_mut().unwrap().add_terminator(ir::Instruction::new(ir::InstructionKind::Return(None)));
                }
            },

            node::StatementKind::Loop(body) => {
                let (new_id, new_block) = self.func.new_basic_block();
                self.target_mut().add_terminator_if_none(ir::Instruction::new(ir::InstructionKind::Branch(new_id)));
                self.replace_target(new_block);
                let errors = self.translate_statement(&body)?;
                
                // Add infinite-looping terminator
                self.target_mut().add_terminator_if_none(ir::Instruction::new(ir::InstructionKind::Branch(new_id)));

                // Create block for following statements
                // (For when we have `break`!)
                let (_, cont_block) = self.func.new_basic_block();
                self.replace_target(cont_block);

                return errors.map(|f| f.into());
            },

            node::StatementKind::If { condition, body } => {
                let mut errors = Fallible::new(());

                let condition = self.translate_expression(condition)?
                    .propagate(&mut errors);
                
                // Create blocks for truth
                let (true_id, mut true_block) = self.func.new_basic_block();
                let (cont_id, cont_block) = self.func.new_basic_block();

                // Set up conditional branch
                self.target_mut().add_terminator_if_none(Instruction::new(ir::InstructionKind::ConditionalBranch {
                    condition,
                    true_block: true_id,
                    false_block: cont_id,
                }));

                // Populate true block
                self.replace_target(true_block);
                self.translate_statement(&body)?.propagate(&mut errors);
                self.target_mut().add_terminator_if_none(Instruction::new(ir::InstructionKind::Branch(cont_id)));

                // Replace target with continuation block
                self.replace_target(cont_block);

                return errors.map(|f| f.into());
            }
        }

        Fallible::new_ok(())
    }

    /// Translates an expression into a set of IR instructions, and return the [VariableId]
    /// describing the final result of the expression.
    #[must_use]
    pub fn translate_expression(&mut self, expr: &node::Expression) -> Fallible<MaybeFatal<VariableId>, TranslateError> {
        match &expr.kind {
            node::ExpressionKind::Identifier(id) => {
                if let Some(local) = self.locals.get(id).copied() {
                    Fallible::new_ok(
                        self.target_mut().add_instruction(
                            ir::Instruction::new(ir::InstructionKind::ReadLocal(local))
                        )
                    )
                } else {
                    Fallible::new_fatal(vec![
                        TranslateError::new(&format!("unknown item `{id}`")),
                    ])
                }
            },

            // TODO: what about other types?
            node::ExpressionKind::Integer(i) => Fallible::new_ok(
                self.target_mut().add_constant(ir::ConstantValue::U16(i.parse().unwrap()))
            ),

            node::ExpressionKind::Add(l, r) =>
                self.translate_expression(&l)?
                    .combine(self.translate_expression(&r)?)
                    .map(|(l, r)|
                        self.target_mut().add_instruction(
                            ir::Instruction::new(ir::InstructionKind::Add(l, r))
                        ).into()),

            node::ExpressionKind::Equals(l, r) =>
            self.translate_expression(&l)?
                .combine(self.translate_expression(&r)?)
                .map(|(l, r)|
                    self.target_mut().add_instruction(
                        ir::Instruction::new(ir::InstructionKind::Equals(l, r))
                    ).into()),
        }
    }

    /// Translates a [Type] to an [ir::Type].
    #[must_use]
    pub fn node_type_to_ir_type(&self, ty: &Type) -> Fallible<MaybeFatal<ir::Type>, TranslateError> {
        match &ty.kind {
            node::TypeKind::Name(t) => match t.as_ref() {
                "u16" => Fallible::new_ok(ir::Type::UnsignedInteger(ir::IntegerSize::Bits16)),
                "i16" => Fallible::new_ok(ir::Type::SignedInteger(ir::IntegerSize::Bits16)),
                "bool" => Fallible::new_ok(ir::Type::Boolean),
                _ => Fallible::new_fatal(vec![
                    TranslateError::new(&format!("unknown type `{t}`")),
                ]),
            }
            node::TypeKind::Void => Fallible::new_ok(ir::Type::Void),
        }
    }

    /// Gets a reference to the current basic block where instructions are being generated.
    #[must_use]
    pub fn target_mut(&mut self) -> &mut BasicBlockBuilder {
        self.target.as_mut().unwrap()
    }

    /// Finalises the current basic block, and replaces it with a different one.
    pub fn replace_target(&mut self, new: BasicBlockBuilder) {
        let old_target = self.target.take().unwrap();
        old_target.finalize();
        self.target = Some(new);
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
        write!(f, "tokenizer error: {}", self.description)
    }
}
impl Error for TranslateError {}
