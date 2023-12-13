use std::{collections::HashMap, fmt::Display, error::Error};

use delta_null_lang_backend::ir::{Module, FunctionBuilder, Local, LocalId, BasicBlockBuilder, VariableId, self, Instruction};

use crate::{node::{TopLevelItem, TopLevelItemKind, self, Type}, fallible::{Fallible, MaybeFatal}};

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
            TopLevelItemKind::FunctionDefinition { name, body } => {
                // Setup
                let mut func_trans = FunctionTranslator::new(
                    FunctionBuilder::new(name),
                );
                func_trans.populate_locals(body)?;

                // Translate
                let (_, mut start_block) = func_trans.func.new_basic_block();
                func_trans.translate_statement(body, &mut start_block)?;
                start_block.finalize();

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

pub struct FunctionTranslator {
    func: FunctionBuilder,
    locals: HashMap<String, LocalId>,
}

impl FunctionTranslator {
    pub fn new(func: FunctionBuilder) -> Self {
        Self {
            func,
            locals: HashMap::new(),
        }
    }

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

    #[must_use]
    pub fn translate_statement(&self, stmt: &node::Statement, target: &mut BasicBlockBuilder) -> Fallible<MaybeFatal<()>, TranslateError> {
        match &stmt.kind {
            node::StatementKind::Block { body, .. } => {
                for s in body {
                    self.translate_statement(s, target)?;
                }
            },
            
            node::StatementKind::Expression(e) => {
                self.translate_expression(e, target)?;
            },

            node::StatementKind::VariableDeclaration { name, ty, value } => {
                // Creating the local was already handled by `populate_locals`.
                let local = *self.locals.get(name).unwrap();

                // If there's an initial value, generate its assignment here.
                if let Some(value) = value {
                    return self.translate_expression(value, target)?
                        .map(|v| {
                            target.add_void_instruction(
                                ir::Instruction::new(ir::InstructionKind::WriteLocal(local, v))
                            );
                            ().into()
                        });
                }
            }

            node::StatementKind::Assignment { name, value } => {
                if let Some(local) = self.locals.get(name) {
                    self.translate_expression(value, target)?
                        .map(|v| {
                            target.add_void_instruction(
                                ir::Instruction::new(ir::InstructionKind::WriteLocal(*local, v))
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
                    return self.translate_expression(value, target)?
                        .map(|v| {
                            target.add_terminator(
                                ir::Instruction::new(ir::InstructionKind::Return(Some(v)))
                            ).into()
                        });
                } else {
                    target.add_terminator(ir::Instruction::new(ir::InstructionKind::Return(None)));
                }
            },

            node::StatementKind::Loop(body) => {
                let (new_id, mut new_block) = self.func.new_basic_block();
                let errors = self.translate_statement(&body, &mut new_block)?;

                // Add jump from previous block to our new one
                target.add_terminator(ir::Instruction::new(ir::InstructionKind::Branch(new_id)));
                
                // Add infinite-looping terminator
                new_block.add_terminator(ir::Instruction::new(ir::InstructionKind::Branch(new_id)));
                new_block.finalize();

                return errors.map(|f| f.into());
            },

            node::StatementKind::If { condition, body } => {
                let mut errors = Fallible::new(());

                let condition = self.translate_expression(condition, target)?
                    .propagate(&mut errors);
                
                // Create block for truth
                let (true_id, mut true_block) = self.func.new_basic_block();
                self.translate_statement(&body, &mut true_block)?.propagate(&mut errors);

                // Create block for following statements
                let (cont_id, mut cont_block) = self.func.new_basic_block();

                // Set up jump terminators
                target.add_terminator(Instruction::new(ir::InstructionKind::ConditionalBranch {
                    condition,
                    true_block: true_id,
                    false_block: cont_id,
                }));
                true_block.add_terminator(Instruction::new(ir::InstructionKind::Branch(cont_id)));

                // TODO: this will break, because other blocks continue using the other as a target
            }
        }

        Fallible::new_ok(())
    }

    #[must_use]
    pub fn translate_expression(&self, expr: &node::Expression, target: &mut BasicBlockBuilder) -> Fallible<MaybeFatal<VariableId>, TranslateError> {
        match &expr.kind {
            node::ExpressionKind::Identifier(id) => {
                if let Some(local) = self.locals.get(id) {
                    Fallible::new_ok(
                        target.add_instruction(
                            ir::Instruction::new(ir::InstructionKind::ReadLocal(*local))
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
                target.add_constant(ir::ConstantValue::U16(i.parse().unwrap()))
            ),

            node::ExpressionKind::Add(l, r) =>
                self.translate_expression(&l, target)?
                    .combine(self.translate_expression(&r, target)?)
                    .map(|(l, r)|
                        target.add_instruction(
                            ir::Instruction::new(ir::InstructionKind::Add(l, r))
                        ).into())
        }
    }

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
        }
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
