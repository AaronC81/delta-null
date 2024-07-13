//! Implements the core compilation logic.

use delta_null_core_assembler::{AssemblyItem, AssemblyOperand, LabelAccess};
use delta_null_core_instructions::{InstructionOpcode, GPR, SPR};
use delta_null_lang_backend::ir::{self, LocalRepository, Type, VariableRepository};

use crate::asm::assemble;

use super::FunctionGenerator;

impl<'f, 'l> FunctionGenerator<'f, 'l> {
    /// Generates and returns the Assembly instructions to implement an entire basic block.
    /// 
    /// If the block contains phi nodes, they are not considered here - these are handled by a
    /// second pass after initial generation.
    pub fn ir_block_to_assembly(&self, block: &ir::BasicBlock) -> Vec<AssemblyItem> {
        let mut buffer = vec![];
        let mut skip_amount = 0;
        for (i, stmt) in block.statements.iter().enumerate() {
            // Statement translation might have told us to skip a particular number of following
            // statements, because it's been able to optimise them out
            if skip_amount > 0 {
                skip_amount -= 1;
                continue;
            }

            let after = &block.statements[i+1..];
            skip_amount = self.ir_statement_to_assembly(&mut buffer, stmt, after);
        }

        // Add label to first instruction of the block
        buffer[0].labels.push(self.basic_block_label(&block.id));

        buffer
    }

    /// Generates the Assembly instructions to implement a single IR statement.
    /// 
    /// Returns the number of statements after `stmt` to skip code generation for, in case this has
    /// been able to optimise them out. (If it hasn't, this should be 0.)
    pub fn ir_statement_to_assembly(&self, buffer: &mut Vec<AssemblyItem>, stmt: &ir::Statement, after: &[ir::Statement]) -> usize {
        match &stmt.instruction.kind {
            ir::InstructionKind::Begin => {
                // Nothing to do. We insert our preamble manually
            }

            ir::InstructionKind::Constant(c) => {
                let Some(reg) = self.variable_reg(stmt.result.unwrap()) else { return 0 };
                
                let imm = match c {
                    ir::ConstantValue::U16(v) => *v,
                    ir::ConstantValue::I16(v) => *v as u16,
                    ir::ConstantValue::Boolean(b) => if *b { 1 } else { 0 },
                };

                buffer.push(AssemblyItem::new_word_put(reg, imm.into()));
            },

            ir::InstructionKind::CastReinterpret { value, ty } => {
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };

                let source_ty = &self.func.get_variable(*value).ty;
                if source_ty.word_size() != ty.word_size() {
                    panic!("cannot `CastReinterpret` {source_ty} to {ty} because the types are different sizes");
                }

                let value = self.generate_read(buffer, *value);
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Mov,
                    &[result.into(), value.into()],
                ));
            }

            ir::InstructionKind::FunctionReference { name, .. } 
            | ir::InstructionKind::DataReference { name, .. } => {
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };
                buffer.push(AssemblyItem::new_word_put(
                    result,
                    AssemblyOperand::Label { name: name.clone(), access: None }
                ))
            }

            ir::InstructionKind::ReadLocal(l) => {
                let local_offset = self.local_access_map()[l];
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };

                let local_ty = &self.func.get_local(*l).ty;
                if !local_ty.is_scalar() {
                    panic!("cannot `ReadLocal` a non-scalar type: `{local_ty}`")
                }

                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Spread,
                    &[result.into(), local_offset.into()] // TODO: limits local offsets to 16
                ));
            },

            ir::InstructionKind::WriteLocal(l, v) => {
                let v = self.generate_read(buffer, *v);
                let local_offset = self.local_access_map()[l];

                let local_ty = &self.func.get_local(*l).ty;
                if !local_ty.is_scalar() {
                    panic!("cannot `WriteLocal` a non-scalar type: `{local_ty}`")
                }

                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Spwrite,
                    &[local_offset.into(), v.into()] // TODO: limits local offsets to 16
                ));
            },

            ir::InstructionKind::AddressOfLocal(l) => {
                let local_offset = self.local_access_map()[l];
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };

                // Calculate sp + offset
                // We need a random register to put the offset in - save it onto the stack
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Movso,
                    &[result.into(), SPR::SP.into()]
                ));
                let offset_reg = if result == GPR::R0 { GPR::R1 } else { GPR::R0 };
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Push,
                    &[offset_reg.into()]
                ));
                buffer.push(AssemblyItem::new_word_put(offset_reg, AssemblyOperand::Immediate(local_offset)));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Add,
                    &[result.into(), offset_reg.into()]
                ));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Pop,
                    &[offset_reg.into()]
                ));
            }

            ir::InstructionKind::WriteMemory { address, value } => {
                let address = self.generate_read(buffer, *address);
                let value = self.generate_read(buffer, *value);
                
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Write,
                    &[address.into(), value.into()],
                ));
            }

            ir::InstructionKind::ReadMemory { address, ty } => {
                let address = self.generate_read(buffer, *address);
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };

                if ty.word_size() != 1 {
                    panic!("reading non-word-sized types nyi");
                }

                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Read,
                    &[result.into(), address.into()],
                ));
            }

            ir::InstructionKind::Add(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Add, true),
            ir::InstructionKind::Subtract(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Sub, false),
            ir::InstructionKind::Multiply(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Mul, false),
            ir::InstructionKind::BitwiseAnd(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::And, false),
            ir::InstructionKind::BitwiseXor(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Xor, false),
            ir::InstructionKind::LeftShift(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Shl, false),
            ir::InstructionKind::RightShift(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Shr, false),
            ir::InstructionKind::BitwiseOr(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Or, false),

            ir::InstructionKind::BitwiseNot(v) => {
                let v = self.generate_read(buffer, *v);
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };

                // Move into result register, then `NOT`
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Mov,
                    &[result.into(), v.into()]
                ));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Not,
                    &[result.into()]
                ));
            }

            ir::InstructionKind::BooleanNot(v) => {
                // Most efficient way to do this is a bit fiddly:
                //   1. Zero the result register
                //   2. Check if the boolean is false, with `eqz`
                //      This effectively sets `EF.cond` to the inverse of the boolean
                //   3. Use `bitset` to write `EF.cond` into the LSB of the result register

                let v = self.generate_read(buffer, *v);
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };

                // Zero result register
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Xor,
                    &[result.into(), result.into()]
                ));

                // Load inverse of boolean into condition flag
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Eqz,
                    &[v.into()]
                ));

                // Write condition flag into result register
                // `bitset` expects a bit position - we just zeroed the result so can recycle that
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Bitset,
                    &[result.into(), result.into()]
                ));
            }

            ir::InstructionKind::BooleanAnd(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Booland, false),

            ir::InstructionKind::Equals(l, r) => {
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };
                let l = self.generate_read(buffer, *l);
                let r = self.generate_read(buffer, *r);

                // Do comparison - this puts result in `ef`
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Eq,
                    &[l.into(), r.into()]
                ));

                // Convert to an object
                if let Some(skips) = self.insert_comparison_value_conversion(buffer, result, stmt, after) {
                    return skips;
                }
            },

            ir::InstructionKind::GreaterThan(l, r) => {
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };
                let l = self.generate_read(buffer, *l);
                let r = self.generate_read(buffer, *r);

                // Do comparison - this puts result in `ef`
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Gt,
                    &[l.into(), r.into()]
                ));

                // Convert to an object
                if let Some(skips) = self.insert_comparison_value_conversion(buffer, result, stmt, after) {
                    return skips;
                }
            },

            ir::InstructionKind::GreaterThanOrEquals(l, r) => {
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };
                let l = self.generate_read(buffer, *l);
                let r = self.generate_read(buffer, *r);

                // Do comparison - this puts result in `ef`
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Gteq,
                    &[l.into(), r.into()]
                ));

                // Convert to an object
                if let Some(skips) = self.insert_comparison_value_conversion(buffer, result, stmt, after) {
                    return skips;
                }
            },

            ir::InstructionKind::LessThan(l, r) => {
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };
                let l = self.generate_read(buffer, *l);
                let r = self.generate_read(buffer, *r);

                // The architecture doesn't have a less-than instruction, for reasons which probably
                // seemed great at the time. Instead, invert the result of a `Gteq`.
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Gteq,
                    &[l.into(), r.into()]
                ));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Inv,
                    &[]
                ));

                // Convert to an object
                if let Some(skips) = self.insert_comparison_value_conversion(buffer, result, stmt, after) {
                    return skips;
                }
            },

            ir::InstructionKind::LessThanOrEquals(l, r) => {
                let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return 0 };
                let l = self.generate_read(buffer, *l);
                let r = self.generate_read(buffer, *r);

                // The architecture doesn't have a less-than-or-equals instruction either.
                // Invert `Gt` instead.
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Gt,
                    &[l.into(), r.into()]
                ));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Inv,
                    &[]
                ));

                // Convert to an object
                if let Some(skips) = self.insert_comparison_value_conversion(buffer, result, stmt, after) {
                    return skips;
                }
            },

            ir::InstructionKind::Call { target, arguments } =>
                self.call_statement_to_assembly(buffer, stmt, *target, arguments),

            ir::InstructionKind::WordSize(ty) => {
                let Some(reg) = self.variable_reg(stmt.result.unwrap()) else { return 0 };
                buffer.push(AssemblyItem::new_word_put(reg, (ty.word_size() as u16).into()));
            }

            ir::InstructionKind::FieldOffset { ty, index } => {
                let Type::Struct(fields) = ty else {
                    panic!("`FieldAccess` type must be a `Struct`");
                };

                let offset: usize = fields[0..*index].iter().map(|t| t.word_size()).sum();
                if let Some(reg) = self.variable_reg(stmt.result.unwrap()) {
                    buffer.push(AssemblyItem::new_word_put(reg, (offset as u16).into()));
                }
            }

            ir::InstructionKind::InlineAssembly(contents) => {
                buffer.extend(assemble(contents));
            }

            ir::InstructionKind::Return(ret) => {
                if let Some(ret) = *ret {
                    // EABI says to use r0-r1 to pass return value
                    let ret = self.generate_read(buffer, ret);
                    buffer.push(AssemblyItem::new_instruction(
                        InstructionOpcode::Mov,
                        &[GPR::R0.into(), ret.into()]
                    ));
                }

                // This is going to exit this function, so we need to insert postamble
                self.insert_postamble(buffer);

                // Actually return
                buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Ret, &[]));
            }

            ir::InstructionKind::Branch(dest) => {
                // TODO: offset-branch might not always reach sufficiently far, but dealing with
                // register allocation for this is complicated!
                // This is probably fine for most trivial stuff, for now.
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Jmpoff,
                    &[AssemblyOperand::Label {
                        name: self.basic_block_label(dest),
                        access: Some(LabelAccess::Offset),
                    }]
                ));
            },

            ir::InstructionKind::ConditionalBranch { condition, true_block, false_block } => {
                let condition = self.generate_read(buffer, *condition);

                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Eqz,
                    &[condition.into()]
                ));

                // We used `eqz`, so conditional-jump to the false block if that condition was met,
                // because 0 is false
                self.insert_bidirectional_branch_instructions(buffer, *false_block, *true_block);
            }

            ir::InstructionKind::Jump(dest) => {
                let dest = self.generate_read(buffer, *dest);

                // Only current application is to exit the function, so insert postamble like
                // `Return` does
                self.insert_postamble(buffer);

                // Architecture has no `jmp X` instruction because `movsi ip, X` fills the same role
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Movsi,
                    &[
                        SPR::IP.into(),
                        dest.into(),
                    ])
                );
            }

            ir::InstructionKind::Phi { .. } => {
                // There's nothing to do here - `Phi` is generated on the blocks which call into
                // this, not in the beginning of the block where the node is placed.
            },

            ir::InstructionKind::Unreachable => {
                // Something's gone wrong if we hit this! This instruction's behaviour is undefined,
                // but we'll be nice and generate a halt, so that case is detectable.
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Hlt,
                    &[]
                ));
            }
        }

        0 // Assume no optimisation, unless a branch of the `match` returns before
    }
}