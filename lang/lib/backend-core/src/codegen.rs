use std::collections::{HashMap, HashSet};

use delta_null_core_assembler::{AssemblyItem, AssemblyOperand, LabelAccess};
use delta_null_core_instructions::{GeneralPurposeRegister, GPR, InstructionOpcode, SPR};
use delta_null_lang_backend::ir::{self, BasicBlockId, Function, InstructionKind, LocalId, LocalRepository, Type, VariableId, VariableRepository};

use crate::{asm::assemble, reg_alloc::Allocation};

/// Handles code generation for a single function.
pub struct FunctionGenerator<'f> {
    func: &'f Function,
    allocations: HashMap<VariableId, Allocation>,

    is_leaf: bool,
}

impl<'f> FunctionGenerator<'f> {
    pub fn new(func: &'f Function, allocations: HashMap<VariableId, Allocation>, is_leaf: bool) -> Self {
        Self { func, allocations, is_leaf }
    }

    /// Converts this entire function to a complete list of Assembly instructions.
    pub fn to_assembly(&self) -> Vec<AssemblyItem> {
        // Assumes that first ID'd block is the entry point of the function
        let mut sorted_block_ids = self.func.blocks.keys().collect::<Vec<_>>();
        sorted_block_ids.sort();

        // Generate code for each block
        let mut blocks = HashMap::new();
        for id in &sorted_block_ids {
            blocks.insert(**id, self.ir_block_to_assembly(&self.func.blocks[id]));
        }

        // Run phi step
        self.insert_phi_instructions(&mut blocks);

        // Concatenate blocks into a buffer
        let mut buffer = vec![];
        for id in &sorted_block_ids {
            buffer.extend(blocks.remove(id).unwrap());
        }

        // Prepend preamble
        self.prepend_preamble(&mut buffer);

        // Add label to very beginning, so other functions can call this
        buffer[0].labels.push(self.func.name.clone());

        buffer
    }

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
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Add),
            ir::InstructionKind::Subtract(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Sub),
            ir::InstructionKind::Multiply(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Mul),
            ir::InstructionKind::BitwiseAnd(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::And),
            ir::InstructionKind::BitwiseXor(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Xor),
            ir::InstructionKind::LeftShift(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Shl),
            ir::InstructionKind::RightShift(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Shr),
            ir::InstructionKind::BitwiseOr(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Or),

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

            ir::InstructionKind::Call { target, arguments } => {
                // Fetch result register. The call puts its result in `r0`, so we'll copy it here
                // later.
                //
                // This might be `None`, which means that the return value of the call was never
                // used. Unlike many instructions, we still need to perform codegen in this case,
                // because the call probably has side effects.
                let result = self.variable_reg(stmt.result.unwrap());

                // TODO
                // There's quite a lot which needs to be done to prepare for a function call:
                //   - Get the target, and move it away from the parameter-passing registers, which
                //     we need to set to specific values
                //   - Preserve any caller-saved registers which are in use
                //   - Shuffle parameters around so that they're in the correct registers for the
                //     call. This can be tricky in some scenarios (e.g. if parameter 1 was
                //     calculated into `r1` and parameter 2 in `r0`, you need to "swap" them.)
                //
                // This is tricky to get right - instead, TEMPORARILY take an easier approach.
                // We push the _entire set_ of (used) GPRs to the stack, which has two functions:
                //   - We can cherry-pick the registers we need back off this copy on the stack,
                //     without worrying about that swapping problem.
                //   - To preserve (most of) them for later. We don't want to restore over a
                //     function call's return value though, so we won't restore whichever register
                //     that gets stored in.
                //
                // This is inefficient, but easy to implement, and trivially correct.

                // Find the set of GPRs which this function uses
                let mut used_gprs = self.allocations.iter()
                    .filter_map(|(_, alloc)| match alloc {
                        Allocation::Register(r) => Some(*r),
                        _ => None
                    })
                    .collect::<HashSet<_>>() // unique
                    .into_iter()
                    .collect::<Vec<_>>();
                used_gprs.sort_by_key(|r| r.number());

                // Push all used GPRs
                let mut gpr_stack_offsets = HashMap::new();
                for reg in &used_gprs {
                    buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Push, &[(*reg).into()]));

                    // There's now one more GPR on the stack, so you need to access one offset
                    // deeper into the stack to retrieve the ones which were already pushed
                    for offset in gpr_stack_offsets.values_mut() {
                        *offset += 1;
                    }

                    // Insert new item, which is now at the top of the stack
                    gpr_stack_offsets.insert(reg, 0);
                }

                // Push parameters into registers
                let parameter_passing_registers = [GPR::R0, GPR::R1, GPR::R2, GPR::R3];
                if arguments.len() > parameter_passing_registers.len() {
                    panic!(
                        "too many parameters for EABI (found {} but maximum is {})",
                        arguments.len(), parameter_passing_registers.len(),
                    );
                }
                for (argument, reg) in arguments.iter().zip(parameter_passing_registers) {
                    let source = self.generate_read(buffer, *argument);

                    // Generate `spread` to grab the parameter off our stack copy
                    buffer.push(AssemblyItem::new_instruction(
                        InstructionOpcode::Spread,
                        &[reg.into(), gpr_stack_offsets[&source].into()],
                    ));
                }
                
                // Use `r4`, an arbitrarily-chosen (non-parameter-passing) register, to hold our
                // target
                let target = self.generate_read(buffer, *target);
                let true_target = GPR::R4;
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Spread,
                    &[true_target.into(), gpr_stack_offsets[&target].into()],
                ));

                // Call, and copy result (in `r0`) to result register
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Call,
                    &[true_target.into()],
                ));
                if let Some(result) = result {
                    buffer.push(AssemblyItem::new_instruction(
                        InstructionOpcode::Mov,
                        &[result.into(), GPR::R0.into()],
                    ));
                }

                // Restore registers
                for reg in used_gprs.iter().rev() {
                    // ...except result register
                    if Some(*reg) == result {
                        buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Spinc, &[]));
                    } else {
                        buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Pop, &[(*reg).into()]));
                    }
                }
            },

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

    /// Inserts instructions at the end of each basic block to fulfill phi instructions in other
    /// blocks.
    /// 
    /// Intended to execute as a post-processing step, after other block code has been generated.
    fn insert_phi_instructions(&self, buffers: &mut HashMap<BasicBlockId, Vec<AssemblyItem>>) {
        // Iterate over each basic block, and check if it starts with a phi
        for block in self.func.blocks.values() {
            let first_stmt = block.first_statement();
            if let InstructionKind::Phi { choices } = &first_stmt.instruction.kind {
                let Some(phi_result) = self.variable_reg(first_stmt.result.unwrap()) else { return };

                // It does contain a phi. Iterate over the possible incoming blocks, and insert a
                // move instruction to populate the phi's result from that block.
                // The phi instruction implementation must come _before_ the terminator - which
                // we'll assume is the last instruction (as it should be!).
                for (incoming_block_id, var) in choices {
                    let buffer = buffers.get_mut(incoming_block_id).unwrap();
                    let source = self.generate_read(buffer, *var);
                    buffer.insert(
                        buffer.len() - 1,
                        AssemblyItem::new_instruction(
                            InstructionOpcode::Mov,
                            &[
                                phi_result.into(),
                                source.into(),
                            ]
                        ),
                    );
                }
            }
        }
    }

    /// Prepends all code required for a function preamble to the instruction buffer, to be executed
    /// when the function is first entered.
    fn prepend_preamble(&self, buffer: &mut Vec<AssemblyItem>) {
        self.prepend_stack_allocation_instructions(buffer);
        self.prepend_callee_saved_register_save_instructions(buffer);
    }

    /// Inserts all code required for a graceful function postamble, before a return.
    fn insert_postamble(&self, buffer: &mut Vec<AssemblyItem>) {
        self.insert_stack_deallocation_instructions(buffer);
        self.insert_callee_saved_register_restore_instructions(buffer);
    }
    
    /// Inserts instructions at the beginning of an instruction buffer to allocate stack space for
    /// usage throughout the function, such as locals.
    fn prepend_stack_allocation_instructions(&self, buffer: &mut Vec<AssemblyItem>) {
        // Put the amount of stack space, negated, in R4 (which will have been preserved), then use
        // `spadd`
        if self.stack_space() > 0 {
            buffer.splice(0..0, [
                AssemblyItem::new_word_put(GPR::R4, AssemblyOperand::Immediate(-(self.stack_space() as i16) as u16)),
                AssemblyItem::new_instruction(
                    InstructionOpcode::Spadd,
                    &[GPR::R4.into()]
                )
            ]);
        }
    }

    /// Inserts instructions at the end of the instruction buffer to deallocate stack space, before
    /// a function returns.
    fn insert_stack_deallocation_instructions(&self, buffer: &mut Vec<AssemblyItem>) {
        // Put the amount of stack space in R4 (which will have been preserved), then use `spadd`
        if self.stack_space() > 0 {
            buffer.push(AssemblyItem::new_word_put(GPR::R4, AssemblyOperand::Immediate(self.stack_space() as u16)));
            buffer.push(AssemblyItem::new_instruction(
                InstructionOpcode::Spadd,
                &[GPR::R4.into()]
            ));
        }
    }

    const CALLEE_SAVED_GPRS: [GPR; 4] = [GPR::R4, GPR::R5, GPR::R6, GPR::R7];

    /// Inserts instructions at the end of the instruction buffer to save callee-saved registers,
    /// when a function is first called.
    fn prepend_callee_saved_register_save_instructions(&self, buffer: &mut Vec<AssemblyItem>) {
        let mut preservation_instructions = vec![];

        // Preserve callee-saved GPRs which we use
        for reg in Self::CALLEE_SAVED_GPRS.iter() {
            if self.should_preserve_callee_saved_register(*reg) {
                preservation_instructions.push(
                    AssemblyItem::new_instruction(
                        InstructionOpcode::Push,
                        &[(*reg).into()]
                    )
                );
            }
        }

        // Preserve RP - unless this is a leaf function, in which case it doesn't call anything, so
        // RP is definitely never trashed
        if !self.is_leaf {
            preservation_instructions.push(
                AssemblyItem::new_instruction(
                    InstructionOpcode::Movso,
                    &[GPR::R4.into(), SPR::RP.into()]
                )
            );
            preservation_instructions.push(
                AssemblyItem::new_instruction(
                    InstructionOpcode::Push,
                    &[GPR::R4.into()]
                )
            );
        }

        // Insert instructions into beginning of buffer
        buffer.splice(0..0, preservation_instructions);
    }

    /// Inserts instructions at the end of the instruction buffer to restore callee-saved registers,
    /// before a function returns.
    fn insert_callee_saved_register_restore_instructions(&self, buffer: &mut Vec<AssemblyItem>) {
        // Restore RP - unless this is a leaf function, in which case we never preserved it
        if !self.is_leaf {
            buffer.push(AssemblyItem::new_instruction(
                InstructionOpcode::Pop, &[GPR::R4.into()],
            ));
            buffer.push(AssemblyItem::new_instruction(
                InstructionOpcode::Movsi, &[SPR::RP.into(), GPR::R4.into()]
            ));
        }

        // Restore GPRs
        for reg in Self::CALLEE_SAVED_GPRS.iter().rev() {
            if self.should_preserve_callee_saved_register(*reg) {
                buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Pop, &[(*reg).into()]));
            }
        }
    }

    fn should_preserve_callee_saved_register(&self, reg: GeneralPurposeRegister) -> bool {
        // If we're going to be trashing this register ourselves, we need to save it
        if self.is_register_allocated(reg) {
            return true
        }
        
        // R4 is used for stack setup and RP saving.
        // If either of those two things are going to be happening, ensure it's reserved
        if reg == GPR::R4 && (self.stack_space() > 0 || !self.is_leaf) {
            return true
        }

        false
    }

    /// Inserts instructions to branch to a particular block if the condition flag is set, or a
    /// different block if it is not set.
    fn insert_bidirectional_branch_instructions(&self, buffer: &mut Vec<AssemblyItem>, set_block: BasicBlockId, unset_block: BasicBlockId) {
        buffer.push(AssemblyItem::new_instruction(
            InstructionOpcode::Cjmpoff,
            &[AssemblyOperand::Label {
                name: self.basic_block_label(&set_block), 
                access: Some(LabelAccess::Offset),
            }]
        ));
        buffer.push(AssemblyItem::new_instruction(
            InstructionOpcode::Jmpoff,
            &[AssemblyOperand::Label {
                name: self.basic_block_label(&unset_block), 
                access: Some(LabelAccess::Offset),
            }]
        ));
    }

    /// Inserts instructions to extract the comparison flag out of EF into a boolean variable, to
    /// complete generation of a comparison instruction like [InstructionKind::Equals].
    /// 
    /// May perform optimisations to avoid doing this if a branch instruction which uses the result
    /// of the test is detected. If so, returns the number of instructions to skip, which should be
    /// propagated in `ir_statement_to_assembly`. If [None], the value was generated into a variable
    /// like normal.
    #[must_use]
    fn insert_comparison_value_conversion(&self, buffer: &mut Vec<AssemblyItem>, result_gpr: GPR, stmt: &ir::Statement, after: &[ir::Statement]) -> Option<usize> {
        // Optimisation check! If this equality test:
        //   - Is followed by a `ConditionalBranch`
        //   - Has the result variable used only by that `ConditionalBranch`
        // then we can skip extracting `ef`'s condition bit into a variable, and just branch
        // right now.
        if let Some(InstructionKind::ConditionalBranch { condition, true_block, false_block })
            = after.first().map(|s| &s.instruction.kind)
        {
            if *condition == stmt.result.unwrap()
                && self.func.variable_references()[condition].len() == 1 // only used as that condition
            {
                self.insert_bidirectional_branch_instructions(buffer, *true_block, *false_block);

                // Skip codegen for the `ConditionalBranch` instruction
                return Some(1);
            }
        }

        // Copy entire `ef` register out into result
        buffer.push(AssemblyItem::new_instruction(
            InstructionOpcode::Movso,
            &[result_gpr.into(), SPR::EF.into()]
        ));

        // Mask out the condition bit (0x0002)
        // This is tricky, because we need to load the mask into a separate register, but
        // the allocator only gave us one!
        // Use `r0`, preserving through the stack, or `r1` in case `r0` happens to be our
        // result register!
        let mask_reg = if result_gpr == GPR::R0 { GPR::R1 } else { GPR::R0 };
        buffer.push(AssemblyItem::new_instruction(
            InstructionOpcode::Push,
            &[mask_reg.into()]
        ));
        buffer.push(AssemblyItem::new_word_put(mask_reg, AssemblyOperand::Immediate(0x0002)));
        buffer.push(AssemblyItem::new_instruction(
            InstructionOpcode::And,
            &[result_gpr.into(), mask_reg.into()]
        ));
        buffer.push(AssemblyItem::new_instruction(
            InstructionOpcode::Pop,
            &[mask_reg.into()]
        ));

        None
    }

    /// Calculates the amount of stack space required throughout this function.
    /// 
    /// Due to the lack of a base pointer, code generation allocates all stack space required for
    /// a function upfront.
    /// 
    /// At the borders between any two IR instructions within the same function, the stack pointer
    /// will not move. The stack pointer may change within the same instruction to implement
    /// register preservation or other actions, but the changes must not carry over to the next IR
    /// instruction.
    fn stack_space(&self) -> usize {
        self.func.locals.values()
            .map(|local| local.ty.word_size())
            .sum()
    }

    /// Builds a map describing the stack offsets used to access locals on the stack. An offset of
    /// 0 means that a local resides at the stack pointer, 1 means stack pointer + 1, etc.
    /// 
    /// This relies on the guarantee the stack pointer within a function does not move between
    /// IR instructions. 
    fn local_access_map(&self) -> HashMap<LocalId, u16> {
        // Create sorted list of IDs
        let mut sorted_local_ids = self.func.locals.keys().copied().collect::<Vec<_>>();
        sorted_local_ids.sort();

        // Iterate in order, build map
        let mut result = HashMap::new();
        let mut current_offset = 0;
        for id in sorted_local_ids {
            result.insert(id, current_offset);
            current_offset += self.func.locals[&id].ty.word_size() as u16;
        }
        result
    }

    /// Gets the register to use for a variable.
    /// 
    /// Returns [None] if the variable was never allocated by the allocator, typically because it is
    /// unused.
    fn variable_reg(&self, var: VariableId) -> Option<GeneralPurposeRegister> {
        match self.allocations.get(&var) {
            Some(Allocation::Register(r)) => Some(*r),
            Some(Allocation::Spill(_)) => todo!("spilling not yet supported"),
            None => None,
        }
    }

    /// Returns the instructions, if any, required to load a variable ID back into registers from a
    /// spill, so that it can be operated on.
    /// 
    /// Then, returns the general purpose register which contains the value.
    fn generate_read(&self, _buffer: &mut Vec<AssemblyItem>, var: VariableId) -> GeneralPurposeRegister {
        match &self.allocations[&var] {
            Allocation::Register(r) => *r,
            Allocation::Spill(_) => todo!("spilling not yet supported"),
        }
    }

    fn generate_arithmetic_bin_op(&self, buffer: &mut Vec<AssemblyItem>, stmt: &ir::Statement, l: VariableId, r: VariableId, op: InstructionOpcode) {
        let l = self.generate_read(buffer, l);
        let r = self.generate_read(buffer, r);

        let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return };

        // Our binop instructions are "mutating" - for example, `add` acts like a `+=`.
        // So copy one of the values into the result register, then add onto that
        buffer.push(AssemblyItem::new_instruction(
            InstructionOpcode::Mov,
            &[result.into(), l.into()]
        ));
        buffer.push(AssemblyItem::new_instruction(
            op,
            &[result.into(), r.into()]
        ));
    }

    // TODO: generate_write(buffer, var, value)

    /// Returns a unique name for a basic block, used as an Assembly label to refer to its start.
    fn basic_block_label(&self, id: &BasicBlockId) -> String {
        format!("{}___block_{}", self.func.name, id.0)
    }

    /// Determines whether this function uses the given register to store any variable.
    ///
    /// Used to optimise register preservation - if a register is never used, it doesn't need to be
    /// preserved across call boundaries.
    fn is_register_allocated(&self, reg: GeneralPurposeRegister) -> bool {
        for alloc in self.allocations.values() {
            if alloc == &Allocation::Register(reg) {
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod test {
    use delta_null_core_emulator::{Core, memory::Memory};
    use delta_null_lang_backend::ir::{FunctionBuilder, ConstantValue, Instruction, InstructionKind, Type, IntegerSize};

    use crate::test_utils::*;

    #[test]
    fn test_add() {
        let func = FunctionBuilder::new("foo", &[]);
        let (_, mut block) = func.new_basic_block();
        let a = block.add_constant(ConstantValue::U16(0xAB));
        let b = block.add_constant(ConstantValue::U16(0x20));
        let c = block.add_constant(ConstantValue::U16(0x1));
        let add_1 = block.add_instruction(Instruction::new(InstructionKind::Add(a, b)));
        let add_2 = block.add_instruction(Instruction::new(InstructionKind::Add(add_1, c)));
        block.add_terminator(Instruction::new(InstructionKind::Return(Some(add_2))));
        block.finalize();
        let func = func.finalize();

        let core = execute_function(&compile_function(&func));

        assert_eq!(0xAB + 0x20 + 0x1, core.gprs[0]);
    }

    #[test]
    fn test_unconditional_branch() {
        let func = FunctionBuilder::new("foo", &[]);
        let (ids, mut blocks) = func.new_basic_blocks(6);

        // Each of the blocks (after the first) has a return statement, with a different value
        // Check that we jump to the correct one!
        blocks[0].add_terminator(Instruction::new(InstructionKind::Branch(ids[2])));
        for i in 1..=5 {
            let c = blocks[i].add_constant(ConstantValue::U16(i as u16 * 0x10));
            blocks[i].add_terminator(Instruction::new(InstructionKind::Return(Some(c))));
        }
        func.finalize_blocks(blocks);
        let func = func.finalize();

        let core = execute_function(&compile_function(&func));

        assert_eq!(0x20, core.gprs[0]);
    }

    #[test]
    fn test_conditional_branch() {
        fn make_test_with_value(b: bool) -> Core<impl Memory> {
            let func = FunctionBuilder::new("foo", &[]);
            let (ids, mut blocks) = func.new_basic_blocks(3);

            // Conditional jump
            let condition = blocks[0].add_constant(ConstantValue::Boolean(b));
            blocks[0].add_terminator(Instruction::new(InstructionKind::ConditionalBranch {
                condition,
                true_block: ids[1],
                false_block: ids[2],
            }));
            
            // True block
            let true_constant = blocks[1].add_constant(ConstantValue::U16(0x10));
            blocks[1].add_terminator(Instruction::new(InstructionKind::Return(Some(true_constant))));

            // False block
            let false_constant = blocks[2].add_constant(ConstantValue::U16(0xAB));
            blocks[2].add_terminator(Instruction::new(InstructionKind::Return(Some(false_constant))));

            func.finalize_blocks(blocks);
            let func = func.finalize();

            execute_function(&compile_function(&func))
        }

        assert_eq!(0x10, make_test_with_value(true).gprs[0]);
        assert_eq!(0xAB, make_test_with_value(false).gprs[0]);
    }

    #[test]
    fn test_phi() {
        fn make_test_with_value(b: bool) -> Core<impl Memory> {
            let func = FunctionBuilder::new("foo", &[]);
            let (ids, mut blocks) = func.new_basic_blocks(4);

            // Conditional jump
            let condition = blocks[0].add_constant(ConstantValue::Boolean(b));
            blocks[0].add_terminator(Instruction::new(InstructionKind::ConditionalBranch {
                condition,
                true_block: ids[1],
                false_block: ids[2],
            }));
            
            // True block
            let true_constant = blocks[1].add_constant(ConstantValue::U16(0x10));
            blocks[1].add_terminator(Instruction::new(InstructionKind::Branch(ids[3])));

            // False block
            let false_constant = blocks[2].add_constant(ConstantValue::U16(0xAB));
            blocks[2].add_terminator(Instruction::new(InstructionKind::Branch(ids[3])));

            // Convergence block with a phi node
            // We do an addition to make sure we're definitely going through this node, not just
            // returning directly from the true/false blocks
            let phi = blocks[3].add_instruction(Instruction::new(InstructionKind::Phi { choices: vec![
                (ids[1], true_constant),
                (ids[2], false_constant),
            ]}));
            let add_constant = blocks[3].add_constant(ConstantValue::U16(0x10));
            let phi_added = blocks[3].add_instruction(Instruction::new(InstructionKind::Add(phi, add_constant)));
            blocks[3].add_terminator(Instruction::new(InstructionKind::Return(Some(phi_added))));

            func.finalize_blocks(blocks);
            let func = func.finalize();

            execute_function(&compile_function(&func))
        }

        assert_eq!(0x10 + 0x10, make_test_with_value(true).gprs[0]);
        assert_eq!(0xAB + 0x10, make_test_with_value(false).gprs[0]);
    }

    #[test]
    fn test_locals() {
        let func = FunctionBuilder::new("foo", &[]);
        let local_a = func.new_local("a", Type::UnsignedInteger(IntegerSize::Bits16));
        let local_b = func.new_local("b", Type::UnsignedInteger(IntegerSize::Bits16));

        let (_, mut block) = func.new_basic_block();
        
        // Write both locals
        let a_value = block.add_constant(ConstantValue::U16(123));
        block.add_void_instruction(Instruction::new(InstructionKind::WriteLocal(local_a, a_value)));

        let b_value = block.add_constant(ConstantValue::U16(456));
        block.add_void_instruction(Instruction::new(InstructionKind::WriteLocal(local_b, b_value)));

        // Read back
        let a_read = block.add_instruction(Instruction::new(InstructionKind::ReadLocal(local_a)));
        let b_read = block.add_instruction(Instruction::new(InstructionKind::ReadLocal(local_b)));

        // Add and return
        let added = block.add_instruction(Instruction::new(InstructionKind::Add(a_read, b_read)));
        block.add_terminator(Instruction::new(InstructionKind::Return(Some(added))));

        block.finalize();
        let func = func.finalize();

        let core = execute_function(&compile_function(&func));

        assert_eq!(123 + 456, core.gprs[0]);
    }
}
