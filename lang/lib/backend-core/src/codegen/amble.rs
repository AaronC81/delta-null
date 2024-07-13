//! Generation of function pre- and post-amble.

use std::collections::HashMap;

use delta_null_core_assembler::{AssemblyItem, AssemblyOperand};
use delta_null_core_instructions::{GeneralPurposeRegister, GPR, InstructionOpcode, SPR};
use delta_null_lang_backend::ir::{BasicBlockId, InstructionKind};

use super::FunctionGenerator;

impl<'f, 'l> FunctionGenerator<'f, 'l> {
    /// Inserts instructions at the end of each basic block to fulfill phi instructions in other
    /// blocks.
    /// 
    /// Intended to execute as a post-processing step, after other block code has been generated.
    pub(super) fn insert_phi_instructions(&self, buffers: &mut HashMap<BasicBlockId, Vec<AssemblyItem>>) {
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
    pub(super) fn prepend_preamble(&self, buffer: &mut Vec<AssemblyItem>) {
        self.prepend_stack_allocation_instructions(buffer);
        self.prepend_callee_saved_register_save_instructions(buffer);
    }

    /// Inserts all code required for a graceful function postamble, before a return.
    pub(super) fn insert_postamble(&self, buffer: &mut Vec<AssemblyItem>) {
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
}
