//! Various helpers for facilitate convenient code generation.

use delta_null_core_assembler::{AssemblyItem, AssemblyOperand, LabelAccess};
use delta_null_core_instructions::{InstructionOpcode, GPR, SPR};
use delta_null_lang_backend::ir::{self, BasicBlockId, InstructionKind};

use super::FunctionGenerator;

impl<'f, 'l> FunctionGenerator<'f, 'l> {
    pub const CALLER_SAVED_GPRS: [GPR; 4] = [GPR::R0, GPR::R1, GPR::R2, GPR::R3];
    pub const CALLEE_SAVED_GPRS: [GPR; 4] = [GPR::R4, GPR::R5, GPR::R6, GPR::R7];

    /// Inserts instructions to branch to a particular block if the condition flag is set, or a
    /// different block if it is not set.
    pub(super) fn insert_bidirectional_branch_instructions(&self, buffer: &mut Vec<AssemblyItem>, set_block: BasicBlockId, unset_block: BasicBlockId) {
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
    pub(super) fn insert_comparison_value_conversion(&self, buffer: &mut Vec<AssemblyItem>, result_gpr: GPR, stmt: &ir::Statement, after: &[ir::Statement]) -> Option<usize> {
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

    /// Returns a unique name for a basic block, used as an Assembly label to refer to its start.
    pub(super) fn basic_block_label(&self, id: &BasicBlockId) -> String {
        format!("{}___block_{}", self.func.name, id.0)
    }
}
