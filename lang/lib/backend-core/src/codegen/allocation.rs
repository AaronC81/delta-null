//! Allocation of variables and locals throughout codegen.

use std::collections::HashMap;

use delta_null_core_assembler::AssemblyItem;
use delta_null_core_instructions::{GeneralPurposeRegister, InstructionOpcode};
use delta_null_lang_backend::ir::{self, LocalId, VariableId};

use crate::reg_alloc::Allocation;

use super::FunctionGenerator;

impl<'f, 'l> FunctionGenerator<'f, 'l> {
    /// Calculates the amount of stack space required throughout this function.
    /// 
    /// Due to the lack of a base pointer, code generation allocates all stack space required for
    /// a function upfront.
    /// 
    /// At the borders between any two IR instructions within the same function, the stack pointer
    /// will not move. The stack pointer may change within the same instruction to implement
    /// register preservation or other actions, but the changes must not carry over to the next IR
    /// instruction.
    pub(super) fn stack_space(&self) -> usize {
        self.func.locals.values()
            .map(|local| local.ty.word_size())
            .sum()
    }

    /// Builds a map describing the stack offsets used to access locals on the stack. An offset of
    /// 0 means that a local resides at the stack pointer, 1 means stack pointer + 1, etc.
    /// 
    /// This relies on the guarantee the stack pointer within a function does not move between
    /// IR instructions. 
    pub(super) fn local_access_map(&self) -> HashMap<LocalId, u16> {
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
    pub(super) fn variable_reg(&self, var: VariableId) -> Option<GeneralPurposeRegister> {
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
    pub(super) fn generate_read(&self, _buffer: &mut Vec<AssemblyItem>, var: VariableId) -> GeneralPurposeRegister {
        match &self.allocations[&var] {
            Allocation::Register(r) => *r,
            Allocation::Spill(_) => todo!("spilling not yet supported"),
        }
    }

    pub(super) fn generate_arithmetic_bin_op(&self, buffer: &mut Vec<AssemblyItem>, stmt: &ir::Statement, l: VariableId, r: VariableId, op: InstructionOpcode, supports_in_place: bool) {
        let l = self.generate_read(buffer, l);
        let r = self.generate_read(buffer, r);

        let Some(result) = self.variable_reg(stmt.result.unwrap()) else { return };

        // Handle in-place usage
        if l == result {
            assert!(supports_in_place, "registers allocated in-place for an operation which doesn't support it");
            buffer.push(AssemblyItem::new_instruction(
                op,
                &[result.into(), r.into()]
            ));
        } else if r == result {
            assert!(supports_in_place, "registers allocated in-place for an operation which doesn't support it");
            buffer.push(AssemblyItem::new_instruction(
                op,
                &[result.into(), l.into()]
            ));
        } else {
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
    }

    // TODO: generate_write(buffer, var, value)

    /// Determines whether this function uses the given register to store any variable.
    ///
    /// Used to optimise register preservation - if a register is never used, it doesn't need to be
    /// preserved across call boundaries.
    pub(super) fn is_register_allocated(&self, reg: GeneralPurposeRegister) -> bool {
        for alloc in self.allocations.values() {
            if alloc == &Allocation::Register(reg) {
                return true;
            }
        }

        false
    }
}
