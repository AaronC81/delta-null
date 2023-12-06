use std::collections::HashMap;

use delta_null_core_assembler::{AssemblyItem, AssemblyOperand};
use delta_null_core_instructions::{GeneralPurposeRegister, Instruction, GPR, InstructionOpcode, AnyRegister};
use delta_null_lang_backend::ir::{Function, VariableId, self};

use crate::reg_alloc::Allocation;

/// Handles code generation for a single function.
pub struct FunctionGenerator<'f> {
    func: &'f Function,
    allocations: HashMap<VariableId, Allocation>,
}

impl<'f> FunctionGenerator<'f> {
    pub fn new(func: &'f Function, allocations: HashMap<VariableId, Allocation>) -> Self {
        Self { func, allocations }
    }

    pub fn to_assembly(&self) -> Vec<AssemblyItem> {
        if self.func.blocks.len() != 1 {
            todo!("more than 1 block nyi");
        }

        self.ir_block_to_assembly(self.func.blocks.iter().next().unwrap().1)
    }

    /// Generates and returns the Assembly instructions to implement an entire basic block.
    pub fn ir_block_to_assembly(&self, block: &ir::BasicBlock) -> Vec<AssemblyItem> {
        let mut buffer = vec![];
        for stmt in &block.statements {
            self.ir_statement_to_assembly(&mut buffer, stmt);
        }
        buffer
    }

    /// Generates the Assembly instructions to implement a single IR statement.
    pub fn ir_statement_to_assembly(&self, buffer: &mut Vec<AssemblyItem>, stmt: &ir::Statement) {
        match stmt.instruction.kind {
            ir::InstructionKind::Constant(c) => {
                let reg = self.variable_reg(stmt.result.unwrap());
                
                let imm = match c {
                    ir::ConstantValue::U16(v) => v,
                    ir::ConstantValue::I16(v) => v as u16,
                    ir::ConstantValue::Boolean(b) => if b { 1 } else { 0 },
                };

                buffer.push(AssemblyItem::new_word_put(reg, imm.into()));
            },

            ir::InstructionKind::Add(l, r) => {
                let l = self.generate_read(buffer, l);
                let r = self.generate_read(buffer, r);

                let result = self.variable_reg(stmt.result.unwrap());

                // Our `add` instruction is "mutating" - it acts like a `+=`.
                // So copy one of the values into the result register, then add onto that
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Mov,
                    &[result.into(), l.into()]
                ));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Add,
                    &[result.into(), r.into()]
                ));
            },

            ir::InstructionKind::Return(ret) => {
                if let Some(ret) = ret {
                    // EABI says to use r0-r1 to pass return value
                    let ret = self.generate_read(buffer, ret);
                    buffer.push(AssemblyItem::new_instruction(
                        InstructionOpcode::Mov,
                        &[GPR::R0.into(), ret.into()]
                    ));
                }

                buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Ret, &[]));
            }

            ir::InstructionKind::Branch(_) => todo!(),
            ir::InstructionKind::ConditionalBranch { condition, true_block, false_block } => todo!(),
        }
    }

    /// Gets the register to use for a variable.
    fn variable_reg(&self, var: VariableId) -> GeneralPurposeRegister {
        match &self.allocations[&var] {
            Allocation::Register(r) => *r,
            Allocation::Spill(_) => todo!("spilling not yet supported"),
        }
    }

    /// Returns the instructions, if any, required to load a variable ID back into registers from a
    /// spill, so that it can be operated on.
    /// 
    /// Then, returns the general purpose register which contains the value.
    fn generate_read(&self, buffer: &mut Vec<AssemblyItem>, var: VariableId) -> GeneralPurposeRegister {
        match &self.allocations[&var] {
            Allocation::Register(r) => *r,
            Allocation::Spill(_) => todo!("spilling not yet supported"),
        }
    }

    /// Returns the instructions, if any, required to write a spilled variable's calculated value
    /// back onto the stack, so that it is preserved when the register it is temporarily using is
    /// trashed agian.
    fn generate_write(&self, buffer: &mut Vec<AssemblyItem>, var: VariableId) {
        match &self.allocations[&var] {
            Allocation::Register(_) => (),
            Allocation::Spill(_) => todo!("spilling not yet supported"),
        }
    }
}

#[cfg(test)]
mod test {
    use delta_null_lang_backend::ir::{FunctionBuilder, ConstantValue, Instruction, InstructionKind};

    use crate::test_utils::*;

    #[test]
    fn test_add() {
        let mut func = FunctionBuilder::new("foo");
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
}
