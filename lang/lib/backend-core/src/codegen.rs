use std::collections::HashMap;

use delta_null_core_assembler::{AssemblyItem, AssemblyOperand, LabelAccess};
use delta_null_core_instructions::{GeneralPurposeRegister, Instruction, GPR, InstructionOpcode, AnyRegister};
use delta_null_lang_backend::ir::{Function, VariableId, self, BasicBlockId};

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
        let mut buffer = vec![];

        // Assumes that first ID'd block is the entry point of the function
        let mut sorted_block_ids = self.func.blocks.keys().collect::<Vec<_>>();
        sorted_block_ids.sort();
        for id in sorted_block_ids {
            buffer.extend(self.ir_block_to_assembly(&self.func.blocks[id]));
        }

        buffer
    }

    /// Generates and returns the Assembly instructions to implement an entire basic block.
    pub fn ir_block_to_assembly(&self, block: &ir::BasicBlock) -> Vec<AssemblyItem> {
        let mut buffer = vec![];
        for stmt in &block.statements {
            self.ir_statement_to_assembly(&mut buffer, stmt);
        }

        // Add label to first instruction of the block
        buffer[0].labels.push(self.basic_block_label(&block.id));

        buffer
    }

    /// Generates the Assembly instructions to implement a single IR statement.
    pub fn ir_statement_to_assembly(&self, buffer: &mut Vec<AssemblyItem>, stmt: &ir::Statement) {
        match &stmt.instruction.kind {
            ir::InstructionKind::Constant(c) => {
                let reg = self.variable_reg(stmt.result.unwrap());
                
                let imm = match c {
                    ir::ConstantValue::U16(v) => *v,
                    ir::ConstantValue::I16(v) => *v as u16,
                    ir::ConstantValue::Boolean(b) => if *b { 1 } else { 0 },
                };

                buffer.push(AssemblyItem::new_word_put(reg, imm.into()));
            },

            ir::InstructionKind::Add(l, r) => {
                let l = self.generate_read(buffer, *l);
                let r = self.generate_read(buffer, *r);

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
                if let Some(ret) = *ret {
                    // EABI says to use r0-r1 to pass return value
                    let ret = self.generate_read(buffer, ret);
                    buffer.push(AssemblyItem::new_instruction(
                        InstructionOpcode::Mov,
                        &[GPR::R0.into(), ret.into()]
                    ));
                }

                buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Ret, &[]));
            }

            ir::InstructionKind::Branch(dest) => {
                // TODO: offset-branch might not always reach sufficiently far, but dealing with
                // register allocation for this is complicated!
                // This is probably fine for most trivial stuff, for now.
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Jmpoff,
                    &[AssemblyOperand::Label {
                        name: self.basic_block_label(&dest),
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
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Cjmpoff,
                    &[AssemblyOperand::Label {
                        // We used `eqz`, so conditional-jump to the false block if that condition
                        // was met, because 0 is false
                        name: self.basic_block_label(&false_block), 
                        access: Some(LabelAccess::Offset),
                    }]
                ));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Jmpoff,
                    &[AssemblyOperand::Label {
                        name: self.basic_block_label(&true_block), 
                        access: Some(LabelAccess::Offset),
                    }]
                ));
            }

            ir::InstructionKind::Phi { choices } => todo!(),
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

    /// Returns a unique name for a basic block, used as an Assembly label to refer to its start.
    fn basic_block_label(&self, id: &BasicBlockId) -> String {
        format!("{}___block_{}", self.func.name, id.0)
    }
}

#[cfg(test)]
mod test {
    use delta_null_core_emulator::{Core, memory::Memory};
    use delta_null_lang_backend::ir::{FunctionBuilder, ConstantValue, Instruction, InstructionKind, PrintIR, PrintOptions};

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

    #[test]
    fn test_unconditional_branch() {
        let mut func = FunctionBuilder::new("foo");
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
            let mut func = FunctionBuilder::new("foo");
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
}
