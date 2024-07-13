use std::collections::HashMap;

use delta_null_core_assembler::AssemblyItem;
use delta_null_lang_backend::{analysis::liveness::LivenessAnalysis, ir::{Function, VariableId}};

use crate::reg_alloc::Allocation;

mod compile;
mod compile_call;
mod amble;
mod helpers;
mod allocation;

/// Handles code generation for a single function.
pub struct FunctionGenerator<'f, 'l> {
    func: &'f Function,
    allocations: HashMap<VariableId, Allocation>,
    liveness: &'l LivenessAnalysis,

    is_leaf: bool,
}

impl<'f, 'l> FunctionGenerator<'f, 'l> {
    pub fn new(func: &'f Function, allocations: HashMap<VariableId, Allocation>, liveness: &'l LivenessAnalysis, is_leaf: bool) -> Self {
        Self { func, allocations, liveness, is_leaf }
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
