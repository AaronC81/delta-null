use std::collections::HashMap;

use delta_null_core_assembler::{AssemblyItem, AssemblyOperand, LabelAccess};
use delta_null_core_instructions::{GeneralPurposeRegister, GPR, InstructionOpcode, SPR};
use delta_null_lang_backend::ir::{Function, VariableId, self, BasicBlockId, InstructionKind, LocalId};

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

        // Prepend stack allocation stuff
        self.insert_stack_allocation_instructions(&mut buffer);

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

            ir::InstructionKind::ReadLocal(l) => {
                let local_offset = self.local_access_map()[l];
                let result = self.variable_reg(stmt.result.unwrap());

                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Spread,
                    &[result.into(), local_offset.into()] // TODO: limits local offsets to 16
                ));
            },

            ir::InstructionKind::WriteLocal(l, v) => {
                let v = self.generate_read(buffer, *v);
                let local_offset = self.local_access_map()[l];

                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Spwrite,
                    &[local_offset.into(), v.into()] // TODO: limits local offsets to 16
                ));
            },

            ir::InstructionKind::Add(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Add),
            ir::InstructionKind::Subtract(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Sub),
            ir::InstructionKind::Multiply(l, r) =>
                self.generate_arithmetic_bin_op(buffer, stmt, *l, *r, InstructionOpcode::Mul),

            ir::InstructionKind::Equals(l, r) => {
                let l = self.generate_read(buffer, *l);
                let r = self.generate_read(buffer, *r);

                let result = self.variable_reg(stmt.result.unwrap());

                // Do comparison - this puts result in `ef`
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Eq,
                    &[l.into(), r.into()]
                ));

                // Copy entire `ef` register out into result
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Movso,
                    &[result.into(), SPR::EF.into()]
                ));

                // Mask out the condition bit (0x0002)
                // This is tricky, because we need to load the mask into a separate register, but
                // the allocator only gave us one!
                // Use `r0`, preserving through the stack, or `r1` in case `r0` happens to be our
                // result register!
                let mask_reg = if result == GPR::R0 { GPR::R1 } else { GPR::R0 };
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Push,
                    &[mask_reg.into()]
                ));
                buffer.push(AssemblyItem::new_word_put(mask_reg, AssemblyOperand::Immediate(0x0002)));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::And,
                    &[result.into(), mask_reg.into()]
                ));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Pop,
                    &[mask_reg.into()]
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

                // This is going to exit this function, so we need to deallocate our stack
                self.insert_stack_deallocation_instructions(buffer);

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
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Cjmpoff,
                    &[AssemblyOperand::Label {
                        // We used `eqz`, so conditional-jump to the false block if that condition
                        // was met, because 0 is false
                        name: self.basic_block_label(false_block), 
                        access: Some(LabelAccess::Offset),
                    }]
                ));
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Jmpoff,
                    &[AssemblyOperand::Label {
                        name: self.basic_block_label(true_block), 
                        access: Some(LabelAccess::Offset),
                    }]
                ));
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
                let phi_result = self.variable_reg(first_stmt.result.unwrap());

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

    /// Inserts instructions at the beginning of an instruction buffer to allocate stack space for
    /// usage throughout the function, such as locals.
    fn insert_stack_allocation_instructions(&self, buffer: &mut Vec<AssemblyItem>) {
        // Insert stack allocation instructions
        // TODO: very crap way of doing it. consider preserving a register and using `spadd`
        for _ in 0..self.stack_space() {
            buffer.insert(
                0,
                AssemblyItem::new_instruction(InstructionOpcode::Spdec, &[])
            );
        }
    }

    /// Inserts instructions at the end of the instruction buffer to deallocate stack space, before
    /// a function returns.
    fn insert_stack_deallocation_instructions(&self, buffer: &mut Vec<AssemblyItem>) {
        // Insert stack deallocation instructions
        // TODO: very crap way of doing it. consider trashing a register and using `spadd`
        for _ in 0..self.stack_space() {
            buffer.push(
                AssemblyItem::new_instruction(InstructionOpcode::Spinc, &[])
            );
        }
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
    fn generate_read(&self, _buffer: &mut Vec<AssemblyItem>, var: VariableId) -> GeneralPurposeRegister {
        match &self.allocations[&var] {
            Allocation::Register(r) => *r,
            Allocation::Spill(_) => todo!("spilling not yet supported"),
        }
    }

    fn generate_arithmetic_bin_op(&self, buffer: &mut Vec<AssemblyItem>, stmt: &ir::Statement, l: VariableId, r: VariableId, op: InstructionOpcode) {
        let l = self.generate_read(buffer, l);
        let r = self.generate_read(buffer, r);

        let result = self.variable_reg(stmt.result.unwrap());

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
}

#[cfg(test)]
mod test {
    use delta_null_core_emulator::{Core, memory::Memory};
    use delta_null_lang_backend::ir::{FunctionBuilder, ConstantValue, Instruction, InstructionKind, Type, IntegerSize};

    use crate::test_utils::*;

    #[test]
    fn test_add() {
        let func = FunctionBuilder::new("foo");
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
        let func = FunctionBuilder::new("foo");
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
            let func = FunctionBuilder::new("foo");
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
            let func = FunctionBuilder::new("foo");
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
        let func = FunctionBuilder::new("foo");
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
