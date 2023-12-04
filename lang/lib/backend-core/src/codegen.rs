use std::collections::HashMap;

use delta_null_core_instructions::{GeneralPurposeRegister, Instruction, GPR};
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

    /// Generates and returns the DNA instructions to implement an entire basic block.
    pub fn ir_block_to_dna_instructions(&self, block: &ir::BasicBlock) -> Vec<Instruction> {
        let mut buffer = vec![];
        for stmt in &block.statements {
            self.ir_statement_to_dna_instructions(&mut buffer, stmt);
        }
        buffer
    }

    /// Generates the DNA instructions to implement a single IR statement.
    pub fn ir_statement_to_dna_instructions(&self, buffer: &mut Vec<Instruction>, stmt: &ir::Statement) {
        match stmt.instruction.kind {
            ir::InstructionKind::Constant(c) => {
                let reg = self.variable_reg(stmt.result.unwrap());
                
                let (low_imm, high_imm) = match c {
                    ir::ConstantValue::U16(v) => ((v & 0xFF) as u8, (v << 8) as u8),
                    ir::ConstantValue::I16(v) => ((v & 0xFF) as u8, (v << 8) as u8),
                    ir::ConstantValue::Boolean(b) => if b { (1, 0) } else { (0, 0) },
                };

                buffer.push(Instruction::Putl { reg, imm: low_imm });
                buffer.push(Instruction::Puth { reg, imm: high_imm });
            },

            ir::InstructionKind::Add(l, r) => {
                let l = self.generate_read(buffer, l);
                let r = self.generate_read(buffer, r);

                let result = self.variable_reg(stmt.result.unwrap());

                buffer.push(Instruction::Mov { dest: result, src: l });
                buffer.push(Instruction::Add { reg: result, val: r });
            },

            ir::InstructionKind::Return(ret) => {
                if let Some(ret) = ret {
                    // EABI says to use r0-r1 to pass return value
                    let ret = self.generate_read(buffer, ret);
                    buffer.push(Instruction::Mov { dest: GPR::R0, src: ret });
                }

                buffer.push(Instruction::Ret);
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
    fn generate_read(&self, buffer: &mut Vec<Instruction>, var: VariableId) -> GeneralPurposeRegister {
        match &self.allocations[&var] {
            Allocation::Register(r) => *r,
            Allocation::Spill(_) => todo!("spilling not yet supported"),
        }
    }

    /// Returns the instructions, if any, required to write a spilled variable's calculated value
    /// back onto the stack, so that it is preserved when the register it is temporarily using is
    /// trashed agian.
    fn generate_write(&self, buffer: &mut Vec<Instruction>, var: VariableId) {
        match &self.allocations[&var] {
            Allocation::Register(_) => (),
            Allocation::Spill(_) => todo!("spilling not yet supported"),
        }
    }
}

#[cfg(test)]
mod test {
    use delta_null_core_instructions::ToAssembly;
    use delta_null_lang_backend::{ir::{FunctionBuilder, ConstantValue, Instruction, InstructionKind, PrintOptions, PrintIR}, analysis::{liveness::liveness_analysis, flow::ControlFlowGraph}};

    use crate::{reg_alloc::allocate, codegen};

    #[test]
    fn TEMP() {
        let mut func = FunctionBuilder::new("foo");
        let (id, mut block) = func.new_basic_block();
        let a = block.add_constant(ConstantValue::U16(0xAB));
        let b = block.add_constant(ConstantValue::U16(0x20));
        let c = block.add_constant(ConstantValue::U16(0x1));
        let add_1 = block.add_instruction(Instruction::new(InstructionKind::Add(a, b)));
        let add_2 = block.add_instruction(Instruction::new(InstructionKind::Add(add_1, c)));
        block.add_terminator(Instruction::new(InstructionKind::Return(Some(add_2))));
        block.finalize();
        let func = func.finalize();

        let analysis = liveness_analysis(&func);
        let cfg = ControlFlowGraph::generate(&func);

        let allocation = allocate(&func, &cfg, &analysis);

        let code = codegen::FunctionGenerator::new(&func, allocation);
        let buffer = code.ir_block_to_dna_instructions(&func.blocks[&id]);

        panic!("{}\n\n---\n\n{}", func.print_ir(&PrintOptions::default()), buffer.iter().map(|i| i.to_assembly()).collect::<Vec<_>>().join("\n"))
    }
}
