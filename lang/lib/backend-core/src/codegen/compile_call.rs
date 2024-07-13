//! Implements compilation for calls.

use std::collections::HashMap;

use delta_null_core_assembler::AssemblyItem;
use delta_null_core_instructions::{InstructionOpcode, GPR};
use delta_null_lang_backend::ir::{self, VariableId};

use crate::{reg_alloc::Allocation, CALL_TARGET_REGISTER, PARAMETER_PASSING_REGISTERS};

use super::FunctionGenerator;

impl<'f, 'l> FunctionGenerator<'f, 'l> {
    /// Translates a [InstructionKind::Call] to assembly and adds it to the given buffer.
    /// 
    /// This is a very complex statement to translate, so it's broken out into a helper function.
    pub(super) fn call_statement_to_assembly(&self, buffer: &mut Vec<AssemblyItem>, stmt: &ir::Statement, target: VariableId, arguments: &[VariableId]) {
        // Fetch result register. The call puts its result in `r0`, so we'll copy it here
        // later.
        //
        // This might be `None`, which means that the return value of the call was never
        // used. Unlike many instructions, we still need to perform codegen in this case,
        // because the call probably has side effects.
        let result = self.variable_reg(stmt.result.unwrap());

        // Right now, we put arguments and the target in their registers using a naive approach;
        // push them all to the stack (along with anything which actually needs to be preserved),
        // then cherry-pick them off.

        // Therefore, we need to push if one (or more) of the following:
        //  - live-in AND live-out AND caller-saved
        //      (so that it still has its old value after the call)
        //  - live-in AND contains an argument
        //  - live-in AND contains the target AND NOT the `CALL_TARGET_REGISTER`
        //      (both so that its value can be retrieved during our stack-twiddling)
        // In any other case, no need to push and restore.
        //
        // Additionally, we can skip pushing an argument if it's not live-out (i.e. not reused
        // elsewhere) and is already in the correct register.

        // Find the set of GPRs we need to push
        let (live_in_gprs, live_out_gprs) = self.liveness.live_in_out(stmt.id);
        let mut pushed_gprs = live_in_gprs
            .iter()
            .filter_map(|var|
                // We only care about variables which are allocated to registers
                if let Some(Allocation::Register(r)) = self.allocations.get(var) {
                    Some((*var, *r))
                } else {
                    None
                })
            .filter(|(var, reg)|
                // We only searched the live GPR set, so all of these include that "live-in AND..."
                // precondition already.

                // ... live-out AND caller-saved
                (live_out_gprs.contains(var) && Self::CALLER_SAVED_GPRS.contains(reg))
                // ... contains an argument
                || arguments.contains(var)
                // ... conatins the target AND NOT the `CALL_TARGET_REGISTER` 
                || *var == target && *reg != CALL_TARGET_REGISTER
            )
            .filter(|(var, reg)| {
                // Optimisation!
                // We *do NOT* need to push if the variable is NOT live-out, and represents an
                // argument which is already in the correct register.
                // We can just leave it where it is, and permit it to get trashed.
                let arg_index = arguments.iter()
                    .enumerate().find(|(_, arg)| *arg == var)
                    .map(|(i, _)| i);
                !(
                    !live_out_gprs.contains(var)
                    && arg_index.is_some()
                    && PARAMETER_PASSING_REGISTERS.get(arg_index.unwrap()) == Some(reg)
                )
            })
            .map(|(_, r)| r)
            .collect::<Vec<_>>();
        pushed_gprs.sort_by_key(|r| r.number());

        // Push all live GPRs
        let mut gpr_stack_offsets = HashMap::new();
        for reg in &pushed_gprs {
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
        if arguments.len() > PARAMETER_PASSING_REGISTERS.len() {
            panic!(
                "too many parameters for EABI (found {} but maximum is {})",
                arguments.len(), PARAMETER_PASSING_REGISTERS.len(),
            );
        }
        for (argument, reg) in arguments.iter().zip(PARAMETER_PASSING_REGISTERS) {
            let source = self.generate_read(buffer, *argument);

            // Generate `spread` to grab the parameter off our stack copy
            if let Some(offset) = gpr_stack_offsets.get(&source) {
                buffer.push(AssemblyItem::new_instruction(
                    InstructionOpcode::Spread,
                    &[reg.into(), (*offset).into()],
                ));
            } else {
                // If there's no offset, we can assume that the argument is already in the correct
                // register.
                assert!(self.allocations[argument] == Allocation::Register(reg))
            }
        }
        
        // Use an arbitrarily-chosen (non-parameter-passing) register, to hold our target
        // (No need to read it off the stack if it's already in the right register)
        let target = self.generate_read(buffer, target);
        if target != CALL_TARGET_REGISTER {
            buffer.push(AssemblyItem::new_instruction(
                InstructionOpcode::Spread,
                &[CALL_TARGET_REGISTER.into(), gpr_stack_offsets[&target].into()],
            ));
        }

        // Call, and copy result (in `r0`) to result register
        buffer.push(AssemblyItem::new_instruction(
            InstructionOpcode::Call,
            &[CALL_TARGET_REGISTER.into()],
        ));
        if let Some(result) = result {
            buffer.push(AssemblyItem::new_instruction(
                InstructionOpcode::Mov,
                &[result.into(), GPR::R0.into()],
            ));
        }

        // Restore registers
        for reg in pushed_gprs.iter().rev() {
            // ...except result register
            if Some(*reg) == result {
                buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Spinc, &[]));
            } else {
                buffer.push(AssemblyItem::new_instruction(InstructionOpcode::Pop, &[(*reg).into()]));
            }
        }
    }
}
