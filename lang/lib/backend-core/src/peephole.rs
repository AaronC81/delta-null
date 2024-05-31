use delta_null_core_assembler::{AssemblyItem, AssemblyItemKind, AssemblyOperand};
use delta_null_core_instructions::{AnyRegister, InstructionOpcode, GPR};

use AssemblyItemKind::*;
use InstructionOpcode::*;
use AssemblyOperand::*;
use AnyRegister::*;

// TODO: test

/// Performs peephole optimisation on assembly items, iterating through a small window of the
/// items and replacing them with more efficient code.
pub fn peephole_optimise(items: &mut Vec<AssemblyItem>) {
    let window_size = 3;
    let mut window_start_index = 0;

    while window_start_index < items.len() {
        let window_end_index = usize::min(window_start_index + window_size, items.len() - 1);
        let window = &items[window_start_index..window_end_index];

        // Labels at the start of the window are fine, but if there are any labels further into the
        // window, we can't safely perform replacements. Something might be jumping to a specific
        // instruction within the window
        if window.iter().skip(1).any(|i| !i.labels.is_empty()) {
            window_start_index += 1;
            continue;
        }

        // Convert window into a window of kinds, which is more suitable for matching against
        let window = window
            .iter()
            .map(|i| i.kind.clone())
            .collect::<Vec<_>>();

        if let Some(replacement) = optimise_one_window(&window[..]) {
            let start_labels = items[window_start_index].labels.clone();

            let replacement_count = replacement.remove_count;
            let replacement_items = replacement.new_items.into_iter()
                .map(|k| AssemblyItem::new(k))
                .collect::<Vec<_>>();

            items.splice(window_start_index..(window_start_index+replacement_count), replacement_items);

            // Replace labels
            // TODO: what if this was one of the last instructions, and we just removed it?
            items[window_start_index].labels = start_labels;

            // Rewind back by the window size, so we can potentially find more optimisations with
            // our new set of instructions
            if window_start_index < window_size {
                window_start_index = 0;
            } else {
                window_start_index -= window_size;
            }
        } else {
            // No match, advance
            window_start_index += 1;
        }
    }
}

struct PeepholeReplacement {
    /// The number of instructions to delete, from the beginning of the window.
    remove_count: usize,

    /// The instructions to insert in place of the deleted ones.
    new_items: Vec<AssemblyItemKind>,
}

fn optimise_one_window(window: &[AssemblyItemKind]) -> Option<PeepholeReplacement> {
    // This instruction:
    //   mov x, x
    // Does nothing and can be removed
    if  let [Instruction(Mov, operands), ..] = window
        && let [Register(dest), Register(src)] = operands[..]
        && dest == src
    {
        return Some(PeepholeReplacement {
            remove_count: 1,
            new_items: vec![],
        })
    }

    // This pattern:
    //   mov x, y
    //   mov y, x
    // Can be replaced with:
    //   mov x, y
    // Because the second mov doesn't do anything
    if  let [
            Instruction(Mov, ops1),
            Instruction(Mov, ops2),
            ..
        ] = window
        && let [Register(dest1), Register(src1)] = ops1[..]
        && let [Register(dest2), Register(src2)] = ops2[..]
        && dest1 == src2
        && src1 == dest2
    {
        return Some(PeepholeReplacement {
            remove_count: 2,
            new_items: vec![window[0].clone()],
        })
    }

    // This pattern:
    //   spwrite i, x
    //   spread x, i
    // Can be replaced with:
    //   spwrite i, x
    // Because the stack is assumed to be 'normal' memory, so the read will not change x
    if  let [
            Instruction(Spwrite, write_ops),
            Instruction(Spread, read_ops),
            ..
        ] = window
        && let [write_index, Register(write_reg)] = &write_ops[..]
        && let [Register(read_reg), read_index] = &read_ops[..]
        && write_index == read_index
        && write_reg == read_reg
    {
        return Some(PeepholeReplacement {
            remove_count: 2,
            new_items: vec![window[0].clone()],
        })
    }

    // This pattern:
    //   .put x, 0
    // Can be replaced with:
    //   xor x, x
    // Which takes one fewer instruction when compiled
    if let [WordPut(r, Immediate(0)), ..] = window {
        return Some(PeepholeReplacement {
            remove_count: 1,
            new_items: vec![
                Instruction(Xor, vec![Register(G(*r)), Register(G(*r))])
            ],
        })
    }

    None
}

