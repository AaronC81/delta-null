from ..src.modules.harness import CoreSimHarness
from amaranth.sim import Simulator
from typing import List, Union, Callable
import subprocess, os
    

def run_sim(instructions: Union[List[int], str], after: Callable[[CoreSimHarness], None]):
    """Creates and runs a simulator with the given instructions, either as words or assembly code.
    
    After running the simulator to completion, executes the given callable, which pay perform
    additional assertions.
    """

    if isinstance(instructions, str):
        instructions = assemble(instructions)

    core = CoreSimHarness(instructions)
    sim = Simulator(core)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        for _ in range(len(instructions) * 3): # worst case
            yield

        yield from after(core)

    sim.add_sync_process(proc)
    sim.run()

ASSEMBLER_PATH = os.path.abspath(os.path.join(
    os.path.dirname(__file__), "..", "..", "..", "target", "debug", "delta-null-core-assembler-bin"
))

def assemble(code: str) -> List[int]:
    """Assembles code into a list of words."""

    if not os.path.exists(ASSEMBLER_PATH):
        raise RuntimeError(f"assembler does not exist, have you run `cargo build`? (at {ASSEMBLER_PATH})")

    process = subprocess.run(
        [ASSEMBLER_PATH, "-", "--output-format", "ascii-hex"],
        input=code.encode(),
        capture_output=True,
        check=True,
    )
    
    return [int(word, 16) for word in process.stdout.decode().split()]
