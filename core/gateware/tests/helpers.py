# Required for import of `src.modules.harness` from `program.py`
import sys, os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from src.modules.harness import CoreSimHarness
from amaranth.sim import Simulator
from typing import List, Optional, Union, Callable
import subprocess
    

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

    limit = 10000
    def proc():
        nonlocal limit
        while (yield core.ef[0]) == 0:
            yield
            limit -= 1
            if limit == 0:
                raise Exception("core didn't halt in reasonable time")

        yield from after(core)

    sim.add_sync_process(proc)
    sim.run()

ASSEMBLER_PATH = os.path.abspath(os.path.join(
    os.path.dirname(__file__), "..", "..", "..", "target", "debug", "delta-null-core-assembler-bin"
))

def assemble(code: str, start_address: Optional[int] = None) -> List[int]:
    """Assembles code into a list of words."""

    if not os.path.exists(ASSEMBLER_PATH):
        raise RuntimeError(f"assembler does not exist, have you run `cargo build`? (at {ASSEMBLER_PATH})")

    args = [ASSEMBLER_PATH, "-", "--output-format", "ascii-hex"]
    if start_address is not None:
        args.extend(["--start-address", str(start_address)])

    process = subprocess.run(
        args,
        input=code.encode(),
        capture_output=True,
        check=True,
    )
    
    return [int(word, 16) for word in process.stdout.decode().split()]
