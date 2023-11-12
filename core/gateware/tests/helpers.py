from ..src.modules.harness import CoreSimHarness
from amaranth.sim import Simulator
from typing import List
import subprocess, os

def make_sim(instructions) -> (Simulator, CoreSimHarness):
    core = CoreSimHarness(instructions)
    sim = Simulator(core)
    sim.add_clock(1e-6) # 1 MHz
    return (sim, core)

def run(instructions, after):
    if isinstance(instructions, str):
        instructions = assemble(instructions)

    sim, core = make_sim(instructions)

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
    if not os.path.exists(ASSEMBLER_PATH):
        raise RuntimeError(f"assembler does not exist, have you run `cargo build`? (at {ASSEMBLER_PATH})")

    process = subprocess.run(
        [ASSEMBLER_PATH, "-", "--output-format", "ascii-hex"],
        input=code.encode(),
        capture_output=True,
        check=True,
    )
    
    return [int(word, 16) for word in process.stdout.decode().split()]
