from ..src.modules.harness import CoreSimHarness
from amaranth.sim import Simulator

def make_sim(instructions) -> (Simulator, CoreSimHarness):
    core = CoreSimHarness(instructions)
    sim = Simulator(core)
    sim.add_clock(1e-6) # 1 MHz
    return (sim, core)

def run(instructions, after):
    sim, core = make_sim(instructions)

    def proc():
        for _ in range(len(instructions) * 3): # worst case
            yield

        yield from after(core)

    sim.add_sync_process(proc)
    sim.run()
