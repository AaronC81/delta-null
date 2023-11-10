from amaranth.sim import Simulator
from ..modules.harness import CoreSimHarness

class Helpers:
    def make_sim(instructions) -> (Simulator, CoreSimHarness):
        core = CoreSimHarness(instructions)
        sim = Simulator(core)
        sim.add_clock(1e-6) # 1 MHz
        return (sim, core)

    def run(instructions, after):
        sim, core = Helpers.make_sim(instructions)

        def proc():
            for _ in range(len(instructions) * 3): # worst case
                yield

            yield from after(core)

        sim.add_sync_process(proc)
        sim.run()

def test_nothing():
    """Ensure that a simple run which halts immediately doesn't explode."""
    core = Helpers.run([0xFFFF], lambda _: [])

def test_put():
    """Tests `putl` and `puth`."""
    
    def after(core):
        assert (yield core.r0) == 0x1234
        assert (yield core.r5) == 0xABCD
    Helpers.run([
        0x1034, # putl r0, 0x34
        0x1812, # puth r0, 0x12

        0x1DAB, # puth r5, 0xAB
        0x15CD, # putl r5, 0xCD
        
        0xFFFF, # hlt
    ], after)
