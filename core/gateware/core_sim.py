from amaranth import *
from amaranth.sim import Simulator
from src.modules.harness import CoreSimHarness

def bench():
    for _ in range(5000):
        yield

sim = Simulator(CoreSimHarness(instructions=
    [int(x, 16) for x in "4300 1101 1900 1210 1A00 1307 1B00 4A12 5012 5000 6303 4000 1203 1A00 21C2".split()]
))
sim.add_clock(1e-6) # 1 MHz
sim.add_sync_process(bench)
with sim.write_vcd("core.vcd"):
    sim.run()

