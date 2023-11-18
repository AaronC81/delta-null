from amaranth import *
from amaranth.sim import Simulator
from src.modules.harness import CoreSimHarness

def bench():
    for _ in range(5000):
        yield

sim = Simulator(CoreSimHarness(instructions=
    [int(x, 16) for x in """
        4300 1211 1A00 131A 1B00 6212 6212 6212 6213 6213 6213 6212 6212 6212 1701 1F00 21C7 2195 1427 1C00 4000 6214 4000 6214 21D5 6218 2195 1427 1C00 4000 6214 6214 6214 4000 6214 6214 6214 21D5 6218 1603 1E00 172B 1F00 4826 5016 5000 6307 6218
    """.split()]
))
sim.add_clock(1e-6) # 1 MHz
sim.add_sync_process(bench)
with sim.write_vcd("core.vcd"):
    sim.run()

