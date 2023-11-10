from amaranth import *
from amaranth.sim import Simulator
from src.modules.harness import CoreSimHarness

def bench():
    for _ in range(100):
        yield

sim = Simulator(CoreSimHarness(instructions=[
    0x1034, # putl r0, 0x34
    0x1812, # puth r0, 0x12
    0xFFFF, # hlt
]))
sim.add_clock(1e-6) # 1 MHz
sim.add_sync_process(bench)
with sim.write_vcd("core.vcd"):
    sim.run()

