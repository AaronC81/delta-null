from amaranth import *
from amaranth.sim import Simulator
from src.modules.core import Core

class CoreSimHarness(Elaboratable):
    def __init__(self):
        # Arrays don't seem to show up on the simulator, so bind them to signals instead
        self.r0 = Signal(Core.DATA_WIDTH)
        self.r1 = Signal(Core.DATA_WIDTH)
        self.r2 = Signal(Core.DATA_WIDTH)
        self.r3 = Signal(Core.DATA_WIDTH)
        self.r4 = Signal(Core.DATA_WIDTH)
        self.r5 = Signal(Core.DATA_WIDTH)
        self.r6 = Signal(Core.DATA_WIDTH)
        self.r7 = Signal(Core.DATA_WIDTH)

    def elaborate(self, platform):
        m = Module()

        mem_init = [0 for _ in range(0x100)]
        mem_init[1] = 0x1034 # putl r0, 0x34
        mem_init[2] = 0x1812 # puth r0, 0x12
        mem_init[3] = 0xFFFF # hlt

        mem = Memory(width=Core.DATA_WIDTH, depth=0x100, init=mem_init)        

        mem_read = mem.read_port()
        mem_write = mem.write_port()
        m.d.comb += mem_write.addr.eq(mem_read.addr)

        core = Core(
            mem_addr=mem_read.addr,
            mem_read_data=mem_read.data,
            mem_read_en=mem_read.en,
            mem_write_data=mem_write.data,
            mem_write_en=mem_write.en,
        )

        for i in range(8):
            m.d.comb += getattr(self, f"r{i}").eq(core.gprs[i])

        m.submodules += mem_read
        m.submodules += mem_write
        m.submodules += core

        return m

def bench():
    for _ in range(100):
        yield

sim = Simulator(CoreSimHarness())
sim.add_clock(1e-6) # 1 MHz
sim.add_sync_process(bench)
with sim.write_vcd("core.vcd"):
    sim.run()

