from amaranth import *
from .core import Core

class CoreSimHarness(Elaboratable):
    MEMORY_DEPTH = 0x100

    def __init__(self, instructions):
        # Arrays don't seem to show up on the simulator, so bind them to signals instead
        self.r0 = Signal(Core.DATA_WIDTH)
        self.r1 = Signal(Core.DATA_WIDTH)
        self.r2 = Signal(Core.DATA_WIDTH)
        self.r3 = Signal(Core.DATA_WIDTH)
        self.r4 = Signal(Core.DATA_WIDTH)
        self.r5 = Signal(Core.DATA_WIDTH)
        self.r6 = Signal(Core.DATA_WIDTH)
        self.r7 = Signal(Core.DATA_WIDTH)

        self.ef = Signal(Core.DATA_WIDTH)

        self.mem_init = [0 for _ in range(CoreSimHarness.MEMORY_DEPTH)]
        for i, ins in enumerate(instructions):
            self.mem_init[i] = ins


    def elaborate(self, platform):
        m = Module()

        mem = Memory(width=Core.DATA_WIDTH, depth=CoreSimHarness.MEMORY_DEPTH, init=self.mem_init)
        mem_read = mem.read_port()
        mem_write = mem.write_port()
        m.d.comb += mem_write.addr.eq(mem_read.addr)

        self.core = Core(
            mem_addr=mem_read.addr,
            mem_read_data=mem_read.data,
            mem_read_en=mem_read.en,
            mem_write_data=mem_write.data,
            mem_write_en=mem_write.en,
        )

        for i in range(8):
            m.d.comb += getattr(self, f"r{i}").eq(self.core.gprs._mem[i])
        m.d.comb += self.ef.eq(self.core.ef)

        m.submodules.mem_read = mem_read
        m.submodules.mem_write = mem_write
        m.submodules.core = self.core

        return m
