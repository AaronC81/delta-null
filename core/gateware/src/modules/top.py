from amaranth import *
from .harness import CoreSimHarness
        
class Top(Elaboratable):
    def elaborate(self, platform) -> Module:
        m = Module()

        # TODO: use proper memory
        core_harness = CoreSimHarness(instructions=[
            # putl r2, data/lo
            # puth r2, data/hi
            # read r1, r2
            # read r0, r2
            # hlt
            # data: .word 1
            0x1205, 0x1A00, 0x2021, 0x2020, 0xFFFF, 0x0001
        ])
        m.submodules += core_harness

        # Bind LED to lowest bit of r0
        led = platform.request("led")
        m.d.comb += led.eq(core_harness.r0[0])

        return m
