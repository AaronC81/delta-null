from amaranth import *
from .harness import CoreSimHarness
        
class Top(Elaboratable):
    def elaborate(self, platform) -> Module:
        m = Module()

        # TODO: use proper memory
        core_harness = CoreSimHarness(instructions=[
            0x1001, # putl r0, 0x01
            0xFFFF, # hlt
        ])
        m.submodules += core_harness

        # Bind LED to lowest bit of r0
        led = platform.request("led")
        m.d.comb += led.eq(core_harness.r0[0])

        return m
