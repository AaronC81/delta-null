from amaranth import *
from .harness import CoreSimHarness
        
class Top(Elaboratable):
    def elaborate(self, platform) -> Module:
        m = Module()

        # TODO: use proper memory
        core_harness = CoreSimHarness(instructions=
            [int(x, 16) for x in "4300 1101 1900 12FF 1AFF 1307 1B00 4A12 5012 5000 6303 4000 1203 1A00 21C2".split()],

            # LED will show lowest bit of r0
            debug_led=platform.request("led")
        )
        m.submodules.harness = core_harness

        return m
