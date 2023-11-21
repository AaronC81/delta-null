from amaranth import *
from .harness import CoreSimHarness
        
class Top(Elaboratable):
    def elaborate(self, platform) -> Module:
        m = Module()

        # TODO: use proper memory
        core_harness = CoreSimHarness(instructions=
            [int(x, 16) for x in """
4300 1217 1A00 1320 1B00 6212 6212 6212 6213 6213 6213 6212 6212 6212 142D 1C00 6214 6214 6214 6214 6214 6214 60EA 2195 142D 1C00 4000 6214 4000 6214 21D5 6218 2195 142D 1C00 4000 6214 6214 6214 4000 6214 6214 6214 21D5 6218 16FF 1EFF 4826 5016 5000 61FC 6218
             """.split()],

            # LED will show lowest bit of r0
            debug_led=platform.request("led")
        )
        m.submodules.harness = core_harness

        return m
