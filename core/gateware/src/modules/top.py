from amaranth import *

class Blinky(Elaboratable):
    def __init__(self, counter_size):
        self.led = Signal()
        self.count = Signal(counter_size)

    def elaborate(self, platform) -> Module:
        m = Module()
        m.d.comb += self.led.eq(self.count[len(self.count) - 1])
        m.d.sync += self.count.eq(self.count + 1)
        return m
        
class Top(Elaboratable):
    def elaborate(self, platform) -> Module:
        m = Module()

        led: Signal = platform.request("led")
        blinky = Blinky(24)
        m.submodules += blinky
        m.d.comb += led.eq(blinky.led)

        return m
