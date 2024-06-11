import os, subprocess
from amaranth import *
from amaranth.build import *
from ..modules.core import Core
from .base import BaseMemoryMap, BaseTop
from amaranth_boards.tinyfpga_bx import TinyFPGABXPlatform

class ColorlightI5MemoryMap(BaseMemoryMap):
    # The start address of RAM.
    RAM_START = 0x1000
    
    # The number of RAM words available.
    #
    # Trial-and-errored to be the largest with synthesises successfully.
    # Seems about right, though - TinyFPGA BX has:
    #       128k bits of block RAM
    #    -> 16k bytes
    #    -> 0x4000 bytes
    #    -> 0x2000 words
    RAM_DEPTH = 0x1E00

    # The start address of the HCR.
    HCR_START = 0xF000

    # The total number of GPIO pins available through the HCR.
    # Currently just one - the LED.
    HCR_GPIO_PIN_COUNT = 32

    # The number of clock cycles which occur per microsecond on this platform.
    # 25 MHz = 25 us^-1
    TICKS_PER_MICROSECOND = 25

    def bind_hcr_peripherals(self, platform: Platform, m: Module):
        # Create resources for each GPIO pin
        # Same hack as TinyFPGA, though it's a bit more complicated here because the pins have more
        # fiddly names.
        # 31 pins are listed here - plus the LED makes 32. (Refer to the design doc.)
        io_pins = [
            "M17", "R17", "T18", "K18", "P17", "R18", "C18",
            "G20", "K20", "L20", "N18", "J20", "L18", "M18", "N17",
            "A18", "A19", "B19", "D20", "C17", "B18", "B20", "F20",
            "E2",  "D2",  "B1",  "A3",  "D1",  "C1",  "C2",  "E3",
        ]
        resources = []
        for i in range(1, ColorlightI5MemoryMap.HCR_GPIO_PIN_COUNT):
            resources.append(Resource(f"gpio{i}", 0,
                Pins(io_pins[i-1], dir="io")
            ))
        platform.add_resources(resources)
        
        # Bind pins to registers
        # (Plus the LED pin)
        m.d.comb += platform.request("led").eq(self.hcr_gpio_o[0])
        for i in range(1, ColorlightI5MemoryMap.HCR_GPIO_PIN_COUNT):
            pin = platform.request(f"gpio{i}")
            m.d.comb += [
                self.hcr_gpio_i[i].eq(pin.i),
                pin.oe.eq(self.hcr_gpio_oe[i]),
                pin.o.eq(self.hcr_gpio_o[i]),
            ]


class ColorlightI5Top(BaseTop):
    MEMORY_MAP = ColorlightI5MemoryMap
    INITIAL_SP = 0x2E00
    INITIAL_IP = 0x1000
