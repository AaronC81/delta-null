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
    HCR_GPIO_PIN_COUNT = 1

    def bind_hcr_peripherals(self, platform: Platform, m: Module):
        # Just the LED for now
        m.d.comb += platform.request("led").eq(self.hcr_gpio_o[0])

class ColorlightI5Top(BaseTop):
    MEMORY_MAP = ColorlightI5MemoryMap
    INITIAL_SP = 0x2E00
    INITIAL_IP = 0x1000
