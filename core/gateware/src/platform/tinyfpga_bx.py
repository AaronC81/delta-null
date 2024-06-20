import os, subprocess
from amaranth import *
from amaranth.build import *
from ..modules.core import Core
from .base import BaseMemoryMap, BaseTop
from amaranth_boards.tinyfpga_bx import TinyFPGABXPlatform

class TinyFPGABXSerialProgPlatform(TinyFPGABXPlatform):
    """Same as `TinyFPGABXPlatform`, but passes `--pyserial` when programming."""

    def toolchain_program(self, products, name):
        # Copied from base implementation, with --pyserial added
        tinyprog = os.environ.get("TINYPROG", "tinyprog")
        with products.extract("{}.bin".format(name)) as bitstream_filename:
            subprocess.check_call([tinyprog, "-p", bitstream_filename, "--pyserial"])

class TinyFPGABXMemoryMap(BaseMemoryMap):
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
    HCR_GPIO_PIN_COUNT = 32

    # The number of clock cycles which occur per microsecond on this platform.
    # 16 MHz = 16 us^-1
    TICKS_PER_MICROSECOND = 16

    # 16 MHz / 9600 Hz = 1667
    TICKS_PER_9600_BAUD = 1667

    def bind_hcr_peripherals(self, platform: Platform, m: Module):
        # Create a new resource for every mapped hardware GPIO pin
        # (There is a "connector" defined already, but I don't think we have direct access to
        #  those...)
        # It's possible to specify multiple pins for one resource, but that only gives us a single
        # output-enable for every pin. We need per-pin control.
        resources = []
        for i in range(1, TinyFPGABXMemoryMap.HCR_GPIO_PIN_COUNT):
            resources.append(Resource(f"gpio{i}", 0,
                Pins(f"{i}", dir="io", conn=("gpio", 0))                          
            ))
        platform.add_resources(resources)

        # Bind each IO pin
        # Pin 0 is LED (only output), the rest are GPIO (input/output)
        m.d.comb += platform.request("led").eq(self.hcr_gpio_o[0])
        for i in range(1, TinyFPGABXMemoryMap.HCR_GPIO_PIN_COUNT):
            pin = platform.request(f"gpio{i}")
            m.d.comb += [
                self.hcr_gpio_i[i].eq(pin.i),
                pin.oe.eq(self.hcr_gpio_oe[i]),
                pin.o.eq(self.hcr_gpio_o[i]),
            ]

class TinyFPGABXTop(BaseTop):
    MEMORY_MAP = TinyFPGABXMemoryMap
    INITIAL_SP = 0x2E00
    INITIAL_IP = 0x1000
