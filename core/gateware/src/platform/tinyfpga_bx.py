import os, subprocess
from amaranth import *
from ..modules.core import Core
from amaranth_boards.tinyfpga_bx import TinyFPGABXPlatform

class TinyFPGABXSerialProgPlatform(TinyFPGABXPlatform):
    """Same as `TinyFPGABXPlatform`, but passes `--pyserial` when programming."""

    def toolchain_program(self, products, name):
        # Copied from base implementation, with --pyserial added
        tinyprog = os.environ.get("TINYPROG", "tinyprog")
        with products.extract("{}.bin".format(name)) as bitstream_filename:
            subprocess.check_call([tinyprog, "-p", bitstream_filename, "--pyserial"])


class TinyFPGABXMemoryMap(Elaboratable):
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

    def __init__(self, init_ram):
        self.init_ram = init_ram

        self.addr = Signal(Core.DATA_WIDTH)
        self.read_data = Signal(Core.DATA_WIDTH)
        self.write_data = Signal(Core.DATA_WIDTH)
        self.read_en = Signal()
        self.write_en = Signal()

    def elaborate(self, platform) -> Module:
        m = Module()

        # Instantiate RAM
        ram = Memory(width=Core.DATA_WIDTH, depth=TinyFPGABXMemoryMap.RAM_DEPTH, init=self.init_ram)
        mem_read = ram.read_port()
        mem_write = ram.write_port()
        m.d.comb += mem_write.addr.eq(mem_read.addr)

        m.submodules.mem_read = mem_read
        m.submodules.mem_write = mem_write

        m.d.comb += [
            mem_read.addr.eq(self.addr),
            self.read_data.eq(mem_read.data),
            mem_write.data.eq(self.write_data),
            mem_write.en.eq(self.write_en),
        ]
        if isinstance(mem_read.en, Signal):
            m.d.comb += mem_read.en.eq(self.read_en)

        return m


class TinyFPGABXTop(Elaboratable):
    def __init__(self, instructions):
        self.mem_init = [0 for _ in range(TinyFPGABXMemoryMap.RAM_DEPTH)]
        for i, ins in enumerate(instructions):
            self.mem_init[i] = ins

    def elaborate(self, platform) -> Module:
        m = Module()

        mem = TinyFPGABXMemoryMap(self.mem_init)
        self.core = Core(
            mem_addr=mem.addr,
            mem_read_data=mem.read_data,
            mem_read_en=mem.read_en,
            mem_write_data=mem.write_data,
            mem_write_en=mem.write_en,
            debug_led=platform.request("led"),
        )

        m.submodules.mem = mem
        m.submodules.core = self.core

        return m



