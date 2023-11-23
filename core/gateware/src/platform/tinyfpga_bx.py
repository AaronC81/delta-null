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


class TinyFPGABXTop(Elaboratable):
    # The number of RAM words available.
    #
    # Trial-and-errored to be the largest with synthesises successfully.
    # Seems about right, though - TinyFPGA BX has:
    #       128k bits of block RAM
    #    -> 16k bytes
    #    -> 0x4000 bytes
    #    -> 0x2000 words
    MEMORY_DEPTH = 0x1E00

    def __init__(self, instructions):
        self.mem_init = [0 for _ in range(TinyFPGABXTop.MEMORY_DEPTH)]
        for i, ins in enumerate(instructions):
            self.mem_init[i] = ins

    def elaborate(self, platform) -> Module:
        m = Module()

        mem = Memory(width=Core.DATA_WIDTH, depth=TinyFPGABXTop.MEMORY_DEPTH, init=self.mem_init)
        mem_read = mem.read_port()
        mem_write = mem.write_port()
        m.d.comb += mem_write.addr.eq(mem_read.addr)

        self.core = Core(
            mem_addr=mem_read.addr,
            mem_read_data=mem_read.data,
            mem_read_en=mem_read.en,
            mem_write_data=mem_write.data,
            mem_write_en=mem_write.en,
            debug_led=platform.request("led"),
        )

        m.submodules.mem_read = mem_read
        m.submodules.mem_write = mem_write
        m.submodules.core = self.core

        return m
