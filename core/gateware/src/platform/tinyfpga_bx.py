import os, subprocess
from amaranth_boards.tinyfpga_bx import TinyFPGABXPlatform

class TinyFPGABXSerialProgPlatform(TinyFPGABXPlatform):
    """Same as `TinyFPGABXPlatform`, but passes `--pyserial` when programming."""

    def toolchain_program(self, products, name):
        # Copied from base implementation, with --pyserial added
        tinyprog = os.environ.get("TINYPROG", "tinyprog")
        with products.extract("{}.bin".format(name)) as bitstream_filename:
            subprocess.check_call([tinyprog, "-p", bitstream_filename, "--pyserial"])
