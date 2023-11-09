from src.modules.top import Top
from src.platform.tinyfpga_bx import TinyFPGABXSerialProgPlatform

TinyFPGABXSerialProgPlatform().build(Top(), do_program=True)
