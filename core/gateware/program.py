from src.modules.top import Top
from src.platform.tinyfpga_bx import TinyFPGABXSerialProgPlatform
import sys

match sys.argv:
    case [_, "program"]:
        program = True
    case [_, "build"]:
        program = False
    case _:
        name = sys.argv[0]
        print(f"Usage: {name} program/build")
        sys.exit(1)

TinyFPGABXSerialProgPlatform().build(Top(), do_program=program)
