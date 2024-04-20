#from src.platform.tinyfpga_bx import TinyFPGABXSerialProgPlatform, TinyFPGABXTop, TinyFPGABXMemoryMap
from src.platform.colorlight_i5 import ColorlightI5Top, ColorlightI5MemoryMap
from src.boards.colorlight_i5_r7_0_ext_board import Colorlighti5R70ExtensionBoardPlatform
import sys, os
from tests.helpers import assemble

match sys.argv:
    case [_, "program"]:
        program = True
    case [_, "build"]:
        program = False
    case _:
        name = sys.argv[0]
        print(f"Usage: {name} program/build")
        sys.exit(1)

ASM_FILE = "/Users/aaron/Source/delta-null/modern_blink.dna" #os.path.join(os.path.dirname(__file__), "..", "examples", "sos_blink.dna")

# Assemble code from file
with open(ASM_FILE) as f:
    code = f.read()
instructions = assemble(code, start_address=ColorlightI5MemoryMap.RAM_START)

# Build/program
Colorlighti5R70ExtensionBoardPlatform().build(ColorlightI5Top(instructions=instructions))
