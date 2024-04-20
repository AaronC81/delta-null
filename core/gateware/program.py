from src.platform.tinyfpga_bx import TinyFPGABXSerialProgPlatform, TinyFPGABXTop, TinyFPGABXMemoryMap
from src.platform.colorlight_i5 import ColorlightI5Top, ColorlightI5MemoryMap
from src.boards.colorlight_i5_r7_0_ext_board import Colorlighti5R70ExtensionBoardPlatform
import sys, os
from tests.helpers import assemble

def abort():
    name = sys.argv[0]
    print(f"Usage: {name} program/build bx/i5 <asm-file>")
    sys.exit(1)

match sys.argv:
    case [_, "program", platform_id, asm_file]:
        program = True
    case [_, "build", platform_id, asm_file]:
        program = False
    case _:
        abort()

match platform_id:
    case "i5":
        platform = Colorlighti5R70ExtensionBoardPlatform()
        memory_map = ColorlightI5MemoryMap
        top_class = ColorlightI5Top
    case "bx":
        platform = TinyFPGABXSerialProgPlatform()
        memory_map = TinyFPGABXMemoryMap
        top_class = TinyFPGABXTop
    case _:
        abort()

# Assemble code from file
with open(asm_file) as f:
    code = f.read()
instructions = assemble(code, start_address=memory_map.RAM_START)

# Build/program
platform.build(top_class(instructions=instructions), do_program=program)
