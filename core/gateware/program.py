from src.platform.tinyfpga_bx import TinyFPGABXSerialProgPlatform, TinyFPGABXTop, TinyFPGABXMemoryMap
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

ASM_FILE = os.path.join(os.path.dirname(__file__), "..", "examples", "sos_blink.dna")

# Assemble code from file
with open(ASM_FILE) as f:
    code = f.read()
instructions = assemble(code, start_address=TinyFPGABXMemoryMap.RAM_START)

# Build/program
TinyFPGABXSerialProgPlatform().build(TinyFPGABXTop(instructions=instructions), do_program=program)
