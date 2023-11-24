from src.platform.tinyfpga_bx import TinyFPGABXSerialProgPlatform, TinyFPGABXTop
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

INSTRUCTIONS = [int(x, 16) for x in """
4300 1217 1A10 1320 1B10 6212 6212 6212 6213 6213 6213 6212 6212 6212 142D 1C10 6214 6214 6214 6214 6214 6214 60EA 2195 142D 1C10 4000 6214 4000 6214 21D5 6218 2195 142D 1C10 4000 6214 6214 6214 4000 6214 6214 6214 21D5 6218 16FF 1EFF 4826 5016 5000 61FC 6218
""".split()]

TinyFPGABXSerialProgPlatform().build(TinyFPGABXTop(instructions=INSTRUCTIONS), do_program=program)
