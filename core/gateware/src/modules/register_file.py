from amaranth import *
from enum import Enum

class RegisterFile(Elaboratable):
    """A register file for the core's general-purpose registers.
    
    Select an address with `addr`, then:
        - Read by setting `read_en` and `read_data`, then waiting two cycles
        - Write by setting `write_en` and `write_data`, then waiting one cycle
    """

    def __init__(self, width):
        self.width = width

        self.addr = Signal(self.width)
        self.read_data = Signal(self.width)
        self.read_en = Signal(self.width)
        self.write_data = Signal(self.width)
        self.write_en = Signal(self.width)

    def elaborate(self, platform):
        m = Module()

        # Instantiate memory
        mem = Memory(width=self.width, depth=8, init=[0,0,0,0,0,0,0,0])
        read = mem.read_port()
        write = mem.write_port()

        m.submodules += read
        m.submodules += write

        m.d.comb += [
            write.addr.eq(read.addr),
            read.addr.eq(self.addr),
            write.data.eq(self.write_data),
            write.en.eq(self.write_en),
            self.read_data.eq(read.data),
        ]

        # Some memory configurations don't provide a read enabler
        if isinstance(read.en, Signal):
            m.d.comb += read.en.eq(self.read_en)

        return m
