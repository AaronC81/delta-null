from amaranth import *
from dataclasses import dataclass
from typing import List, Callable, Union

@dataclass
class Register:
    """
    A readable and writable memory location belonging to a peripheral.
    
    Reads are executed in the combinatorial domain, and writes are executed in the sequential
    domain.
    """

    # An address description identifying this register, as either an `int` or a `range`.
    address: Union[int, range]

    # A lambda which returns a signal for the result of the read.
    read: Callable

    # A lambda which returns a signal assignment (or array of them) to execute to perform the write.
    write: Callable

    def addresses(self):
        if isinstance(self.address, range):
            return self.address
        else:
            return [self.address]


class Peripheral(Elaboratable):
    """Base class for a memory-mapped peripheral."""

    def __init__(
        self,
        mem_addr: Signal, mem_read_data: Signal, mem_read_en: Signal, mem_write_data: Signal, mem_write_en: Signal,
    ):
        self.mem_addr = mem_addr
        self.mem_read_data = mem_read_data
        self.mem_read_en = mem_read_en
        self.mem_write_data = mem_write_data
        self.mem_write_en = mem_write_en

    def handle_registers(self, m: Module, registers: List[Register]):
        """Generate handler code for the given registers."""
        
        # Writers
        for register in registers:
            with m.If(self.mem_write_en):
                with m.Switch(self.mem_addr):
                    with m.Case(*register.addresses()):
                        m.d.sync += register.write()
        
        # Readers
        for register in registers:
            with m.If(self.mem_read_en):
                with m.Switch(self.mem_addr):
                    with m.Case(*register.addresses()):
                        m.d.comb += self.mem_read_data.eq(register.read())
        