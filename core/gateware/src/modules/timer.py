from amaranth import *

class Timer(Elaboratable):


    # NOTE: Input address should start at 0 (i.e. pre-compensated for position in memory map)
        
    def __init__(
        self,
        ticks_per_microsecond: int,
        mem_addr: Signal, mem_read_data: Signal, mem_read_en: Signal, mem_write_data: Signal, mem_write_en: Signal,
    ):
        # Static configuration
        self.ticks_per_microsecond = ticks_per_microsecond

        # Dynamic configuration registers
        self.microsecond_target = Signal(32)
        self.control_register = Signal(2)
        self.has_fired = Signal()

        # Microsecond counter
        self.microsecond_counter = Signal(32)

        # Internal divider
        self.sub_microsecond_counter = Signal(16)

        # Memory interface signals
        self.mem_addr = mem_addr
        self.mem_read_data = mem_read_data
        self.mem_read_en = mem_read_en
        self.mem_write_data = mem_write_data
        self.mem_write_en = mem_write_en

    def elaborate(self, platform):
        m = Module()
        
        with m.If(self.is_running()):
            # Use the internal counter to create a clock divider, based on the number of ticks per
            # microsecond
            m.d.sync += self.sub_microsecond_counter.eq(self.sub_microsecond_counter + 1)
            with m.If(self.sub_microsecond_counter == self.ticks_per_microsecond):
                with m.If(self.microsecond_counter + 1 == self.microsecond_target):
                    # If this increment means we'll hit our target, fire the timer!
                    m.d.sync += self.has_fired.eq(1)

                    with m.If(self.is_repeating()):
                        # If this timer is repeating, reset the state and keep running
                        m.d.sync += [
                            self.microsecond_counter.eq(0),
                            self.sub_microsecond_counter.eq(1),
                        ]
                    with m.Else():
                        # If it's not repeating, stop
                        m.d.sync += self.is_running().eq(0)
                with m.Else():
                    # Just keep incrementing
                    m.d.sync += [
                        self.microsecond_counter.eq(self.microsecond_counter + 1),
                        self.sub_microsecond_counter.eq(1),
                    ]

        
        # Handle memory access
        with m.If(self.mem_write_en):
            with m.Switch(self.mem_addr):
                with m.Case(0x00): # Control register
                    # Writing to the control register resets the state of the timer, too
                    m.d.sync += [
                        self.control_register.eq(self.mem_write_data),
                        self.microsecond_counter.eq(0),
                        self.sub_microsecond_counter.eq(1),
                        self.has_fired.eq(0),
                    ]
                with m.Case(0x01): # Status register
                    # Regardless of value, reset state on write
                    m.d.sync += self.has_fired.eq(0)
                with m.Case(0x02): # Target (low)
                    m.d.sync += self.microsecond_target[0:16].eq(self.mem_write_data)
                with m.Case(0x03): # Target (high)
                    m.d.sync += self.microsecond_target[16:32].eq(self.mem_write_data)
        with m.Elif(self.mem_read_en):
            with m.Switch(self.mem_addr):
                with m.Case(0x00): # Control register
                    m.d.comb += self.mem_read_data.eq(self.control_register)
                with m.Case(0x01): # Status register - can't write to this!
                    m.d.comb += self.mem_read_data.eq(self.has_fired)
                with m.Case(0x02): # Target (low)
                    m.d.comb += self.mem_read_data.eq(self.microsecond_target[0:16])
                with m.Case(0x03): # Target (high)
                    m.d.comb += self.mem_read_data.eq(self.microsecond_target[16:32])

        return m
    
    def is_running(self):
        return self.control_register[0]

    def is_repeating(self):
        return self.control_register[1]
