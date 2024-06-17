from amaranth import *
import math
from .core import Core

class UartLogger(Elaboratable):
    DATA_BUFFER_SIZE = 64

    # The range of addresses, from the beginning of the logger, which should be directed to it.
    LOGGER_SIZE = 0xFF

    def __init__(
        self,
        ticks_per_baud: int,
        data_out: Signal,
        mem_addr: Signal, mem_read_data: Signal, mem_read_en: Signal, mem_write_data: Signal, mem_write_en: Signal,
    ):
        # Static configuration
        self.ticks_per_baud = ticks_per_baud

        # Dynamic configuration registers
        self.data_length = Signal(math.ceil(math.log2(UartLogger.DATA_BUFFER_SIZE+1)))
        self.data_buffer = Array([Signal(Core.DATA_WIDTH) for _ in range(UartLogger.DATA_BUFFER_SIZE)])

        # State
        self.has_finished = Signal()
        self.is_sending = Signal()
        self.send_buffer_byte_index = Signal(math.ceil(math.log2(UartLogger.DATA_BUFFER_SIZE * 2)) + 1) # *not* word index
        self.send_buffer_bit_index = Signal(math.ceil(math.log2(8)) + 1)

        # Tick counter
        self.tick_counter = Signal(32)

        # Memory interface signals
        self.mem_addr = mem_addr
        self.mem_read_data = mem_read_data
        self.mem_read_en = mem_read_en
        self.mem_write_data = mem_write_data
        self.mem_write_en = mem_write_en
    
        # Protocol interface signals
        self.data_out = data_out

        # Debugging
        self.virtual_clock = Signal()

    def elaborate(self, platform):
        m = Module()
        
        with m.If(self.is_sending):
            # Count towards tick target
            m.d.sync += self.tick_counter.eq(self.tick_counter + 1)

            # If reached, we need to change the bit we're sending
            with m.If(self.tick_counter == self.ticks_per_baud - 1):
                # Flip virtual clock, for debugging
                m.d.sync += self.virtual_clock.eq(~self.virtual_clock)

                # Reset counter, ready for the next bit
                m.d.sync += self.tick_counter.eq(0)

                # If we just sent the final bit of this byte - after the stop bit...
                with m.If(self.send_buffer_bit_index == C(8)):
                    # If we just sent the final byte, then we're done!
                    with m.If(self.send_buffer_byte_index + C(1) == self.data_length * 2):
                        m.d.sync += [
                            self.has_finished.eq(1),
                            self.is_sending.eq(0),
                        ]
                    with m.Else():
                        # Increment byte index
                        m.d.sync += self.send_buffer_byte_index.eq(self.send_buffer_byte_index + 1)
                        
                        # Move back to start bit
                        m.d.sync += self.send_buffer_bit_index.eq(10)
                with m.Elif(self.send_buffer_bit_index == C(10)):
                    # This was the start bit - begin real transmission now
                    m.d.sync += self.send_buffer_bit_index.eq(0)
                with m.Else():
                    # Otherwise, move along
                    m.d.sync += self.send_buffer_bit_index.eq(self.send_buffer_bit_index + 1)

            # Bit index 8 is a special marker for the stop bit
            with m.If(self.send_buffer_bit_index == C(8)):
                m.d.sync += self.data_out.eq(1)
            with m.Else():
                # Read bit from buffer
                word_index = self.send_buffer_byte_index >> 1
                word = self.data_buffer[word_index]
                use_high_byte = (self.send_buffer_byte_index % 2) == C(1)

                with m.If(use_high_byte):
                    m.d.sync += self.data_out.eq(word.bit_select(self.send_buffer_bit_index + 8, 1))
                with m.Else():
                    m.d.sync += self.data_out.eq(word.bit_select(self.send_buffer_bit_index, 1))
        with m.Else():
            m.d.sync += self.data_out.eq(1) # Idle high
        
        # Handle memory access
        with m.If(self.mem_write_en):
            with m.Switch(self.mem_addr):
                with m.Case(0x00): # Control register
                    # Writing to the control register resets the state, too
                    m.d.sync += [
                        self.data_length.eq(self.mem_write_data[0:7]),
                        self.is_sending.eq(self.mem_write_data[7]),
                        self.has_finished.eq(0),
                        self.data_out.eq(0), # Start bit
                        self.send_buffer_bit_index.eq(10), # Magic marker for start bit
                        self.send_buffer_byte_index.eq(0),
                    ]
                with m.Case(0x01): # Status register
                    # Regardless of value, reset state on write
                    m.d.sync += self.has_finished.eq(0)
                with m.Case(*range(0x10, 0x10 + UartLogger.DATA_BUFFER_SIZE)): # Buffer
                    m.d.sync += self.data_buffer[self.mem_addr - 0x10].eq(self.mem_write_data)
        with m.Elif(self.mem_read_en):
            with m.Switch(self.mem_addr):
                with m.Case(0x00): # Control register
                    m.d.comb += self.mem_read_data.eq(Cat(self.data_length, self.is_sending))
                with m.Case(0x01): # Status register
                    m.d.comb += self.mem_read_data.eq(self.has_finished)
                with m.Case(*range(0x10, 0x10 + UartLogger.DATA_BUFFER_SIZE)): # Buffer
                    m.d.comb += self.mem_read_data.eq(self.data_buffer[self.mem_addr - 0x10])

        return m
