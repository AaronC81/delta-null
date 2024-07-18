from amaranth import *
import math
from .core import Core
from .peripheral import Peripheral, Register

class SPI(Peripheral):
    DATA_BUFFER_SIZE = 64

    # The range of addresses, from the beginning of the SPI peripheral, which should be directed to it.
    LOGGER_SIZE = 0xFF

    def __init__(
        self,
        copi: Signal, cs: Signal, sclk: Signal,
        mem_addr: Signal, mem_read_data: Signal, mem_read_en: Signal, mem_write_data: Signal, mem_write_en: Signal,
    ):
        super().__init__(mem_addr, mem_read_data, mem_read_en, mem_write_data, mem_write_en)

        # SPI signals
        self.copi = copi
        self.cs = cs
        self.sclk = sclk

        # Configuration registers
        self.clock_scale = Signal(Core.DATA_WIDTH)
        self.data_length = Signal(math.ceil(math.log2(SPI.DATA_BUFFER_SIZE*2)))
        self.data_buffer = Array([Signal(Core.DATA_WIDTH) for _ in range(SPI.DATA_BUFFER_SIZE)])

        # State
        self.has_finished = Signal()
        self.is_sending = Signal()
        self.send_buffer_byte_index = Signal(math.ceil(math.log2(SPI.DATA_BUFFER_SIZE * 2)) + 1) # *not* word index
        self.send_buffer_bit_index = Signal(math.ceil(math.log2(8)) + 1)

        # Tick counter
        self.tick_counter = Signal(32)

    def elaborate(self, platform) -> Module:
        m = Module()

        with m.If(self.is_sending):
            # Count towards tick target
            m.d.sync += self.tick_counter.eq(self.tick_counter + 1)

            # If reached, we need to do something...
            with m.If(self.tick_counter >= Cat(C(0), self.clock_scale)):
                # If the clock is down, flip it up - the right bit is already on COPI
                # Otherwise, we need to advance to the next bit
                with m.If(self.sclk == 0):
                    m.d.sync += [
                        self.sclk.eq(1),
                        self.tick_counter.eq(0),
                    ]
                with m.Else():
                    # Reset counter, ready for the next bit
                    m.d.sync += self.tick_counter.eq(0)

                    # Pull clock low
                    m.d.sync += self.sclk.eq(0)

                    m.d.sync += self.send_buffer_bit_index.eq(self.send_buffer_bit_index + 1)

                    # If we just sent the final bit of this byte...
                    with m.If(self.send_buffer_bit_index == C(0)):
                        # If we just sent the final byte, then we're done!
                        with m.If(self.send_buffer_byte_index + C(1) == self.data_length):
                            m.d.sync += [
                                self.has_finished.eq(1),
                                self.is_sending.eq(0),
                            ]
                        with m.Else():
                            # Increment byte index
                            m.d.sync += self.send_buffer_byte_index.eq(self.send_buffer_byte_index + 1)
                            
                            # Move back to first bit
                            m.d.sync += self.send_buffer_bit_index.eq(7)
                    with m.Else():
                        # Otherwise, move along
                        m.d.sync += self.send_buffer_bit_index.eq(self.send_buffer_bit_index - 1)

            # Read bit from buffer
            word_index = self.send_buffer_byte_index >> 1
            word = self.data_buffer[word_index]
            use_high_byte = (self.send_buffer_byte_index % 2) == C(1)

            with m.If(use_high_byte):
                m.d.sync += self.copi.eq(word.bit_select(self.send_buffer_bit_index + 8, 1))
            with m.Else():
                m.d.sync += self.copi.eq(word.bit_select(self.send_buffer_bit_index, 1))
        with m.Else():
            # Release idle peripheral
            m.d.sync += self.cs.eq(1)

        self.handle_registers(m, [
            # Control register
            Register(
                address=0x00,
                read=lambda: Cat(self.data_length, self.is_sending),
                write=lambda: [
                    # Writing to the control register resets the state, too
                    # TODO
                    self.data_length.eq(self.mem_write_data[0:7]),
                    self.is_sending.eq(self.mem_write_data[7]),
                    self.has_finished.eq(0),
                    self.cs.eq(0), # Chip select
                    self.send_buffer_bit_index.eq(7),
                    self.send_buffer_byte_index.eq(0),
                ],
            ),

            # Status register
            Register(
                address=0x01,
                read=lambda: self.has_finished,
                write=lambda: self.has_finished.eq(0) # Regardless of value, reset state on write
            ),

            # Clock scale register
            Register(
                address=0x02,
                read=lambda: self.clock_scale,
                write=lambda: self.clock_scale.eq(self.mem_write_data),
            ),

            # Buffer
            Register(
                address=range(0x10, 0x10 + SPI.DATA_BUFFER_SIZE),
                read=lambda: self.data_buffer[self.mem_addr - 0x10],
                write=lambda: self.data_buffer[self.mem_addr - 0x10].eq(self.mem_write_data),
            )
        ])

        return m