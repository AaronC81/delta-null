from amaranth import *
from amaranth.sim import Simulator, Settle
from ..src.modules.uart_logger import UartLogger

def test_one_byte():
    """Tests emitting a single byte through the logger."""

    ticks_per_baud = 8

    data_out = Signal()
    uart = UartLogger(
        ticks_per_baud,
        data_out,
        Signal(16), Signal(16), Signal(), Signal(16), Signal(),
    )
    sim = Simulator(uart)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        for _ in range(5):
            # Initial state
            assert (yield uart.is_sending) == 0
            assert (yield uart.has_finished) == 0

            # Put one byte in the buffer
            yield uart.mem_addr.eq(0x10) # Buffer[0]
            yield uart.mem_write_data.eq(ord('A'))
            yield uart.mem_write_en.eq(1)
            yield
            yield

            # Give length, and start send
            yield uart.mem_addr.eq(0x0) # Control
            #                      Send ---.
            #                              |.-----. Size
            yield uart.mem_write_data.eq(0b10000001)
            yield
            yield

            # Set up to read status
            yield uart.mem_write_en.eq(0)
            yield uart.mem_addr.eq(0x01) # Status
            yield uart.mem_read_en.eq(1)
            yield

            # Wait for send to complete, watching data as we go (sampling on "clock edge")
            count = 0
            out = []
            while True:
                status = (yield uart.mem_read_data)
                if count % ticks_per_baud == 0:
                    out.append((yield data_out))
                if status > 0:
                    break
                yield
                count += 1

            # Check that sent waveform matches expected pattern
            assert out == [
                0, # Start bit
                1, # \
                0, #  |
                0, #  |
                0, #  | A
                0, #  |
                0, #  |
                1, #  |
                0, # /
                1, # Stop bit
                0, # Start bit
                0, # \
                0, #  |
                0, #  |
                0, #  | nul
                0, #  |
                0, #  |
                0, #  |
                0, # /
                1, # Stop bit
            ]

            # Reset status
            yield uart.mem_addr.eq(0x1) # Status
            yield uart.mem_write_data.eq(0)
            yield uart.mem_write_en.eq(1)
            yield
            yield
            yield uart.mem_write_en.eq(0)
            
    sim.add_sync_process(proc)
    with sim.write_vcd("uart.vcd"):
        sim.run()
