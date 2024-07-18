from amaranth import *
from amaranth.sim import Simulator, Settle
from ..src.modules.spi import SPI

def test_one_byte():
    """Tests emitting a single byte through the SPI interface."""
    # TODO: not actually a test, but handy for getting a waveform

    ticks_per_baud = 8

    copi = Signal()
    sclk = Signal()
    cs = Signal()
    spi = SPI(
        copi, cs, sclk,
        Signal(16), Signal(16), Signal(), Signal(16), Signal(),
    )
    sim = Simulator(spi)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        # Initial state
        assert (yield spi.is_sending) == 0
        assert (yield spi.has_finished) == 0
        # assert (yield cs) == 1

        # Put one byte in the buffer
        yield spi.mem_addr.eq(0x10) # Buffer[0]
        yield spi.mem_write_data.eq(ord('A'))
        yield spi.mem_write_en.eq(1)
        yield
        yield

        # Shift scale of 1
        yield spi.mem_addr.eq(0x2) # Clock scale
        yield spi.mem_write_data.eq(2)
        yield
        yield

        # Give length, and start send
        yield spi.mem_addr.eq(0x0) # Control
        #                      Send ---.
        #                              |.-----. Size
        yield spi.mem_write_data.eq(0b10000001)
        yield
        yield

        yield spi.mem_write_en.eq(0)

        # TODO
        for _ in range(100):
            yield
            
    sim.add_sync_process(proc)
    with sim.write_vcd("spi.vcd"):
        sim.run()
