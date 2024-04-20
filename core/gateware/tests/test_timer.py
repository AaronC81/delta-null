from amaranth import *
from amaranth.sim import Simulator, Settle
from ..src.modules.timer import Timer

def test_timer():
    """Tests basic reading and writing."""

    ticks_per_us = 8

    timer = Timer(
        ticks_per_us,
        Signal(16), Signal(16), Signal(), Signal(16), Signal(),
    )
    sim = Simulator(timer)
    sim.add_clock(1e-6) # 1 MHz

    def test_timer(us_target):
        # Write 3us target
        yield timer.mem_addr.eq(0x02) # Target (low)
        yield timer.mem_write_data.eq(us_target)
        yield timer.mem_write_en.eq(1)
        yield
        yield
        assert (yield timer.microsecond_target) == us_target

        # Start the timer
        yield timer.mem_addr.eq(0x00) # Control register
        yield timer.mem_write_data.eq(1)
        yield
        yield
        assert (yield timer.is_running) == 1

        # Set up to read status
        yield timer.mem_write_en.eq(0)
        yield timer.mem_addr.eq(0x01) # Status register
        yield timer.mem_read_en.eq(1)
        yield

        # Wait for timer to fire
        elapsed_cycles = 0
        while True:
            print((yield timer.microsecond_counter), (yield timer.sub_microsecond_counter))
            status = (yield timer.mem_read_data)
            if status > 0:
                break
            yield
            elapsed_cycles += 1

        assert elapsed_cycles == (us_target * ticks_per_us)

    def proc():
        # Initial state
        assert (yield timer.is_running) == 0
        assert (yield timer.has_fired) == 0

        # Test multiple times, to ensure the timer resets its state properly
        yield from test_timer(5)
        yield from test_timer(10)
        yield from test_timer(7)
            
    sim.add_sync_process(proc)
    sim.run()
