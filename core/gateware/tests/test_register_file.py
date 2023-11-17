from amaranth.sim import Simulator, Settle
from ..src.modules.register_file import RegisterFile
from ..src.modules.core import Core

def test_read_write():
    """Tests basic reading and writing."""

    reg = RegisterFile(Core.DATA_WIDTH)
    sim = Simulator(reg)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        # Read r1 - should be initialised to zero
        yield reg.addr.eq(1)
        yield reg.read_en.eq(1)
        yield
        assert (yield reg.read_data) == 0

        # Write to r1
        yield reg.write_data.eq(0x1234)
        yield reg.write_en.eq(1)
        yield

        # Check r1
        yield reg.write_en.eq(0)
        yield reg.read_en.eq(1)
        yield
        assert (yield reg.read_data) == 0x1234

    sim.add_sync_process(proc)
    sim.run()

def test_all():
    """Tests reading and writing with all registers."""

    reg = RegisterFile(Core.DATA_WIDTH)
    sim = Simulator(reg)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        # Write all
        yield reg.write_en.eq(1)
        for i in range(8):
            yield reg.addr.eq(i)
            yield reg.write_data.eq(i**5) # roughly approximates smallest to largest values
            yield

        # Read all
        yield reg.write_en.eq(0)
        yield reg.read_en.eq(1)
        for i in range(8):
            yield reg.addr.eq(i)
            yield
            yield
            assert (yield reg.read_data) == i**5

    sim.add_sync_process(proc)
    sim.run()
