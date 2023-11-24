from amaranth.sim import Simulator, Settle
from ..src.platform.tinyfpga_bx import TinyFPGABXMemoryMap

def test_ram():
    """Tests reading and writing with RAM."""

    mem = TinyFPGABXMemoryMap(depth=10, init_ram=[0] * 10)
    sim = Simulator(mem)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        # Read 0x1003 - should be initialised to zero
        yield mem.addr.eq(0x1003)
        yield mem.read_en.eq(1)
        yield
        assert (yield mem.read_data) == 0

        # Write
        yield mem.write_data.eq(0x1234)
        yield mem.write_en.eq(1)
        yield

        # Read again
        yield mem.write_en.eq(0)
        yield mem.read_en.eq(1)
        yield
        assert (yield mem.read_data) == 0x1234

        # Check that memory mapping has applied - 0x1003 is actually 0x3 in RAM
        assert (yield mem.ram[3]) == 0x1234

    sim.add_sync_process(proc)
    sim.run()


def test_hcr_metadata():
    """Tests reading metadata from the HCR."""

    mem = TinyFPGABXMemoryMap(depth=10, init_ram=[0] * 10)
    sim = Simulator(mem)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        # Read magic number
        yield mem.addr.eq(0xF000)
        yield mem.read_en.eq(1)
        yield
        assert (yield mem.read_data) == 0xF90A

        # Read harness indicator
        yield mem.addr.eq(0xF001)
        yield mem.read_en.eq(1)
        yield
        assert (yield mem.read_data) == 0

    sim.add_sync_process(proc)
    sim.run()

