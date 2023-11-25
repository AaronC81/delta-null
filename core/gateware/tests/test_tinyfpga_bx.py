from amaranth.sim import Simulator, Settle
from ..src.platform.tinyfpga_bx import TinyFPGABXMemoryMap, TinyFPGABXTop

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

def test_hcr_output():
    """Tests HCR mode and output words."""

    mem = TinyFPGABXMemoryMap(depth=10, init_ram=[0] * 10)
    sim = Simulator(mem)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        # Set pin 2 to output
        yield mem.addr.eq(0xF010)
        yield mem.write_data.eq(0b_0000_0000_0000_0100)
        yield mem.write_en.eq(1)
        yield
        yield mem.write_en.eq(0)
        yield

        # Now an output, but defaults low
        assert (yield mem.hcr_gpio_oe[2]) == 1
        assert (yield mem.hcr_gpio_o[2]) == 0

        # Pull pin 2 high
        yield mem.addr.eq(0xF012)
        yield mem.write_data.eq(0b_0000_0000_0000_0100)
        yield mem.write_en.eq(1)
        yield
        yield mem.write_en.eq(0)
        yield

        # Now high
        assert (yield mem.hcr_gpio_o[2]) == 1

    sim.add_sync_process(proc)
    sim.run()

def test_core_controlled_hcr():
    top = TinyFPGABXTop(
        instructions=[
            0x1110, 0x19F0, 0x1201, 0x1A00, 0x2092, 0x1112, 0x19F0, 0x2092, 0xFFFF
        ],
        depth=0x100,
    )
    sim = Simulator(top)
    sim.add_clock(1e-6) # 1 MHz

    def proc():
        # Run to end
        while (yield top.core.ef[0]) == 0:
            yield

        assert (yield top.mem.hcr_gpio_oe) == 0x1
        assert (yield top.mem.hcr_gpio_o) == 0x1

    sim.add_sync_process(proc)
    with sim.write_vcd("out.vcd"):
        sim.run()
