from amaranth.sim import Simulator
from .helpers import run_sim, assemble
from ..src.modules.harness import CoreSimHarness
from ..src.modules.core import Core

def test_core_stage_progression():
    """Tests low-level core-internal stage progression."""

    core = CoreSimHarness(assemble("putl r1, 0x12"))
    sim = Simulator(core)
    sim.add_clock(1e-6) # 1 MHz

    R1_INDEX = 1

    def proc():
        # Set r1 to non-default starting value
        yield core.core.gprs._mem[1].eq(0xABCD)

        # === FETCH ===
        yield
        assert (yield core.core.instruction_buffer) == 0

        # === DECODE ===
        yield
        # Instruction buffer ready
        assert (yield core.core.instruction_buffer) == 0x1112 
        # Register indexes decoded, and GPR read initiated
        assert (yield core.core.gprs.read_en)
        assert (yield core.core.gprs.addr) == R1_INDEX
        assert (yield core.core.reg_write_idx) == R1_INDEX
        assert (yield core.core.reg_write_required)

        # === READ_1_LATENCY ===
        yield
        # Nothing interesting happens here!

        # === READ_1 ===
        yield
        # Read 1 should be finished
        assert (yield core.core.reg_read_1_buffer) == 0xABCD

        # === READ_2_LATENCY ===
        yield
        # Nothing interesting happens here!

        # === READ_2 ===
        yield
        # Nothing interesting happens here, either - only one read for this instruction!

        # === EXECUTE ===
        yield
        # Write initiated with correct result
        assert (yield core.core.gprs.write_en)
        assert (yield core.core.gprs.addr) == R1_INDEX
        assert (yield core.core.gprs.write_data) == 0xAB12

        # === WRITE ===
        yield
        # Nothing interesting

        # === FETCH (again) ===
        yield
        # GPR write should have finished
        assert (yield core.core.gprs._mem[1]) == 0xAB12

    sim.add_sync_process(proc)
    sim.run()

