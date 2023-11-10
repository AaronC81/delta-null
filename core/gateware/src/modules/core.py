from amaranth import *
from enum import Enum

class Core(Elaboratable):
    class Stage(Enum):
        FETCH = 0
        EXECUTE = 1
        HANDLE_MEMORY = 2

    # The size of a general/special-purpose register, and memory bus, in bits.
    DATA_WIDTH = 16

    def __init__(self, mem_addr: Signal, mem_read_data: Signal, mem_read_en: Signal, mem_write_data: Signal, mem_write_en: Signal):
        self.gprs = Array(Signal(Core.DATA_WIDTH) for _ in range(8))

        self.ip = Signal(Core.DATA_WIDTH)
        self.rp = Signal(Core.DATA_WIDTH)
        self.sp = Signal(Core.DATA_WIDTH)
        self.ef = Signal(Core.DATA_WIDTH)

        self.stage = Signal(2, reset=Core.Stage.FETCH)

        self.mem_addr = mem_addr
        self.mem_read_data = mem_read_data
        self.mem_read_en = mem_read_en
        self.mem_write_data = mem_write_data
        self.mem_write_en = mem_write_en

    def elaborate(self, platform):
        m = Module()

        # Check if halted flag is set
        with m.If(self.ef[0].bool()):
            # If it is, do nothing!
            pass
        with m.Else():
            with m.If(self.stage == Core.Stage.FETCH.value):
                # Set up memory read from IP
                m.d.sync += self.mem_addr.eq(self.ip)

                # Set read enabler, if we have one - sometimes it's a constant 1, depending on the
                # memory configuration
                if isinstance(self.mem_read_en, Signal):
                    m.d.sync += self.mem_read_en.eq(C(1))

                # Advance stage
                m.d.sync += self.stage.eq(C(Core.Stage.EXECUTE.value))
            
            with m.Elif(self.stage == Core.Stage.EXECUTE.value):
                # Advance IP, now that read has finished
                m.d.sync += self.ip.eq(self.ip + C(1))

                ins = self.mem_read_data

                # TODO
                with m.Switch(ins):
                    # === Core ===
                    with m.Case("0000 0000 0000 0000"): # nop
                        pass

                    with m.Case("1111 1111 1111 1111"): # hlt
                        m.d.sync += self.ef[0].eq(C(1))

                    with m.Case("0010 0001 0--- 0---"): # mov
                        src_gpr_idx = ins[4:7]
                        dest_gpr_idx = ins[0:3]
                        m.d.sync += self.gprs[dest_gpr_idx].eq(self.gprs[src_gpr_idx])

                    with m.Case("0010 0001 0--- 10--"): # d_mov
                        pass # TODO


                    # === Immediate Loads ===
                    with m.Case("0001 0--- ---- ----"): # putl
                        src_gpr_idx = ins[8:11]
                        imm = ins[0:8]
                        m.d.sync += self.gprs[src_gpr_idx][0:8].eq(imm)

                    with m.Case("0001 1--- ---- ----"): # puth
                        src_gpr_idx = ins[8:11]
                        imm = ins[0:8]
                        m.d.sync += self.gprs[src_gpr_idx][8:16].eq(imm)


                    # === Exceptional Circumstances ===
                    with m.Default():
                        # TODO: consider trap?
                        pass

                # Advance stage
                m.d.sync += self.stage.eq(C(Core.Stage.HANDLE_MEMORY.value))

            with m.Elif(self.stage == Core.Stage.HANDLE_MEMORY.value):
                # TODO

                # Advance stage
                m.d.sync += self.stage.eq(C(Core.Stage.FETCH.value))

        return m

