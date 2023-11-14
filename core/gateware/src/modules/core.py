from amaranth import *
from enum import Enum

class Core(Elaboratable):
    class Stage(Enum):
        FETCH = 0
        EXECUTE = 1
        CHECK_MEMORY = 2
        HANDLE_MEMORY = 3

    # The size of a general/special-purpose register, and memory bus, in bits.
    DATA_WIDTH = 16

    def __init__(self, mem_addr: Signal, mem_read_data: Signal, mem_read_en: Signal, mem_write_data: Signal, mem_write_en: Signal):
        self.gprs = Array(Signal(Core.DATA_WIDTH) for _ in range(8))

        self.ip = Signal(Core.DATA_WIDTH)
        self.rp = Signal(Core.DATA_WIDTH)
        self.sp = Signal(Core.DATA_WIDTH)
        self.ef = Signal(Core.DATA_WIDTH)
        self.sprs = Array([self.ip, self.rp, self.sp, self.ef])

        self.stage = Signal(2, reset=Core.Stage.FETCH)

        # Memory interface signals
        self.mem_addr = mem_addr
        self.mem_read_data = mem_read_data
        self.mem_read_en = mem_read_en
        self.mem_write_data = mem_write_data
        self.mem_write_en = mem_write_en

        # Memory state tracking signals
        self.mem_read_pending = Signal()
        self.mem_read_pending_gpr = Signal(3)
        self.mem_write_pending = Signal()

    def elaborate(self, platform):
        m = Module()

        # Check if halted flag is set
        with m.If(self.ef[0].bool()):
            # If it is, do nothing!
            pass
        with m.Else():
            with m.If(self.stage == Core.Stage.FETCH.value):
                # Allows for memory latency of fetching the instruction
                # Set up memory read from IP
                # (HANDLE_MEMORY probably already did this, but this might be first stage executing)
                m.d.sync += self.mem_addr.eq(self.ip)
                self.read_en(m, 1)

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


                    # === Memory ===
                    with m.Case("0010 0000 0--- 0---"): # read
                        addr_gpr_idx = ins[4:7]
                        dest_gpr_idx = ins[0:3]
                        m.d.sync += self.mem_addr.eq(self.gprs[addr_gpr_idx])
                        m.d.sync += self.mem_read_pending.eq(C(1))
                        m.d.sync += self.mem_read_pending_gpr.eq(dest_gpr_idx)
                        # Don't need to set read-enable - it's normally on

                    with m.Case("0010 0000 1--- 0---"): # write
                        addr_gpr_idx = ins[4:7]
                        dest_gpr_idx = ins[0:3]

                        # Set to write, not read
                        self.read_en(m, 0)
                        m.d.sync += self.mem_write_en.eq(C(1))
                        
                        # Set up write
                        m.d.sync += self.mem_addr.eq(self.gprs[addr_gpr_idx])
                        m.d.sync += self.mem_write_pending.eq(C(1))
                        m.d.sync += self.mem_write_data.eq(self.gprs[dest_gpr_idx])
                        pass

                    # TODO: d_read, d_write


                    # === Special-Purpose Registers ===
                    with m.Case("0010 0001 10-- 0---"): # movso
                        dest_gpr_idx = ins[0:3]
                        src_spr_idx = ins[4:6]
                        m.d.sync += self.gprs[dest_gpr_idx].eq(self.sprs[src_spr_idx])

                    with m.Case("0010 0001 11-- 0---"): # movsi
                        src_gpr_idx = ins[0:3]
                        dest_spr_idx = ins[4:6]
                        m.d.sync += self.sprs[dest_spr_idx].eq(self.gprs[src_gpr_idx])

                    with m.Case("0010 0001 1101 1---"): # spadd
                        src_gpr_idx = ins[0:3]
                        m.d.sync += self.sp.eq(self.sp + self.gprs[dest_gpr_idx])

                    with m.Case("0010 0010 1101 0000"): # spinc
                        m.d.sync += self.sp.eq(self.sp + C(1))

                    with m.Case("0010 0010 1101 0001"): # spdec
                        m.d.sync += self.sp.eq(self.sp - C(1))


                    # === Exceptional Circumstances ===
                    with m.Default():
                        # TODO: consider trap?
                        pass

                # Advance stage
                m.d.sync += self.stage.eq(C(Core.Stage.CHECK_MEMORY.value))

            with m.Elif(self.stage == Core.Stage.CHECK_MEMORY.value):
                # TODO: I tried making this conditional on whether there's a pending read, but
                # that seemed to break things.
                # This probably suggests a problem somewhere else!
                m.d.sync += self.stage.eq(C(Core.Stage.HANDLE_MEMORY.value))

            with m.Elif(self.stage == Core.Stage.HANDLE_MEMORY.value):
                # If EXECUTE set up a pending memory read, then we now have the data
                # Copy into the target GPR and unset the flag
                with m.If(self.mem_read_pending):
                    m.d.sync += self.gprs[self.mem_read_pending_gpr].eq(self.mem_read_data)
                    m.d.sync += self.mem_read_pending.eq(C(0))

                # If EXECUTE set up a pending memory write, then it's done now!
                with m.Elif(self.mem_write_pending):
                    m.d.sync += self.mem_write_en.eq(C(0))
                    m.d.sync += self.mem_write_pending.eq(C(0))

                # Set up memory read for FETCH stage, since we trashed everything memory-related
                m.d.sync += self.mem_addr.eq(self.ip)
                self.read_en(m, 1)

                # Advance stage
                m.d.sync += self.stage.eq(C(Core.Stage.FETCH.value))

        return m

    def read_en(self, m: Module, value: int):
        # Set read enabler, if we have one - sometimes it's a constant 1, depending on the
        # memory configuration
        if isinstance(self.mem_read_en, Signal):
            m.d.sync += self.mem_read_en.eq(C(value))
