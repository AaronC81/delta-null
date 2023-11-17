from amaranth import *
from enum import Enum
from .register_file import RegisterFile

class Core(Elaboratable):
    class Stage(Enum):
        FETCH = 0
        DECODE = 1
        READ_1_LATENCY = 2
        READ_1 = 3
        READ_2_LATENCY = 4
        READ_2 = 5
        EXECUTE = 6
        WRITE = 7

    # The size of a general/special-purpose register, and memory bus, in bits.
    DATA_WIDTH = 16

    def __init__(self, mem_addr: Signal, mem_read_data: Signal, mem_read_en: Signal, mem_write_data: Signal, mem_write_en: Signal):
        # SPRs
        self.ip = Signal(Core.DATA_WIDTH)
        self.rp = Signal(Core.DATA_WIDTH)
        self.sp = Signal(Core.DATA_WIDTH)
        self.ef = Signal(Core.DATA_WIDTH)
        self.sprs = Array([self.ip, self.rp, self.sp, self.ef])

        # Execution state
        self.instruction_buffer = Signal(Core.DATA_WIDTH)
        self.stage = Signal(3, reset=Core.Stage.FETCH)

        # Memory interface signals
        self.mem_addr = mem_addr
        self.mem_read_data = mem_read_data
        self.mem_read_en = mem_read_en
        self.mem_write_data = mem_write_data
        self.mem_write_en = mem_write_en

        # Register file
        self.gprs = RegisterFile(Core.DATA_WIDTH)

        # Supporting buffers for tracking data between stages
        self.immediate_buffer = Signal(8)
        self.reg_read_1_buffer = Signal(Core.DATA_WIDTH)
        # No read 1 idx - just gets whacked right into the memory bus
        self.reg_read_2_buffer = Signal(Core.DATA_WIDTH)
        self.reg_read_2_idx = Signal(3)
        self.reg_write_required = Signal()
        self.reg_write_idx = Signal(3)

    def elaborate(self, platform):
        m = Module()

        m.submodules += self.gprs

        # Check if halted flag is set
        with m.If(self.ef[0].bool()):
            # If it is, do nothing!
            pass
        with m.Else():
            with m.If(self.stage == Core.Stage.FETCH.value):
                # Allows for memory latency of fetching the instruction
                # Set up memory read from IP
                m.d.sync += self.mem_addr.eq(self.ip)
                self.eq_unless_constant(m, self.mem_read_en, 1)

                # Advance stage
                m.d.sync += self.stage.eq(C(Core.Stage.DECODE.value))
            
            with m.Elif(self.stage == Core.Stage.DECODE.value):
                # Advance IP, now that read has finished
                m.d.sync += self.ip.eq(self.ip + C(1))

                # Store instruction data in a buffer for later
                ins = self.mem_read_data
                m.d.sync += self.instruction_buffer.eq(ins)

                # TODO
                with m.Switch(ins):
                    # === Core ===
                    with m.Case("0000 0000 0000 0000", "1111 1111 1111 1111"): # nop, hlt
                        pass

                    with m.Case("0010 0001 0--- 0---"): # mov
                        m.d.sync += self.gprs.addr.eq(ins[4:7])
                        m.d.sync += self.reg_write_idx.eq(ins[0:3])
                        m.d.sync += self.reg_write_required.eq(1)

                    # TODO: d_mov


                    # === Immediate Loads ===
                    with m.Case("0001 0--- ---- ----", "0001 1--- ---- ----"): # putl, puth
                        m.d.sync += self.immediate_buffer.eq(ins[0:8])
                        m.d.sync += self.gprs.addr.eq(ins[8:11])
                        m.d.sync += self.reg_write_idx.eq(ins[8:11])
                        m.d.sync += self.reg_write_required.eq(1)


                    # === Exceptional Circumstances ===
                    with m.Default():
                        # TODO: consider trap?
                        pass

                # Set up read 1
                # If an address was required, it was already set in the individual case
                m.d.sync += self.gprs.read_en.eq(C(1))

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.READ_1_LATENCY.value))

            with m.Elif(self.stage == Core.Stage.READ_1_LATENCY):
                # Latency for memory read. Nothing happens here!
                # Just advance
                m.d.sync += self.stage.eq(C(Core.Stage.READ_1.value))

            with m.Elif(self.stage == Core.Stage.READ_1.value):
                # Read 1 is complete - copy data into buffer
                m.d.sync += self.reg_read_1_buffer.eq(self.gprs.read_data)

                # Set up read 2
                m.d.sync += [
                    self.gprs.addr.eq(self.reg_read_2_idx),
                    self.gprs.read_en.eq(C(1)),
                ]

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.READ_2_LATENCY.value))

            with m.Elif(self.stage == Core.Stage.READ_2_LATENCY):
                # Latency for memory read. Nothing happens here!
                # Just advance
                m.d.sync += self.stage.eq(C(Core.Stage.READ_2.value))

            with m.Elif(self.stage == Core.Stage.READ_2.value):
                # Read 2 is complete - copy data into buffer
                m.d.sync += [
                    self.reg_read_2_buffer.eq(self.gprs.read_data),
                    self.gprs.read_en.eq(C(0)),
                ]

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.EXECUTE.value))

            with m.Elif(self.stage == Core.Stage.EXECUTE.value):
                # Switch on buffered instruction
                with m.Switch(self.instruction_buffer):
                    # === Core ===
                    with m.Case("0000 0000 0000 0000"): # nop
                        pass

                    with m.Case("1111 1111 1111 1111"): # hlt
                        m.d.sync += self.ef[0].eq(C(1))

                    with m.Case("0010 0001 0--- 0---"): # mov
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer)

                    # TODO: d_mov


                    # === Immediate Loads ===
                    with m.Case("0001 0--- ---- ----"): # putl
                        m.d.sync += self.gprs.write_data.eq(
                            Cat(self.immediate_buffer, self.reg_read_1_buffer[8:16])
                        )

                    with m.Case("0001 1--- ---- ----"): # puth
                        m.d.sync += self.gprs.write_data.eq(
                            Cat(self.reg_read_1_buffer[0:8], self.immediate_buffer)
                        )

                # If write is required, set it up
                with m.If(self.reg_write_required):
                    # Buffer already contains data from instruction switch
                    m.d.sync += [
                        self.gprs.addr.eq(self.reg_write_idx),
                        self.gprs.write_en.eq(C(1)),
                    ]

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.WRITE.value))
                
            with m.Elif(self.stage == Core.Stage.WRITE.value):
                # Write, if any, has finished
                m.d.sync += [
                    self.gprs.write_en.eq(C(0)),
                    self.reg_write_required.eq(C(0)),
                ]

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.FETCH.value))

        return m

    def eq_unless_constant(self, m: Module, signal_or_const, value: int):
        # Set read enabler, if we have one - sometimes it's a constant 1, depending on the
        # memory configuration
        if isinstance(signal_or_const, Signal):
            m.d.sync += signal_or_const.eq(C(value))
