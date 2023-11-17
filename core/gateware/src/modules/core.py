from amaranth import *
from enum import Enum
from .register_file import RegisterFile

class Core(Elaboratable):
    class Stage(Enum):
        FETCH = 0
        FETCH_LATENCY = 1
        DECODE = 2
        READ_1_LATENCY = 3
        READ_1 = 4
        READ_2_LATENCY = 5
        READ_2 = 6
        EXECUTE = 7
        WRITE = 8

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
        self.stage = Signal(4, reset=Core.Stage.FETCH)

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
                # Set up memory read from IP
                m.d.sync += self.mem_addr.eq(self.ip)
                self.eq_unless_constant(m, self.mem_read_en, 1)

                # Advance stage
                m.d.sync += self.stage.eq(C(Core.Stage.FETCH_LATENCY.value))

            with m.Elif(self.stage == Core.Stage.FETCH_LATENCY.value):
                # Allows for memory latency of instruction fetch
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


                    # TODO: Memory


                    # === Special-Purpose Registers ===
                    with m.Case("0010 0001 10-- 0---"): # movso
                        m.d.sync += self.reg_write_idx.eq(ins[0:3])
                        m.d.sync += self.reg_write_required.eq(1)

                    with m.Case("0010 0001 11-- 0---", "0010 0001 1101 1---"): # movsi, spadd
                        m.d.sync += self.gprs.addr.eq(ins[0:3])

                    with m.Case("0010 0010 1101 0000", "0010 0010 1101 0001"): # spinc, spdec
                        pass


                    # === Bit Manipulation ===
                    with m.Case("0100 0000 0000 0---"): # not
                        m.d.sync += self.gprs.addr.eq(ins[0:3])
                        m.d.sync += self.reg_write_idx.eq(ins[0:3])
                        m.d.sync += self.reg_write_required.eq(1)

                    with m.Case(
                        "0100 0001 0--- 0---", # and
                        "0100 0010 0--- 0---", # or
                        "0100 0011 0--- 0---", # xor
                        "0100 0100 0--- 0---", # shl
                        "0100 0101 0--- 0---", # shr
                    ):
                        m.d.sync += self.gprs.addr.eq(ins[0:3])
                        m.d.sync += self.reg_read_2_idx.eq(ins[4:7])
                        m.d.sync += self.reg_write_idx.eq(ins[0:3])
                        m.d.sync += self.reg_write_required.eq(1)


                    # === Comparison ===
                    with m.Case("0101 0000 0000 0000"): # inv
                        pass

                    with m.Case("0101 0000 0001 0---"): # eqz
                        m.d.sync += self.gprs.addr.eq(ins[0:3])

                    with m.Case(
                        "0101 0001 0--- 0---", # eq
                        "0101 0010 0--- 0---", # gt
                        "0101 0011 0--- 0---", # gteq
                    ):
                        m.d.sync += self.gprs.addr.eq(ins[0:3])
                        m.d.sync += self.reg_read_2_idx.eq(ins[4:7])


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


                    # TODO: Memory


                    # === Special-Purpose Registers ===
                    with m.Case("0010 0001 10-- 0---"): # movso
                        spr_idx = self.instruction_buffer[4:6]
                        m.d.sync += self.gprs.write_data.eq(self.sprs[spr_idx])

                    with m.Case("0010 0001 11-- 0---"): # movsi
                        spr_idx = self.instruction_buffer[4:6]
                        m.d.sync += self.sprs[spr_idx].eq(self.reg_read_1_buffer)

                    with m.Case("0010 0001 1101 1---"): # spadd
                        m.d.sync += self.sp.eq(self.sp + self.reg_read_1_buffer)

                    with m.Case("0010 0010 1101 0000"): # spinc
                        m.d.sync += self.sp.eq(self.sp + C(1))

                    with m.Case("0010 0010 1101 0001"): # spdec
                        m.d.sync += self.sp.eq(self.sp - C(1))


                    # === Bit Manipulation ===
                    with m.Case("0100 0000 0000 0---"): # not
                        m.d.sync += self.gprs.write_data.eq(~self.reg_read_1_buffer)

                    with m.Case("0100 0001 0--- 0---"): # and
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer & self.reg_read_2_buffer)

                    with m.Case("0100 0010 0--- 0---"): # or
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer | self.reg_read_2_buffer)

                    with m.Case("0100 0011 0--- 0---"): # xor
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer ^ self.reg_read_2_buffer)

                    with m.Case("0100 0100 0--- 0---"): # shl
                        # Amaranth's left-shift produces a ludicrously large result if we use the
                        # operand register at its full size.
                        # Instead, we know that a shift of 16 or larger always produces 0, and for
                        # a shift of less, we only need to look at the lowest 4 bits.
                        with m.If(self.reg_read_2_buffer < C(16)):
                            m.d.sync += self.gprs.write_data \
                                .eq(self.reg_read_1_buffer << self.reg_read_2_buffer[0:4])
                        with m.Else():
                            m.d.sync += self.gprs.write_data.eq(0)

                    with m.Case("0100 0101 0--- 0---"): # shr
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer >> self.reg_read_2_buffer)


                    # === Comparison ===
                    with m.Case("0101 0000 0000 0000"): # inv
                        m.d.sync += self.ef[1].eq(~self.ef[1])

                    with m.Case("0101 0000 0001 0---"): # eqz
                        m.d.sync += self.ef[1].eq(self.reg_read_1_buffer == C(0))

                    with m.Case("0101 0001 0--- 0---"): # eq
                        m.d.sync += self.ef[1].eq(self.reg_read_2_buffer == self.reg_read_1_buffer)

                    with m.Case("0101 0010 0--- 0---"): # gt
                        m.d.sync += self.ef[1].eq(self.reg_read_2_buffer > self.reg_read_1_buffer)

                    with m.Case("0101 0011 0--- 0---"): # gteq
                        m.d.sync += self.ef[1].eq(self.reg_read_2_buffer >= self.reg_read_1_buffer)


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
