from amaranth import *
from enum import Enum
from .register_file import RegisterFile
from typing import Optional

class Core(Elaboratable):
    class Stage(Enum):
        """The stages of execution which take place to completely execute an instruction.
        
        See the comments in `elaborate` for documentation on the purpose of each stage.
        """

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

    def __init__(
        self,
        mem_addr: Signal, mem_read_data: Signal, mem_read_en: Signal, mem_write_data: Signal, mem_write_en: Signal,
        initial_ip: int = 0, initial_sp: int = 0,
        debug_led: Optional[Signal] = None,
    ):
        # SPRs
        self.ip = Signal(Core.DATA_WIDTH, reset=initial_ip)
        self.rp = Signal(Core.DATA_WIDTH)
        self.sp = Signal(Core.DATA_WIDTH, reset=initial_sp)
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

        # Special trackers used for specific instructions
        self.instruction_is_read = Signal()
        self.instruction_is_write = Signal()
        self.instruction_is_spread = Signal()
        self.instruction_is_spwrite = Signal()

        # Debugging stuff
        self.debug_led = debug_led

    def elaborate(self, platform):
        m = Module()

        m.submodules.gprs = self.gprs

        # Check if halted flag is set
        with m.If(self.ef[0].bool()):
            # If it is, do nothing!
            pass
        with m.Else():
            # FETCH:
            #   Initiates a read from main memory, from the address pointed to by IP, in order to
            #   retrieve the next instruction to execute.
            with m.If(self.stage == Core.Stage.FETCH.value):
                # Set up memory read from IP
                m.d.sync += self.mem_addr.eq(self.ip)
                self.eq_unless_constant(m, self.mem_read_en, 1)

                # Advance stage
                m.d.sync += self.stage.eq(C(Core.Stage.FETCH_LATENCY.value))

            # FETCH_LATENCY:
            #   Does nothing. Provides memory read latency after `FETCH`.
            with m.Elif(self.stage == Core.Stage.FETCH_LATENCY.value):
                # Allows for memory latency of instruction fetch
                m.d.sync += self.stage.eq(C(Core.Stage.DECODE.value))
            
            # DECODE:
            #   Retrieves the instruction which was `FETCH`ed, and sets up state for the upcoming
            #   stages.
            #
            #   Any instruction can read up to two GPRs, and write up to one GPR:
            #   - If the decoded instruction needs to read one GPR, this stage initiates a read
            #     from the GPR file, which will complete in `READ_1`.
            #   - If it needs to read a second register, the index is saved into `reg_read_2_idx`.
            #     `READ_1` will initiate this second read, and it will complete in `READ_2`.
            #   - If the decoded instruction needs to write back to a GPR, the index is saved into
            #     `reg_write_idx`, and `reg_write_required` is set.
            #
            #   The `read` and `write` instructions require special attention because they access
            #   main memory, so set special `instruction_is_read|write` flags here, which override
            #   future step behaviour. This is mentioned later where relevant.
            #
            #   SPRs are `Signal`s, so their access is immediate, and not considered here.
            with m.Elif(self.stage == Core.Stage.DECODE.value):
                # Advance IP, now that read has finished
                m.d.sync += self.ip.eq(self.ip + C(1))

                # Store instruction data in a buffer for later
                ins = self.mem_read_data
                m.d.sync += self.instruction_buffer.eq(ins)

                # Reset some stuff
                m.d.sync += self.instruction_is_read.eq(C(0))
                m.d.sync += self.instruction_is_write.eq(C(0))

                # TODO
                with m.Switch(ins):
                    # === Core ===
                    with m.Case("0000 0000 0000 0000", "1111 1111 1111 1111"): # nop, hlt
                        pass

                    with m.Case("0010 0001 0--- 0---"): # mov
                        self.decode(m, read_1=ins[4:7], write=ins[0:3])

                    # TODO: d_mov


                    # === Immediate Loads ===
                    with m.Case("0001 0--- ---- ----", "0001 1--- ---- ----"): # putl, puth
                        self.decode(m, read_1=ins[8:11], write=ins[8:11], imm=ins[0:8])


                    # === Memory ===
                    with m.Case("0010 0000 0--- 0---"): # read
                        self.decode(m, read_1=ins[4:7], write=ins[0:3])
                        m.d.sync += self.instruction_is_read.eq(1)

                    with m.Case("0010 0000 1--- 0---"): # write
                        self.decode(m, read_1=ins[4:7], read_2=ins[0:3])
                        m.d.sync += self.instruction_is_write.eq(1)

                    with m.Case("0011 0000 0--- ----"): # spread
                        self.decode(m, write=ins[4:7])
                        m.d.sync += [
                            self.immediate_buffer.eq(ins[0:4]),
                            self.instruction_is_spread.eq(1),
                        ]

                    with m.Case("0011 0000 1--- ----"): # spwrite
                        self.decode(m, read_1=ins[4:7])
                        m.d.sync += [
                            self.immediate_buffer.eq(ins[0:4]),
                            self.instruction_is_spwrite.eq(1),
                        ]


                    # === Special-Purpose Registers ===
                    with m.Case("0010 0001 10-- 0---"): # movso
                        self.decode(m, write=ins[0:3])

                    with m.Case("0010 0001 11-- 0---", "0010 0001 1101 1---"): # movsi, spadd
                        self.decode(m, read_1=ins[0:3])

                    with m.Case("0010 0010 1101 0000", "0010 0010 1101 0001"): # spinc, spdec
                        pass


                    # === Bit Manipulation ===
                    with m.Case("0100 0000 0000 0---"): # not
                        self.decode(m, read_1=ins[0:3], write=ins[0:3])

                    with m.Case(
                        "0100 0001 0--- 0---", # and
                        "0100 0010 0--- 0---", # or
                        "0100 0011 0--- 0---", # xor
                        "0100 0100 0--- 0---", # shl
                        "0100 0101 0--- 0---", # shr
                    ):
                        self.decode(m, read_1=ins[0:3], read_2=ins[4:7], write=ins[0:3])


                    # === Comparison ===
                    with m.Case("0101 0000 0000 0000"): # inv
                        pass

                    with m.Case("0101 0000 0001 0---"): # eqz
                        self.decode(m, read_1=ins[0:3])

                    with m.Case(
                        "0101 0001 0--- 0---", # eq
                        "0101 0010 0--- 0---", # gt
                        "0101 0011 0--- 0---", # gteq
                    ):
                        self.decode(m, read_1=ins[0:3], read_2=ins[4:7])


                    # === General-Purpose Arithmetic ===
                    with m.Case(
                        "0100 1000 0000 0---", # neg
                        "0100 1000 0001 0---", # inc
                        "0100 1000 0010 0---", # dec
                    ):
                        self.decode(m, read_1=ins[0:3], write=ins[0:3])

                    with m.Case(
                        "0100 1001 0--- 0---", # add
                        "0100 1010 0--- 0---", # sub
                        "0100 1011 0--- 0---", # mul
                    ):
                        self.decode(m, read_1=ins[0:3], read_2=ins[4:7], write=ins[0:3])


                    # === Branching ===
                    with m.Case("0110 0000 ---- ----", "0110 0001 ---- ----"): # jmpoff, # cjmpoff
                        self.decode(m, imm=ins[0:8])

                    with m.Case("0110 0011 0000 0---", "0110 0010 0001 0---"): # cjmp, call
                        self.decode(m, read_1=ins[0:3])

                    with m.Case("0110 0010 0001 1000"): # ret
                        pass


                    # === Exceptional Circumstances ===
                    with m.Default():
                        # TODO: consider trap?
                        pass

                # Set up read 1
                # If an address was required, it was already set in the individual case
                m.d.sync += self.gprs.read_en.eq(C(1))

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.READ_1_LATENCY.value))

            # READ_1_LATENCY:
            #   Does nothing. Provides memory read latency before `READ_1`.
            with m.Elif(self.stage == Core.Stage.READ_1_LATENCY):
                # Latency for memory read. Nothing happens here!
                # Just advance
                m.d.sync += self.stage.eq(C(Core.Stage.READ_1.value))

            # READ_1:
            #   Retrieves data read from the first decoded GPR, and initiates a read for the second
            #   which will complete in `READ_2`.
            #
            #   If the instruction is `read`, this initiates a read from main memory instead. This
            #   can happen now because the address is now known, as it was read from that first
            #   GPR.
            with m.Elif(self.stage == Core.Stage.READ_1.value):
                # Read 1 is complete - copy data into buffer
                m.d.sync += self.reg_read_1_buffer.eq(self.gprs.read_data)

                with m.If(self.instruction_is_read):
                    # Set up main memory read, from address we just read
                    m.d.sync += self.mem_addr.eq(self.gprs.read_data)
                    self.eq_unless_constant(m, self.mem_read_en, 1)
                with m.Elif(self.instruction_is_spread):
                    # Set up main memory read, indexed with immediate off SP
                    m.d.sync += self.mem_addr.eq(self.sp + self.immediate_buffer)
                    self.eq_unless_constant(m, self.mem_read_en, 1)
                with m.Else():
                    # Set up read 2
                    m.d.sync += [
                        self.gprs.addr.eq(self.reg_read_2_idx),
                        self.gprs.read_en.eq(C(1)),
                    ]

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.READ_2_LATENCY.value))

            # READ_2_LATENCY:
            #   Does nothing. Provides memory read latency before `READ_2`.
            with m.Elif(self.stage == Core.Stage.READ_2_LATENCY):
                # Latency for memory read. Nothing happens here!
                # Just advance
                m.d.sync += self.stage.eq(C(Core.Stage.READ_2.value))

            # READ_2:
            #   Retrieves data read from the second decoded GPR (or main memory, if the instruction
            #   is `read`).
            with m.Elif(self.stage == Core.Stage.READ_2.value):
                with m.If(self.instruction_is_read | self.instruction_is_spread):
                    # Main memory read is complete - piggyback off the reg_read_2_buffer to store
                    # the read value
                    m.d.sync += self.reg_read_2_buffer.eq(self.mem_read_data)
                    self.eq_unless_constant(m, self.mem_read_en, 0)
                with m.Else():
                    # Read 2 is complete - copy data into buffer
                    m.d.sync += [
                        self.reg_read_2_buffer.eq(self.gprs.read_data),
                        self.gprs.read_en.eq(C(0)),
                    ]

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.EXECUTE.value))

            # EXECUTE:
            #   Executes instruction logic, using data from read GPRs. If the `DECODE` stage
            #   identified that the instruction will write back to a GPR, the new data shall be
            #   provided as part of this stage, by storing it in `gprs.write_data`.
            #
            #   If the instruction is `write`, initiates a write back to main memory, instead of a
            #   GPR.
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


                    # === Memory ===
                    with m.Case("0010 0000 0--- 0---", "0011 0000 0--- ----"): # read, spread
                        # Read 2 was "magic" for this instruction, and actually read main memory
                        # instead. See those sections for details.
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_2_buffer)

                    with m.Case("0010 0000 1--- 0---"): # write
                        m.d.sync += [
                            self.mem_addr.eq(self.reg_read_1_buffer),
                            self.mem_write_data.eq(self.reg_read_2_buffer),
                            self.mem_write_en.eq(C(1)),
                        ]

                    with m.Case("0011 0000 1--- ----"): # spwrite
                        m.d.sync += [
                            self.mem_addr.eq(self.sp + self.immediate_buffer),
                            self.mem_write_data.eq(self.reg_read_1_buffer),
                            self.mem_write_en.eq(C(1)),
                        ]


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


                    # === General-Purpose Arithmetic ===
                    with m.Case("0100 1000 0000 0---"): # neg
                        m.d.sync += self.gprs.write_data.eq(-self.reg_read_1_buffer)

                    with m.Case("0100 1000 0001 0---"): # inc
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer + 1)

                    with m.Case("0100 1000 0010 0---"): # dec
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer - 1)

                    with m.Case("0100 1001 0--- 0---"): # add
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer + self.reg_read_2_buffer)
                        
                    with m.Case("0100 1010 0--- 0---"): # sub
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer - self.reg_read_2_buffer)

                    with m.Case("0100 1011 0--- 0---"): # mul
                        m.d.sync += self.gprs.write_data.eq(self.reg_read_1_buffer * self.reg_read_2_buffer)


                    # === Branching ===
                    with m.Case("0110 0000 ---- ----", ): # jmpoff
                        m.d.sync += self.ip.eq(self.ip + self.immediate_buffer.as_signed())

                    with m.Case("0110 0001 ---- ----"): # cjmpoff
                        with m.If(self.ef[1]):
                            m.d.sync += self.ip.eq(self.ip + self.immediate_buffer.as_signed())

                    with m.Case("0110 0011 0000 0---"): # cjmp
                        with m.If(self.ef[1]):
                            m.d.sync += self.ip.eq(self.reg_read_1_buffer)
                    
                    with m.Case("0110 0010 0001 0---"): # call
                        m.d.sync += self.rp.eq(self.ip)
                        m.d.sync += self.ip.eq(self.reg_read_1_buffer)

                    with m.Case("0110 0010 0001 1000"): # ret
                        m.d.sync += self.ip.eq(self.rp)



                # If write is required, set it up
                with m.If(self.reg_write_required):
                    # Buffer already contains data from instruction switch
                    m.d.sync += [
                        self.gprs.addr.eq(self.reg_write_idx),
                        self.gprs.write_en.eq(C(1)),
                    ]

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.WRITE.value))
                
            # WRITE:
            #   Completes the write initiated by `EXECUTE`, to a GPR, or main memory if the
            #   instruction was `write`.
            #
            #   If the target GPR was r0 and `debug_led` was provided, also writes the lowest bit of
            #   r0 out to that signal. 
            with m.Elif(self.stage == Core.Stage.WRITE.value):
                # If the write was to r0, and we have a debugging LED, then update it
                if self.debug_led is not None:
                    with m.If(self.reg_write_required):
                        with m.If(self.reg_write_idx == C(0)):
                            m.d.sync += self.debug_led.eq(self.gprs.write_data[0])

                # Write, if any, has finished
                m.d.sync += [
                    self.gprs.write_en.eq(C(0)),
                    self.reg_write_required.eq(C(0)),
                ]

                with m.If(self.instruction_is_write | self.instruction_is_spwrite):
                    m.d.sync += self.mem_write_en.eq(C(0))

                # Advance
                m.d.sync += self.stage.eq(C(Core.Stage.FETCH.value))

        return m

    def decode(
        self, m: Module,
        read_1: Optional[Signal] = None, read_2: Optional[Signal] = None,
        write: Optional[Signal] = None,
        imm: Optional[Signal] = None,
    ):
        """Completes the `DECODE` stage for a particular instruction, by starting `READ_1` and
        saving state for `READ_2` and `WRITE`."""

        if imm is not None:
            m.d.sync += self.immediate_buffer.eq(imm)

        if read_1 is not None:
            m.d.sync += self.gprs.addr.eq(read_1)
        
        if read_2 is not None:
            m.d.sync += self.reg_read_2_idx.eq(read_2)

        if write is not None:
            m.d.sync += self.reg_write_idx.eq(write)
            m.d.sync += self.reg_write_required.eq(1)

    def eq_unless_constant(self, m: Module, signal_or_const, value: int):
        # Set read enabler, if we have one - sometimes it's a constant 1, depending on the
        # memory configuration
        if isinstance(signal_or_const, Signal):
            m.d.sync += signal_or_const.eq(C(value))
