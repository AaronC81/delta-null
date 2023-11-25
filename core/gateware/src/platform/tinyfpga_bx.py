import os, subprocess
from amaranth import *
from amaranth.build import *
from ..modules.core import Core
from amaranth_boards.tinyfpga_bx import TinyFPGABXPlatform

class TinyFPGABXSerialProgPlatform(TinyFPGABXPlatform):
    """Same as `TinyFPGABXPlatform`, but passes `--pyserial` when programming."""

    def toolchain_program(self, products, name):
        # Copied from base implementation, with --pyserial added
        tinyprog = os.environ.get("TINYPROG", "tinyprog")
        with products.extract("{}.bin".format(name)) as bitstream_filename:
            subprocess.check_call([tinyprog, "-p", bitstream_filename, "--pyserial"])


class TinyFPGABXMemoryMap(Elaboratable):
    # The start address of RAM.
    RAM_START = 0x1000
    
    # The number of RAM words available.
    #
    # Trial-and-errored to be the largest with synthesises successfully.
    # Seems about right, though - TinyFPGA BX has:
    #       128k bits of block RAM
    #    -> 16k bytes
    #    -> 0x4000 bytes
    #    -> 0x2000 words
    RAM_DEPTH = 0x1E00

    # The start address of the HCR.
    HCR_START = 0xF000

    # The total number of GPIO pins available through the HCR.
    HCR_GPIO_PIN_COUNT = 32

    def __init__(self, init_ram, depth):
        self.init_ram = init_ram
        self.depth = depth

        self.addr = Signal(Core.DATA_WIDTH)
        self.read_data = Signal(Core.DATA_WIDTH)
        self.write_data = Signal(Core.DATA_WIDTH)
        self.read_en = Signal()
        self.write_en = Signal()

        self.hcr_gpio_o = Signal(TinyFPGABXMemoryMap.HCR_GPIO_PIN_COUNT)
        self.hcr_gpio_i = Signal(TinyFPGABXMemoryMap.HCR_GPIO_PIN_COUNT)
        self.hcr_gpio_oe = Signal(TinyFPGABXMemoryMap.HCR_GPIO_PIN_COUNT)

    def elaborate(self, platform: Platform) -> Module:
        m = Module()

        # Instantiate RAM
        self.ram = Memory(width=Core.DATA_WIDTH, depth=self.depth, init=self.init_ram)
        mem_read = self.ram.read_port()
        mem_write = self.ram.write_port()
        m.d.comb += mem_write.addr.eq(mem_read.addr)

        m.submodules.mem_read = mem_read
        m.submodules.mem_write = mem_write

        if platform is not None:
            # Create a new resource for every mapped hardware GPIO pin
            # (There is a "connector" defined already, but I don't think we have direct access to
            #  those...)
            # It's possible to specify multiple pins for one resource, but that only gives us a single
            # output-enable for every pin. We need per-pin control.
            resources = []
            for i in range(1, TinyFPGABXMemoryMap.HCR_GPIO_PIN_COUNT):
                resources.append(Resource(f"gpio{i}", 0,
                    Pins(f"{i}", dir="io", conn=("gpio", 0))                          
                ))
            platform.add_resources(resources)

            # Bind each IO pin
            # Pin 0 is LED (only output), the rest are GPIO (input/output)
            m.d.comb += platform.request("led").eq(self.hcr_gpio_o[0])
            for i in range(1, TinyFPGABXMemoryMap.HCR_GPIO_PIN_COUNT):
                pin = platform.request(f"gpio{i}")
                m.d.comb += [
                    self.hcr_gpio_i[i].eq(pin.i),
                    pin.oe.eq(self.hcr_gpio_oe[i]),
                    pin.o.eq(self.hcr_gpio_o[i]),
                ]

        with m.If(self.addr.matches("1111 ---- ---- ----")):
            # Handled by HCR (0xF---)
            hcr_rel_addr = self.addr - TinyFPGABXMemoryMap.HCR_START

            # Check write first, that takes "priority"
            # (Also, read_en is fixed high on some platforms)
            with m.If(self.write_en):
                with m.Switch(hcr_rel_addr):
                    # === GPIO ===
                    with m.Case(0x10): # Mode configuration (low)
                        m.d.sync += self.hcr_gpio_oe[0:16].eq(self.write_data)
                    with m.Case(0x11): # Mode configuration (high)
                        m.d.sync += self.hcr_gpio_oe[16:32].eq(self.write_data)
                    with m.Case(0x12): # Output (low)
                        m.d.sync += self.hcr_gpio_o[0:16].eq(self.write_data)
                    with m.Case(0x13): # Output (high)
                        m.d.sync += self.hcr_gpio_o[16:32].eq(self.write_data)
                    # TODO: input

            with m.Elif(self.read_en):
                with m.Switch(hcr_rel_addr):
                    # === Metadata ===
                    with m.Case(0x0): # Magic number
                        m.d.comb += self.read_data.eq(0xF90A)
                    with m.Case(0x1): # Harness indicator
                        m.d.comb += self.read_data.eq(0)

                    # === GPIO ===
                    with m.Case(0x10): # Mode configuration (low)
                        m.d.comb += self.read_data.eq(self.hcr_gpio_oe[0:16])
                    with m.Case(0x11): # Mode configuration (high)
                        m.d.comb += self.read_data.eq(self.hcr_gpio_oe[16:32])
                    with m.Case(0x12): # Output (low)
                        m.d.comb += self.read_data.eq(self.hcr_gpio_o[0:16])
                    with m.Case(0x13): # Output (high)
                        m.d.comb += self.read_data.eq(self.hcr_gpio_o[16:32])
                    # TODO: input

                    # Something we don't know!
                    with m.Default():
                        m.d.comb += self.read_data.eq(0x0BAD)
        
        with m.Else():
            # Forward to RAM
            m.d.comb += [
                mem_read.addr.eq(self.addr - TinyFPGABXMemoryMap.RAM_START),
                self.read_data.eq(mem_read.data),
                mem_write.data.eq(self.write_data),
                mem_write.en.eq(self.write_en),
            ]
            if isinstance(mem_read.en, Signal):
                m.d.comb += mem_read.en.eq(self.read_en)

        return m


class TinyFPGABXTop(Elaboratable):
    def __init__(self, instructions, depth=None):
        if depth is None:
            depth = TinyFPGABXMemoryMap.RAM_DEPTH
        self.depth = depth

        self.mem_init = [0 for _ in range(depth)]
        for i, ins in enumerate(instructions):
            self.mem_init[i] = ins

    def elaborate(self, platform) -> Module:
        m = Module()

        self.mem = mem = TinyFPGABXMemoryMap(self.mem_init, self.depth)

        led = None
        if platform is not None:
            led = platform.request("led")
        self.core = Core(
            mem_addr=mem.addr,
            mem_read_data=mem.read_data,
            mem_read_en=mem.read_en,
            mem_write_data=mem.write_data,
            mem_write_en=mem.write_en,

            initial_sp=0x1E00,
            initial_ip=0x1000,

            debug_led=led,
        )

        m.submodules.mem = mem
        m.submodules.core = self.core

        return m
