from amaranth import *
from amaranth.build import *
from ..modules.core import Core
from ..modules.timer import Timer
from ..modules.uart_logger import UartLogger
from ..modules.spi import SPI
from typing import Optional

class BaseMemoryMap(Elaboratable):
    # The start address of RAM.
    RAM_START = None
    
    # The number of RAM words available.
    RAM_DEPTH = None

    # The start address of the HCR.
    HCR_START = None

    # The total number of GPIO pins available through the HCR.
    HCR_GPIO_PIN_COUNT = None

    # The number of clock cycles which occur per microsecond on this platform.
    TICKS_PER_MICROSECOND = None

    # The number of clock cycles which occur per tick of a 9600 baud clock on this platform.
    TICKS_PER_9600_BAUD = None

    # The start address of the timer peripheral within the HCR.
    TIMER_START = 0x100

    # The start address of the logger peripheral within the HCR.
    LOGGER_START = 0x200

    # The start address of the SPI peripheral within the HCR.
    SPI_START = 0x300

    def __init__(self, init_ram, depth):
        self.init_ram = init_ram
        self.depth = depth

        self.addr = Signal(Core.DATA_WIDTH)
        self.read_data = Signal(Core.DATA_WIDTH)
        self.write_data = Signal(Core.DATA_WIDTH)
        self.read_en = Signal()
        self.write_en = Signal()

        self.hcr_gpio_o =  Signal(type(self).HCR_GPIO_PIN_COUNT)
        self.hcr_gpio_i =  Signal(type(self).HCR_GPIO_PIN_COUNT)
        self.hcr_gpio_oe = Signal(type(self).HCR_GPIO_PIN_COUNT)

        self.logger_data_out = Signal()

        self.timer = Timer(
            type(self).TICKS_PER_MICROSECOND,
            self.addr - type(self).HCR_START - type(self).TIMER_START,
            Signal(Core.DATA_WIDTH), Signal(), self.write_data, Signal()
        )

        self.uart_logger = UartLogger(
            type(self).TICKS_PER_9600_BAUD,
            self.logger_data_out,
            self.addr - type(self).HCR_START - type(self).LOGGER_START,
            Signal(Core.DATA_WIDTH), Signal(), self.write_data, Signal()
        )

        self.spi = SPI(
            Signal(), Signal(), Signal(),
            self.addr - type(self).HCR_START - type(self).SPI_START,
            Signal(Core.DATA_WIDTH), Signal(), self.write_data, Signal()
        )

    def bind_hcr_peripherals(self, platform: Platform, m: Module):
        raise NotImplementedError()

    def elaborate(self, platform: Platform) -> Module:
        m = Module()

        # Instantiate RAM
        self.ram = Memory(width=Core.DATA_WIDTH, depth=self.depth, init=self.init_ram)
        mem_read = self.ram.read_port()
        mem_write = self.ram.write_port()
        m.d.comb += mem_write.addr.eq(mem_read.addr)

        m.submodules.mem_read = mem_read
        m.submodules.mem_write = mem_write

        m.submodules.timer = self.timer
        m.submodules.uart_logger = self.uart_logger
        m.submodules.spi = self.spi

        if platform is not None:
            self.bind_hcr_peripherals(platform, m)

        with m.If(self.addr.matches("1111 ---- ---- ----")):
            # Handled by HCR (0xF---)
            hcr_rel_addr = self.addr - type(self).HCR_START

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

                    # === Timer ===
                    with m.Case(*range(type(self).TIMER_START, type(self).TIMER_START + 4)):
                        m.d.comb += self.timer.mem_write_en.eq(1)

                    # == UART Logger ==
                    # TODO: specify in platform override
                    with m.Case(*range(type(self).LOGGER_START, type(self).LOGGER_START + UartLogger.LOGGER_SIZE)):
                        m.d.comb += self.uart_logger.mem_write_en.eq(1)

                    # == SPI ==
                    with m.Case(*range(type(self).SPI_START, type(self).SPI_START + SPI.LOGGER_SIZE)):
                        m.d.comb += self.spi.mem_write_en.eq(1)

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

                    # === Timer ===
                    with m.Case(*range(type(self).TIMER_START, type(self).TIMER_START + 4)):
                        m.d.comb += self.timer.mem_read_en.eq(1)
                        m.d.comb += self.read_data.eq(self.timer.mem_read_data)

                    # == UART Logger ==
                    # TODO: specify in platform override
                    with m.Case(*range(type(self).LOGGER_START, type(self).LOGGER_START + UartLogger.LOGGER_SIZE)):
                        m.d.comb += self.uart_logger.mem_read_en.eq(1)
                        m.d.comb += self.read_data.eq(self.uart_logger.mem_read_data)

                    # == SPI ==
                    with m.Case(*range(type(self).SPI_START, type(self).SPI_START + SPI.LOGGER_SIZE)):
                        m.d.comb += self.spi.mem_read_en.eq(1)
                        m.d.comb += self.read_data.eq(self.spi.mem_read_data)

                    # Something we don't know!
                    with m.Default():
                        m.d.comb += self.read_data.eq(0x0BAD)
        
        with m.Else():
            # Forward to RAM
            m.d.comb += [
                mem_read.addr.eq(self.addr - type(self).RAM_START),
                self.read_data.eq(mem_read.data),
                mem_write.data.eq(self.write_data),
                mem_write.en.eq(self.write_en),
            ]
            if isinstance(mem_read.en, Signal):
                m.d.comb += mem_read.en.eq(self.read_en)

        return m


class BaseTop(Elaboratable):
    MEMORY_MAP = None
    INITIAL_SP = None
    INITIAL_IP = None

    def __init__(self, instructions, depth=None):
        if depth is None:
            depth = type(self).MEMORY_MAP.RAM_DEPTH
        self.depth = depth

        self.mem_init = [0 for _ in range(depth)]
        for i, ins in enumerate(instructions):
            self.mem_init[i] = ins

    def elaborate(self, platform) -> Module:
        m = Module()

        self.mem = mem = type(self).MEMORY_MAP(self.mem_init, self.depth)

        self.core = Core(
            mem_addr=mem.addr,
            mem_read_data=mem.read_data,
            mem_read_en=mem.read_en,
            mem_write_data=mem.write_data,
            mem_write_en=mem.write_en,

            initial_sp=type(self).INITIAL_SP,
            initial_ip=type(self).INITIAL_IP,
        )

        m.submodules.mem = mem
        m.submodules.core = self.core

        return m
