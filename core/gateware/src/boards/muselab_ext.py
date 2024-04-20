# From: https://github.com/LukeMoll/amaranth-boards/blob/main/amaranth_boards/muselab_ext.py
# But modified to use `ecpdap` instead of `openfpgaloader`

import os
import subprocess

from amaranth.build import *
from amaranth.vendor.lattice_ecp5 import *
from amaranth_boards.resources import *

__all__ = ["Colorlight_i5_Muselab_ext"]

# https://github.com/wuxx/Colorlight-FPGA-Projects#component

class Colorlight_i5_Muselab_ext(LatticeECP5Platform):
    device              = "LFE5U-25F"
    package             = "BG381"
    speed               = "6"
    default_clk         = "clk25"

    resources = [
        Resource("clk25", 0, Pins("P3", dir="i"), Clock(25e6), Attrs(IO_TYPE="LVCMOS33")),

        *LEDResources(pins="U16", invert=True, attrs=Attrs(IO_TYPE="LVCMOS33", DRIVE="4")),

        UARTResource(0, tx="J17", rx="H18", attrs=Attrs(IO_TYPE="LVCMOS33")),

        # GD25Q16CSIG 2MB
        Resource("spi_flash", 0,
            Subsignal("cs",  PinsN("R2", dir='o')),
            Subsignal("cipo", Pins("V2", dir='i')),
            Subsignal('copi', Pins("W2", dir='o')),
            Subsignal("clk",  Pins("U3", dir='o')),
            Attrs(IO_TYPE="LVCMOS33")
        ),

        Resource("eth_rgmii", 0,
            Subsignal("mdc",    Pins("N5",  dir='o')),
            Subsignal("mdio",   Pins("P5",  dir='io')),
            Subsignal("rst",    Pins("P4",  dir='o')),
            Subsignal("tx_clk", Pins("U19", dir='o')),
            Subsignal("tx_data", Pins("U20 T19 T20 R20", dir='o')),
            Subsignal("tx_ctl", Pins("P19", dir='o')),
            Subsignal("rx_clk", Pins("L19", dir='i')),
            Subsignal("rx_data", Pins("P20 N19 N20 M19", dir='i')),
            Subsignal("rx_ctl", Pins("M20", dir='i')),
            Attrs(IO_TYPE="LVCMOS33")
        ),

        Resource("eth_rgmii", 1,
            Subsignal("mdc",    Pins("N5",  dir='o')),
            Subsignal("mdio",   Pins("P5",  dir='io')),
            Subsignal("rst",    Pins("P4",  dir='o')),
            Subsignal("tx_clk", Pins("G1",  dir='o')),
            Subsignal("tx_data", Pins("G2 H1 J1 J3", dir='o')),
            Subsignal("tx_ctl", Pins("K1",  dir='o')),
            Subsignal("rx_clk", Pins("H2",  dir='i')),
            Subsignal("rx_data", Pins("K2 L1 N1 P1", dir='i')),
            Subsignal("rx_ctl", Pins("P2",  dir='i')),
            Attrs(IO_TYPE="LVCMOS33")
        ),

        # EtronTech EM638325BK-6H: 2M x 32bit 166 MHz SDRAMs
        SDRAMResource(0,
            clk="B9", we_n="A10", cas_n="A9", ras_n="B10",
            ba="B11 C8", a="B13 C14 A16 A17 B16 B15 A14 A13 A12 A11 B12",
            dq="B6  A5  A6  A7  C7  B8  B5  A8  D8  D7  E8  D6  C6  D5  E7  C5"
               "C10 D9  E11 D11 C11 D12 E9  C12 E14 C15 E13 D15 E12 B17 D14 D13",
            attrs=Attrs(PULLMODE="NONE", DRIVE="4", SLEWRATE="FAST", IO_TYPE="LVCMOS33")
        ),

        Resource("pmod", 1, Pins("1 2 3 4 7 8 9 10", dir='io', conn=("pmod", 1))),
        Resource("pmod", 2, Pins("1 2 3 4 7 8 9 10", dir='i',  conn=("pmod", 2))),


    ]
    connectors = [
        Connector("pmod", 0, "P17 R18 C18 U16 - - M17 R17 T18 K18 - -"), # P2, pmod 1a # Shares pins with LED
        Connector("pmod", 1, "J20 L18 M18 N17 - - G20 K20 L20 N18 - -"), # P2, pmod 1b
        Connector("pmod", 2, "C17 B18 B20 F20 - - A18 A19 B19 D20 - -"), # P3, pmod 2a
        Connector("pmod", 3, "D1  C1  C2  E3  - - E2  D2  B1  A3  - -"), # P3, pmod 2b
        Connector("pmod", 4, "H4  G3  F1  F2  - - H3  F3  E4  E1  - -"), # P4, pmod 3a
        Connector("pmod", 5, "-   E19 B3  K5  - - -   B2  K4  A2  - -"), # P4, pmod 3b # TODO: should this still be a pmod?
        Connector("pmod", 6, "D18 G5  F5  E5  - - D17 D16 E6  F4  - -"), # P5, pmod 4a
        Connector("pmod", 7, "J17 H17 H16 G16 - - H18 G18 F18 E18 - -"), # P5, pmod 4b # Shares pins with UART
        Connector("pmod", 8, "R3  M4  L5  J16 - - N4  L4  P16 J18 - -"), # P6, pmod 5a
        Connector("pmod", 9, "R1  U1  W1  M1  - - T1  Y2  V1  N2  - -"), # P6, pmod 5b
        # "loose" i/o between pmods?
    ]

    @property
    def required_tools(self):
        return super().required_tools + [
            "openFPGALoader"
        ]
    
    def toolchain_prepare(self, fragment, name, **kwargs):
        overrides = dict(ecppack_opts="--compress")
        overrides.update(kwargs)
        return super().toolchain_prepare(fragment, name, **overrides)

    @property
    def required_tools(self):
        return super().required_tools + [
            "ecpdap"
        ]

    def toolchain_program(self, products, name):
        with products.extract("{}.bit".format(name)) as bitstream_filename:
            subprocess.run(["ecpdap", "program", bitstream_filename])

if __name__ == "__main__":
    from test.blinky import *
    Colorlight_i5_Muselab_ext().build(Blinky(), do_program=True)
