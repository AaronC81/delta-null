# TinyFPGA BX Implementation

## Memory Map

`0x1000 ..= 0x2DFF` is RAM - readable and writable.

`0xF000 ..= 0xFFFF` is the hardware control region, which provides access to hardware state.

## Boot State

At boot, all RAM is zeroed except for the program which was uploaded.

Registers are initialised as follows:

- `ip` is `0x1000`, pointing to the beginning of RAM.
- `sp` is `0x2E00`, pointing one word after the end of RAM.
- All other registers are zeroed.

## Hardware Control Region (HCR)

All addresses here are relative to the beginning of the HCR, `0xF000`.

### Metadata

`0x0000` is a read-only magic number which shows that the HCR exists, always set to `0xF90A` (which
looks like "FPGA" is you squint really hard).

`0x0001` is a read-only harness indicator. This is zero if the core is executing as normal on its
intended hardware, or any application-defined non-zero value if it is running in an emulator or as
part of a testbench.

### GPIO

32 GPIO pins are accessible from the core through the HCR. These are numbered as follows:

- Pin 0 references the onboard LED, only usable as an output. (This is also exposed as a pin on the
  underside of the board.)
- Pins 1-31 match the silkscreen on the board, usable as both input and output.
  Note that only pins 1-24 are usable with the board soldered through-hole; the others are on the
  underside.

The TinyFPGA BX does provide more pins, but they are occupied for other resources (e.g. flash, USB),
so it is difficult to safely use them for additional I/O. Therefore they are not exposed through the
HCR.

All pin state and configuration is stored as a word-packed set of booleans, so that one word
describes 16 pins. As there are 32 pins, two words are required - the lower word in memory describes
pins 0-15, and the upper word describes 16-31.

#### Mode Configuration (R/W)

`0x0010` and `0x0011` configure the operating mode of GPIO pins:

- `0` - the pin is a high-impedance (Z) state, which allows it to be used as an input.
- `1` - the pin is driven as an output.

#### Output (R/W)

`0x0012` and `0x0013` configure the output of GPIO pins:

- `0` - the pin is driven low.
- `1` - the pin is driven high.

This only has an effect if the pin's mode configuration says that pin should be driven as an output
(`1`). Otherwise, the value is ignored.

#### Input (RO)

`0x0014` and `0x0015` can be read to determine the input logic level of a pin.

If the pin is not being driven by any external hardware, or the pin's configuration is not 
high-impedance (`0`), the value is indeterminate.
