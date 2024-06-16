# TinyFPGA BX Implementation

## Memory Map

`0x1000 ..= 0x2DFF` is RAM - readable and writable.

`0xF000 ..= 0xFFFF` is the hardware control region, which provides access to hardware state.

## GPIO Mapping

Pin 0 references the onboard LED, only usable as an output. (This is also exposed as a pin on the
underside of the board.)

Pins 1-31 match the silkscreen on the board, usable as both input and output. Note that only pins
1-24 are usable with the board soldered through-hole; the others are on the underside.

## Logger

This platform does not currently implement the [logger peripheral](../peripherals/logger.md).
