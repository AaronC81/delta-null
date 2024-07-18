# Colorlight i5 Implementation

Currently, the memory map, boot state, HCR layout, and GPIO layout is identical to that of the
[TinyFPGA BX](tinyfpga_bx.md), for program portability between the two.

The Colorlight i5 has considerably more of these resources than the TinyFPGA BX, so theoretically
it would be possible to expand this later, but I'd like to have more infrastructure in place first.

## GPIO Mapping

Physical pins are written according to
[this guide by Tom Verbeure](https://tomverbeure.github.io/2021/01/30/Colorlight-i5-Extension-Board-Pin-Mapping.html).

| Delta Null Bit(s) | Physical Pin(s)    |
|-------------------|--------------------|
| 0                 | Onboard LED        |
| 1-7               | PMOD_P2A_IO1-**7** |
| 8-15              | PMOD_P2B_IO1-8     |
| 16-23             | PMOD_P3A_IO1-8     |
| 24-31             | PMOD_P3B_IO1-8     |

Pin 0 is the onboard LED pin (for consistency with the TinyFPGA BX), but this signal is driven by 
PMOD_P2A_IO8, so the pin ordering for that port is a bit weird.

Connector P1 is skipped because it appears to be intended for Ethernet connectivity.

## Logger

This platform implements the [logger peripheral](../peripherals/logger.md) using the USB UART
interface provided by the Colorlight i5's carrier board.

Serial data is transmitted with the following characteristics:

- 9600 baud
- 8 data bits
- 1 stop bit
- No parity
- No flow control

The UTF-16 message bytes are transmitted as **UTF-16 LE**, so the character `A` is transmitted as
`0x41 0x00`.

## SPI

This platform implements the SPI peripheral, using the following pins:

| SPI Purpose | Physical Pin |
|-------------|--------------|
| COPI        | PMOD_P4A_IO1 |
| SCLK        | PMOD_P4A_IO2 |
| CS          | PMOD_P4A_IO3 |
