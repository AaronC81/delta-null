# SPI

The core provides an SPI transmission interface, to send data to an SPI peripheral device.

Receiving data from an SPI peripheral is not supported.

## Configuration

The SPI peripheral can be configured to run the clock at some division of the core's main clock.
This means that the SPI speed is specific to the platform.

## Usage Overview

Sending an SPI transmission involves the following steps:

1. Copy data into the SPI buffer
2. Tell SPI the length of the data
3. Trigger SPI to write data - it will send asynchronously as the core executes further
   instructions
4. Optionally, if there is more data which didn't fit into the buffer, wait until the transmission
   is done by monitoring its status, then repeat the process

## Data Encoding

This SPI implementation has the following characteristics:

- CS is active low
- COPI and SCLK are active high
- Data is valid on rising edge of clock
- MSB first
- 8-bit words

Buffer data is **packed**, because two SPI data bytes fit into a single memory location.

The lower 8 bits of a 16-bit word is the first byte, and the upper 8 bits are the second bytes.

For example:

```
Buffer:       3412  CDAB
Transmission: 12 34 AB CD
```

## Registers

### Control

`0x0000` is the control register, which contains the following bits:

- **Bits 0-6: Data size**. The unsigned integer expressed by these bits holds the number of
  **bytes** to send (not words).
- **Bit 7: Send**. When set to 1, immediately begins sending the data in the buffer.

Do not write to the control register while data is being sent.

The control register is deliberately laid out with the data size occupying the lower bits, so
that writing `size | 0x80` will both populate the size and start sending the data in the buffer.

### Status

`0x0001` is the status register, which contains the following bits:

- **Bit 0: Sent**. Once the data has finished sending, this bit is changed from 0 to 1.

The status register can be written to, but the written value is ignored. Instead, it simply resets
the "sent" bit to 0.

## Clock Shift

`0x0002` is the clock shift configuration register.

The higher the value, the slower the SPI clock. The exact speeds are platform-specific.

Do not modify this register during a transmission.

### Buffer

`0x0010` to `0x004F` (inclusive) is the buffer where data is placed before sending.

The content of the buffer is undefined during and after transmitting data.
