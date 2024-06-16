# Logger

The logger provides a convenient interface for the core to emit data such as log messages,
diagnostic data, or control messages.

The way in which the platform exposes log data is entirely dependent on the platform, which should
select the most convenient and natural method. For example:

- Platforms with some kind of UART/USB interface can send data over serial to a connected device.
- Platforms with a display may show data there.

The timer's memory is at `0xF200` - all addresses listed here are relative to this.

## Usage Overview

Using the logger to emit a message will involve the following steps:

1. Copy a message into the logger's buffer
2. Tell the logger the length of the message
3. Trigger the logger to write message - it will send asynchronously as the core executes further
   instructions
4. Optionally, if there is more data which didn't fit into the buffer, wait until the logger is done
   by monitoring its status, then repeat the process

## Message Encoding

Log messages should comprise valid text in **UTF-16** format.

UTF-16 is rarely used nowadays, and is generally a bad choice in the world of the superior UTF-8
encoding.

However, UTF-16 is an unconventionally perfect fit for the Delta Null's 16-bit memory architecture.
It is significantly easier to process on the core than UTF-8, which would either require packing
multiple codepoints into a single word, or wasting one half of each word.

Because the core operates with 16-bit words and memory locations, specifying whether the codepoints
are stored as little-endian or big-endian isn't necessary. If a platform emits the messages over a
byte-based protocol, it should be specified there.

## Registers

### Control

`0x0000` is the control register, which contains the following bits:

- **Bits 0-6: Message size**. The unsigned integer expressed by these bits holds the length of the
  message to send. Should be no larger than 64.
- **Bit 7: Send**. When set to 1, immediately begins sending the message in the buffer.

Do not write to the control register while a message is being sent.

The control register is deliberately laid out with the message size occupying the lower bits, so
that writing `size | 0x80` will both populate the size and start sending the message in the buffer.

### Status

`0x0001` is the status register, which contains the following bits:

- **Bit 0: Sent**. Once the message has finished sending, this bit is changed from 0 to 1.

The status register can be written to, but the written value is ignored. Instead, it simply resets
the "sent" bit to 0.

### Buffer

`0x0010` to `0x004F` (inclusive) is the buffer where messages are placed before sending.

(Some addresses within the logger are skipped so that the buffer does not start from an awkward
address.)

The content of the buffer is undefined during and after the sending of a message.
