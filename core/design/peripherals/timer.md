# Timer

The core provides a basic timer peripheral, useful for writing software which needs portable
approximate timings.

The timer's memory at `0xF100` - all addresses listed here are relative to this.

## Usage Overview

Using the timer will involve the following steps:

1. Configure a _target_, the amount of time which the timer will wait before firing
2. Start the timer in the desired mode
3. Wait until the timer fires by monitoring its status
4. Write to the status register to reset the "fired" flag.

Timer targets are configured in _microseconds_. The timer target is 32-bit, which gives a range of
target times from 1µs to ~1.193h.

## Registers

### Control

`0x0000` is the control register, which contains the following bits:

- **Bit 0: Running**. Set this bit to 1 to start the timer, or 0 to stop the timer.
- **Bit 1: Repeating**. If set, once the timer fires, it will immediately start again.

Writing to the control register has the side-effect of resetting the state of the timer, even if the
observable value in memory hasn't changed.

### Status

`0x0001` is the status register, which contains the following bits:

- **Bit 0: Fired**. Once the timer has fired, this bit is changed from 0 to 1.

The status register can be written to, but the written value is ignored. Instead, it simply resets
the "fired" bit to 0. 

### Target

`0x0002` and `0x0003` are the low and high words respectively of the target time in microseconds.

These should not be changed while the timer is running.
