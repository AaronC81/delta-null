# Delta Null Processor Core

## Overview

- 16-bit architecture
    - Not many instructions required
    - Keeps program memory usage down
- RISC
- Fixed-size instructions
- Register based, with:
    - 8 general-purpose registers
    - 4? decimal numeric registers
    - Special-purpose registers:
        - Instruction pointer (IP)
        - Return pointer (RP)
        - Stack pointer (SP)
        - Execution flags (EF)
            - **TODO: what flags?**
            - Halt flag (bit 0)
            - Condition flag (bit 1)
- Connected to memory with 16-bit address and data bus
    - Each address refers to a 16-bit word - smaller access not possible

## State

### Decimal Registers

Inspiration: https://knightos.org/2017/08/19/Decimal-math-support.html

TODO
