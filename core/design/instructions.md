# Instruction Set

## Register Encoding

A general-purpose register can be encoded with three bits - so `r0` to `r7` are encoded with
`000` - `111` respectively.

The decimal registers `d0` to `d3` are encoded with `00` - `11`.

When instructions are generic over the special-purpose registers, the following encodings are used:

- `00` = IP
- `01` = RP
- `10` = SP
- `11` = EF

If an instruction has variants which support both general-purpose and decimal registers, these are
typically encoded with four bits - `0000` - `0111` for general-purpose, and `1000` - `1011` for
decimal.

## Core

- `0000 0000 0000 0000` - `nop` - No operation
- `1111 1111 1111 1111` - `hlt` - Halt execution
- `0010 0001 0sss 0ddd` - `mov` - Copy value from general-purpose register `s` into general-purpose
  register `d`
- `0010 0001 00ss 10dd` - `d_mov` - Copy value from decimal register `s` into decimal register `d`

## Immediate Loads

Due to the limited instruction width, immediate instructions are kept to a minimum.

It would be possible to minimise even further by using a "shift 1 byte left and load low byte"
instruction, but this may be excessively minimal, and using an extra bit to select the byte rounds
out the encoding nicely.

- `0001 0rrr bbbb bbbb` - `putl` - Load immediate `b` into low byte of general-purpose register `r` 
- `0001 1rrr bbbb bbbb` - `puth` - Load immediate `b` into high byte of general-purpose register `r`

## Memory

In all of the below encodings, `a` is a general-purpose register whose value is interpreted as a
memory address.

Being a RISC architecture, there should be few instructions which deal with memory. The only
instructions which perform any kind of memory access are those listed here.

- `0010 0000 0aaa 0rrr` - `read` - Read word from memory `a` into general-purpose register `r`
- `0010 0000 1aaa 0rrr` - `write` - Write word to memory `a` from general-purpose register `r`
- `0011 0000 0rrr iiii` - `spread` - Read word from `SP + i` into general-purpose register `r`
- `0011 0000 1rrr iiii` - `spwrite` - Write word to `SP + i` from general-purpose register `r`
- `0010 0000 0aaa 10dd` - `d_read` - Read word from memory `a` into decimal register `d`
- `0010 0000 1aaa 10dd` - `d_write` - Write word from memory `a` from decimal register `r`
- `0010 0011 1101 0rrr` - `push` - Decrement SP by 1, then write value from general-purpose register
  `r` into memory pointed to by SP
- `0010 0011 1101 1rrr` - `pop` - Read value from memory pointed to by SP into general-purpose
  register `r`, then increment SP by 1

## Special-Purpose Registers

- `0010 0001 10ss 0ddd` - `movso` - Copy value from special-purpose register `s` into general-purpose
register `d`
- `0010 0001 11dd 0sss` - `movsi` - Copy value from general-purpose register `s` into special-purpose
register `d`
- `0010 0001 1101 1ooo` - `spadd` - Add value from general-purpose register `o` onto SP
- `0010 0010 1101 0000` - `spinc` - Increment SP by 1
- `0010 0010 1101 0001` - `spdec` - Decrement SP by 1

## Bit Manipulation

- `0100 0000 0000 0rrr` - `not` - `r = ~r`
- `0100 0001 0xxx 0rrr` - `and` - `r = r & x`
- `0100 0010 0xxx 0rrr` - `or` - `r = r | x`
- `0100 0011 0xxx 0rrr` - `xor` - `r = r ^ x`
- `0100 0100 0xxx 0rrr` - `shl` - `r = r << x`
- `0100 0101 0xxx 0rrr` - `shr` - `r = r >> x`
- `0100 0111 0iii 0rrr` - `bitset` - Set bit of register `r`, indexed by value in register `b`, to
  `EF.cond`

## General-Purpose Arithmetic

- `0100 1000 0000 0rrr` - `neg` - `r = -r`
- `0100 1000 0001 0rrr` - `inc` - `r = r + 1`
- `0100 1000 0010 0rrr` - `dec` - `r = r - 1`
- `0100 1001 0xxx 0rrr` - `add` - `r = r + x`
- `0100 1010 0xxx 0rrr` - `sub` - `r = r - x`
- `0100 1011 0xxx 0rrr` - `mul` - `r = r * x`

This is a relatively limited set of arithmetic. Potential future improvements include:
- With-carry operations (e.g. add with carry)
- Division
- Overflow flag

## Comparison

- `0101 0000 0000 0000` - `inv` - `EF.cond = ~EF.cond`
- `0101 0000 0001 0rrr` - `eqz` - `EF.cond = (r == 0)`
- `0101 0001 0aaa 0bbb` - `eq` - `EF.cond = (a == b)`
- `0101 0010 0aaa 0bbb` - `gt` - `EF.cond = (a > b)`
- `0101 0011 0aaa 0bbb` - `gteq` - `EF.cond = (a >= b)`

### Branching

- `0110 0000 bbbb bbbb` - `jmpoff` - Sign-extend immediate `b` to 16-bits, and add to `IP`
- `0110 0001 bbbb bbbb` - `cjmpoff` - If `EF.cond` set, sign-extend immediate `b` to 16-bits, and add
  to `IP`
- `0110 0010 0000 0rrr` - **Unused but reserved** (was previously `jmp`, but `movsi` fills the same
  role)
- `0110 0011 0000 0rrr` - `cjmp` - If `EF.cond` set, copy value from general-purpose register `r`
  into `IP`
- `0110 0010 0001 0rrr` - `call` - Copy address of next instruction into `RP`, then copy value from
  general-purpose register `r` into `IP`
- `0110 0100 bbbb bbbb` - `calloff` - Copy address of next instruction into `RP`, then sign-extend
  immediate `b` to 16-bits, and add to `IP`
- `0110 0010 0001 1000` - `ret` - Copy `RP` into `IP`

### Decimal Arithmetic

**TODO**
