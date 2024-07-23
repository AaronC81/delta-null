## Delta Null

With Delta Null, I set out to
**create an embedded project, as close to "from scratch" as you can get**.
This comprises:

- A **16-bit RISC processor core design**, implemented as:
  - An **FPGA soft-core** written using [Amaranth HDL](https://github.com/amaranth-lang/amaranth), targeting:
    - [TinyFPGA BX](https://tinyfpga.com/)
    - [Colorlight i5](https://www.colorlight-led.com/colorlight-i5/)
  - A software emulator
- An **assembler**
- A **compiler for a low-level programming language**, producing assembly
- An implementation of **Conway's Game of Life** written in that programming language, running on
  the FPGA implementation of the core, displaying the result on an SPI display

https://github.com/user-attachments/assets/50236fe0-164b-4091-94fb-241a1d4d2d7c

## Progress

**I am calling this complete!** You could spend forever on a project like this, so I wanted to get
something cool out of it, and leave it there.

The processor core is fully-functional, with the exception of a BCD extension which was designed
but never implemented. The idea was that this could have been used for a calculator, but I didn't
end up making one!

It could make use of pipelining to significantly speed things up, but right now it only executes a
single instruction at a time.

The assembler works perfectly, and is backed by a pile of libraries for instruction handling which
came in useful for later work.

The compiler is mostly usable, and produces decently-optimised code! The language front-end and
back-end are nicely decoupled, with the front-end producing a typed IR which the back-end ingests
and transform into core instructions. The back-end's generated code is reasonably optimised, but
sometimes hits issues with internal limitations (e.g. a lack of register spilling limiting the
complexity of single statements). Being aware of its flaws makes the language perfectly acceptable
for writing non-trivial programs.

## Showcase

### Architecture/Assembly Language

The Delta Null's instruction set and assembly language are closely linked: one opcode corresponds to
one instruction encoding. There are eight general-purpose registers, `r0`-`r7`, and a small set of
special-purpose registers.

The assembler provides some macros (prefixed with `.`) to make some operations more convenient, such
as initialising a register.

The below example implements an LED blink on the TinyFPGA BX gateware, which maps some hardware
control peripherals to the `0xF000` region.

```
; set built-in LED to output
.put r1, 0xF010
.put r2, 0x0001
write r1, r2

blink:

    ; busy-wait
    .put r2, 0xFFFF ; r2 = counter
    loop:
        dec r2
        eqz r2
        inv
        cjmpoff loop/offset

    ; toggle LED
    .put r1, 0xF012
    read r2, r1
    not r2
    write r1, r2

    ; repeat
    jmpoff blink/offset
```

### Emulator

The emulator has a nifty terminal-based user interface:

```
┌Instructions────────────────────────────┐┌GPRs───┐┌SPRs───┐
│> 0000 1110 putl r1, 16                 ││r0 0000││ip 0000│
│  0001 19f0 puth r1, 240                ││r1 0000││rp 0000│
│  0002 1201 putl r2, 1                  ││r2 0000││sp ffff│
│  0003 1a00 puth r2, 0                  ││r3 0000││ef 0000│
│  0004 2092 write r1, r2                ││r4 0000││       │
│  0005 12ff putl r2, 255                ││r5 0000││       │
│  0006 1aff puth r2, 255                ││r6 0000││       │
│  0007 4822 dec r2                      ││r7 0000││       │
│  0008 5012 eqz r2                      ││       ││       │
└────────────────────────────────────────┘└───────┘└───────┘
 BREAK    [S]tep   [R]un   [M]emory...   [/]Command   [Q]uit
```

It also uses an interchangable backend, with communication between the frontend and backend over
ZeroMQ. Theoretically, this allows for alternative backend implementations, like a gateware
simulator adapter or even a hardware debugger - but I haven't looked at this yet!

### Language

There isn't much there yet, but it is Turing-complete. You can write simple programs with functions,
looping, branching, and recursion:

```rust
fn fact(x: u16) -> u16 {
    if x == 1 {
        return 1;
    } else {
        return x * fact(x - 1);
    }
}

fn main() -> u16 {
    return fact(5);
}
```

These get converted into an SSA IR, inspired by LLVM:

![Control-flow graph of SSA IR instructions for the above program](img/fact_dot.png)

And finally end up as assembly programs with an "acceptable" level of optimisation.

### Gadget

The "gadget" is what I've called the final product, which runs Conway's Game of Life and draws the
result to an ILI9341 display.

It's not particularly fast, and has a rather small (30x40-ish) non-wrapping area, but it works as a
demonstration.

This only supports the Colorlight i5, as it requires the SPI peripheral to be implemented on the
core, which the TinyFPGA BX doesn't have any pins spare for unless I change things around.

With reference to [this pin diagram](https://tomverbeure.github.io/2021/01/30/Colorlight-i5-Extension-Board-Pin-Mapping.html),
the display should be connected as follows:

| i5 Pin       | Display pin |
|--------------|-------------|
| PMOD_P4A_IO1 | MOSI        |
| PMOD_P4A_IO2 | SCK         |
| PMOD_P4A_IO3 | CS          |
| PMOD_P2A_IO1 | DC          |
| PMOD_P2A_IO2 | RST         |
| Any 3V3      | VCC         |
| Any 3V3      | LED         |
| Any GND      | GND         |

MISO is not used.

## Repository Structure

This goes over the notable parts of this repository's folder hierarchy.

- `core` - The processor core implementation, and tooling which directly supports it.
  - `design` - Describes the architecture, instruction set, and EABI. Also documents the TinyFPGA BX
    specific parts of the implementation, such as the memory map
  - `gateware` - Amaranth soft-core implementation
  - `examples` - Demo software written in assembly
  - `lib` - Libraries to support development of low-level tooling, such as instruction
    encoding/decoding
  - `bin` - Assembler, emulator backend & frontend
- `lang` - Compiler implementation
  - `frontend` - Parser, tokeniser, IR generator
  - `backend` - IR definition, common analysis tools
  - `backend-core` - Translates IR to Delta Null assembly; the idea is that other `backend-X` crates
    could target other architectures
  - `examples` - Example programs
- `gadget` - Game of Life implementation

## Usage

**Note!** I really wouldn't recommend _actually_ using this project - it's not really in a state
where others could use it without running into rough edges.

### Prerequisites

You will need:

- A recent version of Rust (tested on 1.71.0 nightly)
- Python 3.10
- [`just`](https://github.com/casey/just)
- [`tinyprog`](https://pypi.org/project/tinyprog/), if uploading to a TinyFPGA BX
- [`ecpdap`](https://github.com/adamgreig/ecpdap), if uploading to a Colorlight i5
- [ZeroMQ](https://zeromq.org/download/), for socket communication between the emulator frontend and
  backend

### Build/Test Commands

- `just build` - Compile all supporting tooling (assembler, emulator, compiler).
- `just test` (or simply `just`) - Run a `just build`, then run tooling and gateware simulator
  tests.
- `just gateware-build` - Synthesise FPGA soft-core gateware. This is relatively slow, so isn't
  included in `just build`.
- `just gateware-program` - Synthesise FPGA soft-core gateware, then upload it to the connected
  board.
- `just gadget-program` - Compile gadget code, and synthesise FPGA soft-core gateware with it.

### Tooling Commands

- `just assemble` - Run the assembler to produce machine code.
- `just compiler` - Run the compiler to produce assembly.
- `just emulator-backend` - Launch emulator backend.
- `just emulator-frontend` - Launch emulator frontend, which connects to the backend with a pretty
  TUI.
