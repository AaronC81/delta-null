# EABI

## Data

A word is 16 bits. A memory address references a word. Smaller values, like bytes, should be
extended to words across EABI boundaries.

Larger values are handled in little-endian format:

```
                0x1000   0x1001
0x1234ABCD  ->  0xABCD   0x1234
```

## Stack Pointer

Unless the stack is empty, the stack pointer (`sp`) shall always point to the final word of a pushed
item. The stack grows downwards.

Therefore, a manual stack push looks like:

```
spdec
spwrite r0
```

(This is the behaviour used by the `push` instruction.)

## Calling Convention

### Register Preservation

`r0`-`r3` are caller-saved. A function may use these as scratch registers, and overwrite their
values without preservation.

`r4`-`r7` are callee-saved. If a function uses these, they must be restored to their pre-call values
before the function returns. 

In addition, functions should ensure that they preserve `rp` until their `ret` if they call any
other functions inside their body:

```
fn1:
    ; preserve rp
    movso r0, rp
    spdec
    spwrite r0

    ; call another function
    calloff fn2/offs

    ; other things
    ; ...

    ; retrieve rp and return
    spread r0
    spinc
    movsi rp, r0    ; could also `movsi ip, r0` and skip the `ret`
    ret

fn2:
    ; ...
    ret
```

### Parameters

All of the caller-saved general-purpose registers, `r0`-`r3`, may be used to pass parameters to a
function.

Parameters are passed from left-to-right. The general-purpose registers should be used first, then
if more are required, they can be pushed onto the stack. A single argument should be stored either
entirely in registers, or entirely on the stack. (This can result in some registers being unused,
even when the stack is used.)

Any arguments smaller than a word should be rounded up to a word in size. Besides this, there are no
other alignment requirements on arguments.

### Returns

`r0`-`r1` may be used to pass a return value back from a function to its caller.

If the return value is larger than two words, it must be pushed onto the stack instead.

### Examples

#### 1: Simple Passing

```
u16 add(u16 a, u16 b);
```

- `a` is passed in `r0`
- `b` is passed in `r1`
- The return value is stored in `r0`

#### 2: Non-Word Values / Stack

```
u32 foo(u8 a, u32 b, u8 c, u16 d);
```

- `a` is extended to a word and passed in `r0`
- `b` is passed across two registers - its lower word in `r1`, and its higher word in `r2`
- `c` is extended to a word and passed in `r3`
- `d` does not fit into registers, so is passed on the stack
- The return value is stored across `r0` and `r1`
