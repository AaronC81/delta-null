from .helpers import run_sim

def test_nothing():
    """Ensure that a simple run which halts immediately doesn't explode."""
    run_sim("hlt", lambda _: [])

def test_put():
    """Tests `putl` and `puth`."""
    
    def after(core):
        assert (yield core.r0) == 0x1234
        assert (yield core.r5) == 0xABCD
    run_sim("""
        putl r0, 0x34
        puth r0, 0x12

        puth r5, 0xAB
        putl r5, 0xCD
        
        hlt
    """, after)

def test_mov():
    """Tests `mov`."""
    
    def after(core):
        assert (yield core.r0) == 0x1234
        assert (yield core.r7) == 0x1234
        assert (yield core.r6) == 0
    run_sim("""
        putl r6, 0
        puth r6, 0
        
        putl r3, 0x34
        puth r3, 0x12

        mov r0, r3
        mov r7, r0
        
        hlt
    """, after)

def test_read():
    """Tests `read`."""
    
    def after(core):
        assert (yield core.r1) == 0x1234
        assert (yield core.r2) == 0xABCD
        assert (yield core.r3) == 0x1234
        assert (yield core.r4) == 0xABCD
    run_sim("""
        putl r0, data/lo
        puth r0, data/hi
        putl r7, other/lo
        puth r7, other/hi
        read r1, r0
        read r2, r7
        read r3, r0
        read r4, r7
        hlt
            
        data: .word 0x1234
        other: .word 0xABCD
    """, after)

def test_write():
    """Tests `write`."""

    def after(core):
        assert (yield core.r3) == 0x1234
        assert (yield core.r4) == 0xABCD
        assert (yield core.r5) == 0xABCD
    run_sim("""
        ; r0 = address of `data`
        putl r0, data/lo
        puth r0, data/hi
        
        ; r1 = content of `new1`
        putl r1, new1/lo
        puth r1, new1/hi
        read r1, r1
            
        ; r2 = content of `new2`
        putl r2, new2/lo
        puth r2, new2/hi
        read r2, r2
            
        ; simple write
        read r3, r0
        write r0, r1
        read r4, r0
            
        ; stress test
        write r0, r1
        write r0, r2
        write r0, r1
        write r0, r2
        write r0, r1
        read r5, r0
            
        hlt
            
        data: .word 0x1234
        new1: .word 0xABCD
        new2: .word 0xEF78
    """, after)
    
def test_sp_ops():
    """Tests SP-specific operations - `spadd`, `spinc`, `spdec`."""

    def after(core):
        assert (yield core.r1) == 21
        assert (yield core.r2) == 22
        assert (yield core.r3) == 21
    run_sim("""
        ; sp = 16
        putl r0, 16
        puth r0, 0
        movsi sp, r0

        ; r0 = 5
        putl r0, 5
        
        ; r1 = (sp += r0)
        spadd r0
        movso r1, sp

        ; r2 = (sp++)
        spinc
        movso r2, sp

        ; r3 = (sp--)
        spdec
        movso r3, sp
            
        hlt
    """, after)

def test_ip_movsi():
    """Tests jumping implemented by `movsi`-ing into IP."""

    def after(core):
        assert (yield core.r0) == 5
    run_sim("""
        ; r0 = 0
        putl r0, 0
        puth r0, 0

        ; jump to dest (thru r1)
        putl r1, dest/lo
        puth r1, dest/hi
        movsi ip, r1

        hlt
        hlt
        hlt
        hlt
        hlt

        dest:
        putl r0, 5
        hlt
    """, after)

def test_bit_ops():
    """Tests some bitwise operations: `not`, `and`, `or`, `xor`."""

    def after(core):
        value =                   0b_00010100_01011010
        mask  =                   0b_10111000_00001001

        assert (yield core.r2) == 0b_11101011_10100101 # not
        assert (yield core.r3) == 0b_00010000_00001000 # and
        assert (yield core.r4) == 0b_10111100_01011011 # or
        assert (yield core.r5) == 0b_10101100_01010011 # xor

    run_sim("""
        ; r0 = value
        putl r0, 0b01011010
        puth r0, 0b00010100

        ; r1 = mask
        putl r1, 0b00001001
        puth r1, 0b10111000

        mov r2, r0
        not r2

        mov r3, r0
        and r3, r1

        mov r4, r0
        or r4, r1

        mov r5, r0
        xor r5, r1

        hlt
    """, after)

def test_bit_shift():
    """Tests bit shift operators: `shl`, `shr`."""

    def after(core):
        value =                   0b_00010100_01011010

        assert (yield core.r2) == 0b_10100010_11010000 # shl
        assert (yield core.r3) == 0b_00000010_10001011 # shr
        assert (yield core.r4) == 0 # extreme shl

    run_sim("""
        ; r0 = value
        putl r0, 0b01011010
        puth r0, 0b00010100

        ; r1 = 3
        putl r1, 3
        puth r1, 0

        mov r2, r0
        shl r2, r1

        mov r3, r0
        shr r3, r1

        ; extreme
        putl r1, 24
        mov r4, r0
        shl r4, r1

        hlt
    """, after)

def test_comparison_eq():
    """Tests equality comparison operators: `eqz`, `eq`."""

    def after(core):
        assert (yield core.r3) == 0
        assert (yield core.r4) > 0
        assert (yield core.r5) > 0
        assert (yield core.r6) > 0
        assert (yield core.r7) == 0

    run_sim("""
        ; r0 = mask with extracts just comparison flag
        ;      also, conveniently, 2 - we'll use this as one of the comparison items
        putl r0, 0b00000010
        puth r0, 0b00000000

        ; r1 = 5
        putl r1, 5
        puth r1, 0
            
        ; r2 = 2
        putl r2, 2
        puth r2, 0
            
        ; r3 = (r1 == r2)   - false
        eq r1, r2
        movso r3, ef
        and r3, r0
            
        ; r4 = (r0 == r2)   - true
        eq r0, r2
        movso r4, ef
        and r4, r0
            
        ; r5 = (r0 == r0)   - true
        eq r0, r0
        movso r5, ef
        and r5, r0
            
        ; r1 = 0
        xor r1, r1
            
        ; r6 = (r1 == 0)    - true
        eqz r1
        movso r6, ef
        and r6, r0
            
        ; r7 = (r2 == 0)   - false
        eqz r2
        movso r7, ef
        and r7, r0

        hlt
    """, after)

def test_comparison_ord():
    """Tests ordered comparison operators: `gt`, `gteq`."""

    def after(core):
        assert (yield core.r3) > 0
        assert (yield core.r4) == 0
        assert (yield core.r5) == 0
        assert (yield core.r6) > 0

    run_sim("""
        ; r0 = mask with extracts just comparison flag
        ;      also, conveniently, 2 - we'll use this as one of the comparison items
        putl r0, 0b00000010
        puth r0, 0b00000000

        ; r1 = 5
        putl r1, 5
        puth r1, 0
                    
        ; r3 = (r1 > r0)   - true
        gt r1, r0
        movso r3, ef
        and r3, r0
            
        ; r4 = (r0 > r1)   - false
        gt r0, r1
        movso r4, ef
        and r4, r0
            
        ; r5 = (r0 > r0)   - false
        gt r0, r0
        movso r5, ef
        and r5, r0
            
        ; r6 = (r0 >= r0)    - true
        gteq r0, r0
        movso r6, ef
        and r6, r0

        hlt
    """, after)

def test_cjmp():
    """Tests `cjmp` by implementing a count-based loop construct."""

    def after(core):
        assert (yield core.r0) == (1 << 10)

    run_sim("""
        ; total, starts at 1
        putl r0, 1
        puth r0, 0

        ; counter
        ; at the time of writing, we don't have arithmetic instructions
        ; instead, use a bit-shift representation
        putl r1, 0b10000
        puth r1, 0
            
        ; constant 1
        putl r2, 1
        puth r2, 0

        putl r3, loop/lo
        puth r3, loop/hi
        loop:
            shl r0, r2
            shl r0, r2
            shr r1, r2
            
            eqz r1
            inv
            cjmp r3

        hlt
    """, after)

def test_gp_add_sub():
    """Tests `inc`, `dec`, `add`, and `sub`."""

    def after(core):
        assert (yield core.r2) == 18
        assert (yield core.r3) == 16
        assert (yield core.r4) == 28
        assert (yield core.r5) == 6

    run_sim("""
        putl r0, 17
        puth r0, 0

        putl r1, 11
        puth r1, 0
            
        mov r2, r0
        inc r2
            
        mov r3, r0
        dec r3
            
        mov r4, r0
        add r4, r1

        mov r5, r0
        sub r5, r1
            
        hlt
    """, after)

def test_gp_mul():
    """Tests `mul`."""

    def after(core):
        assert (yield core.r3) == (17 * 11)
        assert (yield core.r4) == 0

    run_sim("""
        putl r0, 17
        puth r0, 0

        putl r1, 11
        puth r1, 0
            
        xor r2, r2
            
        mov r3, r0
        mul r3, r1
            
        mov r4, r0
        mul r4, r2
            
        hlt
    """, after)

def test_gp_neg():
    """Tests `neg`."""

    def after(core):
        assert (yield core.r0) == 0
        assert (yield core.r1) == 0xFFFB # -5

    run_sim("""
        xor r0, r0
        neg r0

        putl r1, 5
        puth r1, 0
        neg r1
            
        hlt
    """, after)

def test_call():
    """Tests `call` and `ret`."""

    def after(core):
        assert (yield core.r1) == (12 + 5)
        assert (yield core.r2) == (100 + 5)

    run_sim("""
        putl r0, add5/lo
        puth r0, add5/hi

        putl r7, 12
        puth r7, 0
        call r0
        mov r1, r7
            
        putl r7, 100
        puth r7, 0
        call r0
        mov r2, r7
            
        hlt
            

        ; adds 5 to r7
        ; trashes r6
        add5:
            putl r6, 5
            puth r6, 0
            add r7, r6
            ret
    """, after)

def test_stack_manual():
    """Tests behaviour of stack instructions like `spread`, `spwrite`, `spinc`, and `spdec`."""

    def after(core):
        assert (yield core.r1) == 0xABCD
        assert (yield core.r2) == 0x1234
        assert (yield core.r3) == 0x5678
        assert (yield core.r4) == 0xDCBA

    run_sim("""
        ; sp = 0xFFFF
        putl r0, 0xFF
        puth r0, 0xFF
        movsi sp, r0
            
        ; sp[0] (top) = 0xDCBA
        putl r0, 0xBA
        puth r0, 0xDC
        spwrite 0, r0

        spdec
        spdec
        spdec
        
        ; sp[0] = 0xABCD
        putl r0, 0xCD
        puth r0, 0xAB
        spwrite 0, r0
            
        ; sp[1] = 0x1234
        putl r0, 0x34
        puth r0, 0x12
        spwrite 1, r0
            
        ; sp[2] = 0x5678
        putl r0, 0x78
        puth r0, 0x56
        spwrite 2, r0
            
        ; read stack items back
        spread r1, 0
        spread r2, 1
        spread r3, 2
        spread r4, 3
            
        hlt
    """, after)

def test_stack_auto():
    """Tests behaviour of easier stack instructions: `push` and `pop`."""

    def after(core):
        assert (yield core.r1) == 0x5678
        assert (yield core.r2) == 0x1234
        assert (yield core.r3) == 0xABCD

    run_sim("""
        ; sp = 0xFFFF
        .put r0, 0xFFFF
        movsi sp, r0
            
        ; Push some stuff
        .put r0, 0xABCD
        push r0
        .put r0, 0x1234
        push r0
        .put r0, 0x5678
        push r0
            
        ; Pop into registers for validation
        pop r1
        pop r2
        pop r3
            
        hlt
    """, after)

def test_offset_jumps():
    """Tests offset-immediate jump instructions: `jmpoff` and `cjmpoff`."""

    def after(core):
        assert (yield core.r0) == 19
        assert (yield core.r1) == 11

    run_sim("""
        xor r0, r0
        jmpoff target/offset

        hlt
        hlt
        target:
            putl r0, 19
            jmpoff after/offset
        hlt
        hlt
        after:
        
        xor r1, r1
        eq r1, r0
        cjmpoff end/offset
        inv
        cjmpoff correct/offset    
        
        end: hlt
            
        correct:
            putl r1, 11
            jmpoff end/offset
    """, after)
