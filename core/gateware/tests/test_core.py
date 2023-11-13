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
    
