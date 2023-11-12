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
