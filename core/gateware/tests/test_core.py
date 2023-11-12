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
