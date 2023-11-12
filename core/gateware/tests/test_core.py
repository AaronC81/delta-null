from .helpers import run

def test_nothing():
    """Ensure that a simple run which halts immediately doesn't explode."""
    run([0xFFFF], lambda _: [])

def test_put():
    """Tests `putl` and `puth`."""
    
    def after(core):
        assert (yield core.r0) == 0x1234
        assert (yield core.r5) == 0xABCD
    run([
        0x1034, # putl r0, 0x34
        0x1812, # puth r0, 0x12

        0x1DAB, # puth r5, 0xAB
        0x15CD, # putl r5, 0xCD
        
        0xFFFF, # hlt
    ], after)
