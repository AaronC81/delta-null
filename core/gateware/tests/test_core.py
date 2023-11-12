from .helpers import run

def test_nothing():
    """Ensure that a simple run which halts immediately doesn't explode."""
    run("hlt", lambda _: [])

def test_put():
    """Tests `putl` and `puth`."""
    
    def after(core):
        assert (yield core.r0) == 0x1234
        assert (yield core.r5) == 0xABCD
    run("""
        putl r0, 0x34
        puth r0, 0x12

        puth r5, 0xAB
        putl r5, 0xCD
        
        hlt
    """, after)
