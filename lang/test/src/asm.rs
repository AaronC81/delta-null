use crate::util;

#[test]
fn test_inline_assembly() {
    assert_eq!(
        util::compile_and_execute("
            fn value() -> u16 {
                asm {
                    .put r0, 0x1234
                    spinc ; pop preserved rp
                    spinc ; pop preserved r4
                    ret
                }
                return 0; // satisfy tc
            }

            fn main() -> u16 {
                return value() + 1;
            }
        ").unwrap(),
        0x1234 + 1,
    )
}


#[test]
fn test_extern_fn() {
    // Traditional inline assembly
    assert_eq!(
        util::compile_and_execute("
            extern fn number() -> u16;
            fn dummy() {
                asm {
                    number:
                        .put r0, 0xAB
                        ret
                }
            }

            fn main() -> u16 {
                return number();
            }
        ").unwrap(),
        0xAB
    );

    // Top-level assembly
    assert_eq!(
        util::compile_and_execute("
            extern fn number() -> u16;
            asm {
                number:
                    .put r0, 0xAB
                    ret
            }

            fn main() -> u16 {
                return number();
            }
        ").unwrap(),
        0xAB
    );
}
