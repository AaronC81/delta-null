use crate::util;

#[test]
fn test_global_var() {
    // Uninitialised
    assert_eq!(
        util::compile_and_execute("
            var counter: u16;

            fn add(v: u16) {
                counter = counter + v;
            }

            fn main() -> u16 {
                counter = 2;
                add(5);
                add(7);
                add(3);
                return counter;
            }
        ").unwrap(),
        2 + 5 + 7 + 3
    );

    // Initialised
    assert_eq!(
        util::compile_and_execute("
            var counter: u16 = 6;

            fn add(v: u16) {
                counter = counter + v;
            }

            fn main() -> u16 {
                add(5);
                add(7);
                add(3);
                return counter;
            }
        ").unwrap(),
        6 + 5 + 7 + 3
    );
}

#[test]
fn test_variables() {
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var a: u16 = 5;
                var b: u16 = 12;
                return a + b;
            }
        ").unwrap(),
        5 + 12
    )
}

#[test]
fn test_init_entry_flow() {
    // Initialised
    assert_eq!(
        util::compile_and_execute("
            var x: u16 = 5;

            fn main() -> u16 {
                return x;
            }
        ").unwrap(),
        5
    );    
}
