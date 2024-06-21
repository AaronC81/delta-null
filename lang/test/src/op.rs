use crate::util;

#[test]
fn test_equality() {
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var total: u16 = 0;
                var i: u16 = 0;
                loop {
                    i = i + 1;
                    total = total + i;
                    if i == 5 {
                        return total;
                    }
                }

                return 0; // Shouldn't be necessary!
            }
        ").unwrap(),
        1 + 2 + 3 + 4 + 5
    );
}

#[test]
fn test_ordering_comparisons() {
    // TODO: this should be possible with a cast
    let bool_to_u16 = "fn bool_to_u16(b: bool) -> u16 { if b { return 1; } else { return 0; } }\n";

    // >, truthy
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(5 > 2);
            }}
        ")).unwrap(),
        1,
    );

    // >, falsey
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(5 > 10);
            }}
        ")).unwrap(),
        0,
    );

    // <, truthy
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(3 < 10);
            }}
        ")).unwrap(),
        1,
    );

    // <, falsey
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(3 < 2);
            }}
        ")).unwrap(),
        0,
    );

    // <=, truthy
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(3 <= 10);
            }}
        ")).unwrap(),
        1,
    );

    // <=, equal
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(3 <= 3);
            }}
        ")).unwrap(),
        1,
    );

    // <=, falsey
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(3 <= 2);
            }}
        ")).unwrap(),
        0,
    );

    // >=, truthy
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(10 >= 3);
            }}
        ")).unwrap(),
        1,
    );

    // >=, equal
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(3 >= 3);
            }}
        ")).unwrap(),
        1,
    );

    // >=, falsey
    assert_eq!(
        util::compile_and_execute(&format!("
            {bool_to_u16}
            fn main() -> u16 {{
                return bool_to_u16(3 >= 10);
            }}
        ")).unwrap(),
        0,
    );
}

#[test]
fn test_add_sub() {
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return 1 + 6 - 2 + 4;
            }
        ").unwrap(),
        1 + 6 - 2 + 4
    );
}

#[test]
fn test_bitwise() {
    // AND
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return 15 & 19;
            }
        ").unwrap(),
        15 & 19
    );

    // XOR
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return 13 ^ 19;
            }
        ").unwrap(),
        13 ^ 19
    );

    // OR
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return 5 | 19;
            }
        ").unwrap(),
        5 | 19
    );

    // Precedence
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return 1 | 3 & 5 ^ 3;
            }
        ").unwrap(),
        1 | ((3 & 5) ^ 3)
    );

    // NOT
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return ~1234;
            }
        ").unwrap(),
        !1234u16
    );

    // Left-shift
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return 12 << 3;
            }
        ").unwrap(),
        12 << 3
    );

    // Right-shift
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return 54 >> 2;
            }
        ").unwrap(),
        54 >> 2
    );
}

#[test]
fn test_mul() {
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return 1 + 2 * 4 + 1;
            }
        ").unwrap(),
        1 + 2 * 4 + 1
    );

    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return (1 + 2) * (4 + 1);
            }
        ").unwrap(),
        (1 + 2) * (4 + 1)
    );
}

#[test]
fn test_cast() {
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: u16 = 0;
                var y: u16 = 0;
                var z: u16 = 0;

                // This is a very crappy test, but hey! It works! For now!
                return (&z as u16) - (&x as u16);
            }
        ").unwrap(),
        2,
    )
}

#[test]
fn test_array_index() {
    // Just reads; do writes with pointers
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: [4]u16;
                *((&x as *u16) + 1) = 123;
                *((&x as *u16) + 2) = 456;

                return x[1] + x[2];
            }
        ").unwrap(),
        123 + 456,
    );

    // Reads and writes
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: [4]u16;
                x[1] = 123;
                x[2] = 456;

                return x[1] + x[2];
            }
        ").unwrap(),
        123 + 456,
    );

    // Compute fibonacci sequence
    assert_eq!(
        util::compile_and_execute("
            fn fib(count: u16, results: *u16) {
                // Initialise
                *(results    ) = 0;
                *(results + 1) = 1;
                var i: u16 = 2;
            
                loop {
                    if i == count {
                        break;
                    }
            
                    *(results + i) = *(results + (i - 1)) + *(results + (i - 2));
            
                    i = i + 1;
                }
            }
            
            fn main() -> u16 {
                var results: [10]u16;
                fib(10, &results as *u16);

                return results[9];
            }
        ").unwrap(),
        34
    );
}

#[test]
fn test_boolean_not() {
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: bool = true;
                if (!x) {
                    return 4;
                } else {
                    return 8; // <<
                }
            }
        ").unwrap(),
        8
    );

    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: bool = false;
                if (!x) {
                    return 4; // <<
                } else {
                    return 8; 
                }
            }
        ").unwrap(),
        4
    );
}
