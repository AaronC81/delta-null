use crate::util;

#[test]
fn test_if() {
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                if 1 == 1 {
                    return 123;
                }
                return 456;
            }
        ").unwrap(),
        123
    );

    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                if 1 == 0 {
                    return 123;
                }
                return 456;
            }
        ").unwrap(),
        456
    );

    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                if 1 == 1 {
                    return 123;
                } else {
                    return 456;
                }
            }
        ").unwrap(),
        123
    );

    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                if 1 == 0 {
                    return 123;
                } else {
                    return 456;
                }
            }
        ").unwrap(),
        456
    );
}

#[test]
fn test_break() {
    // One level
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var total: u16 = 0;
                var i: u16 = 0;
                loop {
                    i = i + 1;
                    total = total + 10;
                    if i == 5 {
                        break;
                    }
                }

                return total + 1; // +1 to ensure `break` gets here, rather than returning
            }
        ").unwrap(),
        51
    );

    // Nested
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var total: u16 = 0;

                // Outer loop
                var i: u16 = 0;
                loop {
                    i = i + 1;

                    // Inner loop
                    var j: u16 = 0;
                    loop {
                        j = j + 1;

                        // Incrementer
                        total = total + 2;

                        if j == 5 {
                            break;
                        }
                    }

                    if i == 5 {
                        break;
                    }
                }

                return total + 1; // +1 to ensure `break` gets here, rather than returning
            }
        ").unwrap(),
        (5 * 5) * 2 + 1
    );
}

#[test]
fn test_call() {
    // One layer
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return number();
            }
            
            fn number() -> u16 {
                return 1234;
            }            
        ").unwrap(),
        1234
    );

    // Nesting!
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return num_a() + num_b() + num_c();
            }
            
            fn num_a() -> u16 { return num_b() + 2; }
            fn num_b() -> u16 { return num_c() + 5; }
            fn num_c() -> u16 { return 10; }
        ").unwrap(),
        (10 + 2 + 5) + (10 + 5) + 10
    );

    // Parameter passing
    assert_eq!(
        util::compile_and_execute("
            fn add(a: u16, b: u16) -> u16 {
                return a + b;
            }
            
            fn main() -> u16 {
                return add(10, 20);
            }
        ").unwrap(),
        10 + 20
    );

    // Recursion
    assert_eq!(
        util::compile_and_execute("
            fn fact(x: u16) -> u16 {
                if x == 1 {
                    return 1;
                } else {
                    return x * fact(x - 1);
                }
            }
            
            fn main() -> u16 {
                return fact(5);
            }
        ").unwrap(),
        5 * 4 * 3 * 2
    );

    // Recursion with locals
    assert_eq!(
        util::compile_and_execute("
            fn fact(x: u16) -> u16 {
                var result: u16 = x;
                if result == 1 {
                    // Remains as 1!
                } else {
                    result = result * fact(result - 1);
                }

                return result;
            }
            
            fn main() -> u16 {
                return fact(5);
            }
        ").unwrap(),
        5 * 4 * 3 * 2
    );
}

#[test]
fn test_while() {
    // Simple loop
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: u16 = 0;
                while x < 10 {
                    x = x + 1;
                }
                return x;
            }
        ").unwrap(),
        10
    );

    // Breaking early
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: u16 = 0;
                while x < 10 {
                    if x == 5 { break; }
                    x = x + 1;
                }
                return x;
            }
        ").unwrap(),
        5
    );

    // Never executes
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: u16 = 20;
                while x < 10 {
                    if x == 5 { break; }
                    x = x + 1;
                }
                return x;
            }
        ").unwrap(),
        20
    );
}

#[test]
fn test_valueless_return() {
    assert_eq!(
        util::compile_and_execute("
            var x: u16;

            fn setter() {
                x = 1;
                x = 2;
                return;
                x = 3; // unreachable
            }

            fn main() -> u16 {
                setter();
                return x;
            }
        ").unwrap(),
        2
    );
}

#[test]
fn test_discard() {
    assert_eq!(
        util::compile_and_execute("
            fn do() {
                // Blank!
            }

            fn val() -> u16 {
                return 5;
            }
            
            fn main() -> u16 {
                do();
                val();
                return 2;
            }
        ").unwrap(),
        2,
    )
}
