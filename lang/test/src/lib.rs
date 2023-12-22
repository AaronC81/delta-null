#![feature(exit_status_error)]

// This library is functionally blank.
// It is only a harness for integration tests!

#[cfg(test)]
pub mod util;

#[cfg(test)]
mod test {
    use crate::util;

    #[test]
    fn test_basic() {
        assert_eq!(
            util::compile_and_execute("fn main() -> u16 { return 2; }").unwrap(), 2
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
    fn test_boolean_literals() {
        assert_eq!(
            util::compile_and_execute("
                fn main() -> bool {
                    return true;
                }
            ").unwrap(),
            1
        );

        assert_eq!(
            util::compile_and_execute("
                fn main() -> bool {
                    return false;
                }
            ").unwrap(),
            0
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
        )
    }
}
