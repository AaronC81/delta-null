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
            util::compile_and_execute("fn main() { return 2; }").unwrap(), 2
        );
    }

    #[test]
    fn test_variables() {
        assert_eq!(
            util::compile_and_execute("
                fn main() {
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
                fn main() {
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
                fn main() {
                    if 1 == 0 {
                        return 123;
                    }
                    return 456;
                }
            ").unwrap(),
            456
        );
    }

    #[test]
    fn test_equality() {
        // TODO: compilation fails without useless `return 0`
        assert_eq!(
            util::compile_and_execute("
                fn main() {
                    var total: u16 = 0;
                    var i: u16 = 0;
                    loop {
                        i = i + 1;
                        total = total + i;
                        if i == 5 {
                            return total;
                        }
                    }
                    return 0;
                }
            ").unwrap(),
            1 + 2 + 3 + 4 + 5
        );
    }
}
