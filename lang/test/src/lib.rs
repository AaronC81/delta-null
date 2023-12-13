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
}
