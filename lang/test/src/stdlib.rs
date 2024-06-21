use crate::util;

#[test]
fn test_string_data_length() {
    // Simple case
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                return string_data_length(\"Hello\");
            }
        ").unwrap(),
        5
    );

    // Surrogate pairs - both code units count
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                return string_data_length(\"😻 Meow\");
            }
        ").unwrap(),
        7
    );
}

#[test]
fn test_string_char_length() {
    // Simple case
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                return string_char_length(\"Hello\");
            }
        ").unwrap(),
        5
    );

    // Surrogate pairs - the emoji should count as a single character despite being made up of two
    // code units
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                return string_char_length(\"😻 Meow\");
            }
        ").unwrap(),
        6
    );

    // Many surrogate pairs
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                return string_char_length(\"😻🚞🌄\");
            }
        ").unwrap(),
        3
    );
}

#[test]
fn test_string_validate() {
    // All sorts of valid strings
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                if (!(string_validate(\"Hello\"))) { return 1; }
                if (!(string_validate(\"😻 Meow\"))) { return 2; }
                if (!(string_validate(\"😻🚞🌄\"))) { return 3; }

                return 0;
            }
        ").unwrap(),
        0
    );

    // Craft a string which ends in the middle of a surrogate pair, which is invalid
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                var s: String = \"😻🚞🌄\";
                *(string_data_length_ptr(s)) = string_data_length(s) - 1;

                if (string_validate(s)) {
                    return 1;
                } else {
                    return 0;
                }
            }
        ").unwrap(),
        0
    );

    // Craft a string which has a new character after the high surrogate
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                var s: String = \"😻 Meow\";
                *(string_data(s) + 1) = 0x41; // 'A'

                if (string_validate(s)) {
                    return 1;
                } else {
                    return 0;
                }
            }
        ").unwrap(),
        0
    );

    // Craft a string which has a low surrogate without a high surrogate first
    assert_eq!(
        util::compile_and_execute("
            use \"string.dnc\";
            fn main() -> u16 {
                var s: String = \"😻 Meow\";
                *(string_data(s)) = 0x41; // 'A'

                if (string_validate(s)) {
                    return 1;
                } else {
                    return 0;
                }
            }
        ").unwrap(),
        0
    );
}
