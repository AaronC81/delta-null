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
    fn test_literals() {
        assert_eq!(
            util::compile_and_execute("
                fn main() -> u16 {
                    return 0xAB + 12 + 0b1101;
                }
            ").unwrap(),
            0xAB + 12 + 0b1101
        )
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
    }

    #[test]
    fn test_booleans() {
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

        // Check passing booleans around without using them immediately
        // (Because we have an optimisation for when we *do* use them immediately)
        assert_eq!(
            util::compile_and_execute("
                fn ternary(cond: bool, if_t: u16, if_f: u16) -> u16 {
                    if cond {
                        return if_t;
                    }
                    return if_f;
                }

                fn main() -> u16 {
                    return
                        ternary(
                            2 + 2 == 4,
                            15,
                            7
                        ) +
                        ternary(
                            2 + 2 == 5,
                            6,
                            1,
                        );
                }
            ").unwrap(),
            15 + 1
        )
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
    fn test_pointers() {
        // Simple writing
        assert_eq!(
            util::compile_and_execute("
                fn write(ptr: *u16) -> u16 {
                    *ptr = 3;
                    return 15;
                }
                
                fn main() -> u16 {
                    var x: u16 = 0;
                    var y: u16 = write(&x);
                    return x + y;
                }
            ").unwrap(),
            15 + 3,
        );

        // Indirection
        assert_eq!(
            util::compile_and_execute("                
                fn write(ptr: **u16) {
                    **ptr = 42;
                }
                
                fn main() -> u16 {
                    var x: u16 = 0;
                    var xptr: *u16 = &x;
                    write(&xptr);
                    return *xptr + x + 1;
                }
            ").unwrap(),
            42 + 42 + 1,
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
    fn test_type_alias() {
        // Check that type aliases compile. Most of the heavy lifting happens at type checking, so
        // it's probably fine :)
        assert_eq!(
            util::compile_and_execute("
                type Word = u16;

                fn main() -> u16 {
                    var x: Word = 14;
                    var y: Word = 16;
                    var z: Word = x + y;
                    return z;
                }
            ").unwrap(),
            30
        );
    }

    #[test]
    fn test_complex_lvalue() {
        // Test that something moderately complex (e.g. the result of a function call) is accepted
        // as an lvalue
        assert_eq!(
            util::compile_and_execute("
                fn third_element(start: *u16) -> *u16 {
                    return start + 2;
                }

                fn main() -> u16 {
                    var items: [10]u16;
                    *(third_element(&items as *u16)) = 24;

                    return items[2];
                }
            ").unwrap(),
            24
        );
    }

    #[test]
    fn test_struct() {
        // Simple instantiation
        assert_eq!(
            util::compile_and_execute("
                type Point = struct { x: i16, y: i16 };

                fn main() -> u16 {
                    var pt: Point;
                    return 5;
                }
            ").unwrap(),
            5
        );

        // Field access
        assert_eq!(
            util::compile_and_execute("
                type Point = struct { x: u16, y: u16 };

                fn main() -> u16 {
                    var pt: Point;
                    pt.x = 2;
                    pt.y = 3;
                    return pt.x + pt.y;
                }
            ").unwrap(),
            2 + 3
        );

        // Passing pointers
        assert_eq!(
            util::compile_and_execute("
                type Point = struct { x: u16, y: u16 };

                fn add_points(p1: *Point, p2: *Point, out: *Point) {
                    (*out).x = (*p1).x + (*p2).x;
                    (*out).y = (*p1).y + (*p2).y;
                }

                fn main() -> u16 {
                    var p1: Point;
                    p1.x = 5;
                    p1.y = 10;

                    var p2: Point;
                    p2.x = 2;
                    p2.y = 3;

                    var out: Point;

                    add_points(&p1, &p2, &out);
                    
                    return out.x * out.y;
                }
            ").unwrap(),
            (5 + 2) * (10 + 3)
        );
    }

    #[test]
    fn test_sizeof() {
        // Scalar
        assert_eq!(
            util::compile_and_execute("
                fn main() -> u16 {
                    return sizeof(u16);
                }
            ").unwrap(),
            1
        );

        // Struct
        assert_eq!(
            util::compile_and_execute("
                fn main() -> u16 {
                    return sizeof(struct { a: u16, b: u16, c: u16 });
                }
            ").unwrap(),
            3
        );

        // Array
        assert_eq!(
            util::compile_and_execute("
                fn main() -> u16 {
                    return sizeof([4]struct { a: u16, b: u16, c: u16 });
                }
            ").unwrap(),
            4 * 3
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
}
