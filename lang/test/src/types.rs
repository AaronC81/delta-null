use crate::util;

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
                (*out).x = (*p1).x + (*p2).x; // Classic style
                out.*.y = p1.*.y + p2.*.y; // Access style
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

    // Arrays in structures
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: struct { s1: u16, a: [4]u16, s2: u16 };
                x.s1 = 1;
                x.a[0] = 2;
                x.a[1] = 3;
                x.a[2] = 4;
                x.a[3] = 5;
                x.s2 = 6;

                return x.s1 + x.a[0] + x.a[1] + x.a[2] + x.a[3] + x.s2;
            }
        ").unwrap(),
        1 + 2 + 3 + 4 + 5 + 6
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

    // Arrays in structures
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                return sizeof(struct { a: u16, b: [4]u16, c: u16 });
            }
        ").unwrap(),
        6
    )
}

#[test]
fn test_string() {
    assert_eq!(
        util::compile_and_execute("
            fn main() -> u16 {
                var x: String = \"Hello\";
                return *(x as *u16);
            }
        ").unwrap(),
        5
    );
}
