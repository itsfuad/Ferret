# Ferret Compiler

A modern compiler for the Ferret programming language.

## Design philosophy
1. **It does what it looks like.**: The language syntax and semantics are designed to be intuitive and predicsymbol_table.
2. **Helpful error messages.**: The compiler provides clear and actionable error messages to help developers fix issues quickly.
3. **Nearly 0 runtime error**: The type system and compile-time checks aim to eliminate runtime errors as much as possible.
4. **Fast compilation.**: The compiler is optimized for speed to provide a smooth development experience
## Quick Start

### Run the compiler

```batch
./scripts/run.bat test/simple.fer
```

That's it! The script uses `go run` to compile and run directly.

### Build
To build the compiler binary, run:

```batch
./scripts/build.bat
```
Or to build for both wasm and native:

```batch
./scripts/build-all.bat
```

## Language Features

### Variables and Constants

```ferret
let x := 10;              // Type inference
let y: i32 = 20;          // Explicit type
const pi := 3.14;         // Constant
```
There is nothing like truethy or falsy values in Ferret. Only `bool` type is used for boolean logic.

### Types

```ferret
// Basic types
i8, i16, i32, i64         // Signed integers
u8, u16, u32, u64         // Unsigned integers
f32, f64                  // Floats
str, bool, byte           // String, boolean, byte

// Arrays
[3]i32                    // Fixed size array
[]i32                     // Dynamic array

// Optional types
i32?                      // Nullable integer

// Result types
Data ! Error              // Used with functions that can fail

fn get_data() -> i32 ! str { // we can return either an i32 or a str. But what is the error value?
    // ...
    if fail {
        return "Failed to get data"!;  // returning str as error. See the `!` operator. Yeah it marks an expression as error 
    }

    return 42; // returning i32 as success value
}
```

### Structs

```ferret
type Point struct {
    .x: f64,
    .y: f64
};

let p : Point = { .x = 1.0, .y = 2.0 }; // From the value (anonymous struct literal), type is inferred to Point because of the variable type
````
But what if the type is not specified and we want to create a Point from the literal?
Other languages may use this syntax: Point{ x: 1.0, y: 2.0 }. But we keep behaviors consistent and use the same syntax for both cases.
```ferret
let p2 := { .x = 3.0, .y = 4.0 } as Point; // Here we use `as` to cast the anonymous struct literal to Point.
```
Every type can be anonymous. Like structs, interface, enums, functions etc.

### Enums

```ferret
type Color enum {
    Red,
    Green,
    Blue
};

let color := Color::Red;
```

The `::` operator is used to access static members of types like enums and modules (symbols on other files).

### Functions

```ferret
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

### Optional Types & Elvis Operator

```ferret
let maybe: i32? = None;
let value := maybe ?: 42;  // Defaults to 42 if None. Kind of like `??` in other languages. Or we may switch to `??` later. making ?: a ternary operator.
```

### Error Handling

```ferret
fn divide(a: i32, b: i32) -> i32 ! Error {
    if b == 0 {
        return Error{ .msg = "Division by zero" }!;
    }
    return a / b;
}

const result := divide(10, 2) catch err {
    println("Error: {}", err.msg);
} 0;  // Fallback value
```

If you return from the handler block, the function will return early with that value and you don't need to provide a fallback value.
Also you can use shorthand syntax like,
```ferret
const result := divide(10, 2) catch 0;  // Fallback value
```

### Control Flow

```ferret
if x < y {
    println("x is less");
} else {
    println("x is greater or equal");
}

for i := 0; i < 10; i++ {
    println(i);
}

while condition {
    // loop body
}
```

## Error Reporting

Ferret provides beautiful, helpful error messages:

```
error[T0001]: type mismatch
  --> example.fer:10:15
   |
10 | let num: i32 = "hello";
   |                ~~~~~~~ expected i32, found str
   |
  = note: type inference determined this expression has type 'str'
  = help: convert the string to an integer using the parse function

Compilation failed with 1 error(s)



error: integer literal 200 overflows i8
  --> example.fer:13:13
   | 
12 | let e: i8 = 100;  // OK: literal 100 fits in i8
13 | let f: i8 = 200;  // Error: 200 doesn't fit in i8 (-128 to 127)
   |     -       ~~~ need at least u8 or i16
   |     |
   |     -- type 'i8'
   | 
   = help: i8 can hold values in range: -128 to 127

```

## License

See [LICENSE](LICENSE) for details.

## Contributing

Contributions are welcome! Please read the documentation to understand the compiler architecture before submitting PRs.

## Note to new contributors
Feel free to open issues or ask questions in the discussions section. We appreciate your interest in improving Ferret!
