# Ferret Compiler

A modern compiler for the Ferret programming language, featuring:
- Go-style AST architecture
- Rust-style error diagnostics
- Rich type system with inference
- Compile-time error detection
- Module system

## Quick Start

### Run the compiler

```batch
run.bat test/simple.fer
```

That's it! The script uses `go run` to compile and run directly.

## Language Features

### Variables and Constants

```ferret
let x := 10;              // Type inference
let y: i32 = 20;          // Explicit type
const pi := 3.14;         // Constant
```

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

// Error types
Data ! Error              // Can return error
```

### Structs

```ferret
type Point struct {
    .x: f64,
    .y: f64
};

let p := Point{ .x = 1.0, .y = 2.0 };
```

### Enums

```ferret
type Color enum {
    Red,
    Green,
    Blue
};

let color := Color::Red;
```

### Functions

```ferret
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

### Optional Types & Elvis Operator

```ferret
let maybe: i32? = None;
let value := maybe ?: 42;  // Defaults to 42 if None
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

### Pattern Matching

```ferret
when color {
    Color::Red => println("Red"),
    Color::Green => println("Green"),
    Color::Blue => println("Blue")
}
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
```

## Scripts

**Single script for everything:**

```batch
run.bat <file.fer>     # Compile a Ferret file
```

Set `DEBUG=1` for debug output:
```batch
set DEBUG=1
run.bat test/simple.fer
```

## Development

### Running the Compiler

```batch
run.bat test/simple.fer
```

### With Debug Output

```batch
set DEBUG=1
run.bat test/simple.fer
```

### Running Tests

```batch
go test ./...
```

## Status

### Completed
- [x] Go-style AST architecture
- [x] Rust-style diagnostic system
- [x] Semantic information system
- [x] Type system infrastructure
- [x] Module system design
- [x] Lexer implementation
- [x] Error code taxonomy
- [x] **Shared context architecture**
- [x] **Multi-file compilation pipeline**

### 🚧 In Progress
- [ ] Parser implementation
- [ ] Collector (declaration gathering)
- [ ] Symbol resolution
- [ ] Type checker
- [ ] Code generation

## License

MIT License

## Contributing

Contributions are welcome! Please read the documentation to understand the compiler architecture before submitting PRs.
