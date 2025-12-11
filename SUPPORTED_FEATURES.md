# Ferret - Supported Features Guide

## ✅ What You CAN Write (Fully Working)

### 1. Variables & Constants
```ferret
let x := 10;              // Type inference
let y: i32 = 20;          // Explicit type
const PI := 3.14159;      // Constants
```

### 2. Basic Types
- **Integers**: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- **Floats**: `f32`, `f64`
- **Strings**: `str`
- **Booleans**: `bool`
- **Bytes**: `byte`

### 3. Arithmetic Operations
```ferret
let a: i32 = 10;
let b: i32 = 5;

a + b   // ✅ Addition
a - b   // ✅ Subtraction
a * b   // ✅ Multiplication
a / b   // ✅ Division
a % b   // ✅ Modulo
```

### 4. Comparison Operations
```ferret
x == y   // ✅ Equal
x != y   // ✅ Not equal
x < y    // ✅ Less than
x > y    // ✅ Greater than
x <= y   // ✅ Less than or equal
x >= y   // ✅ Greater than or equal
```

### 5. Logical Operations
```ferret
a && b   // ✅ Logical AND
a || b   // ✅ Logical OR
!a       // ✅ Logical NOT
```

### 6. If-Else Statements
```ferret
if condition {
    // code
} else {
    // code
}

// Else-if chains
if x > 10 {
} else if x > 5 {
} else {
}
```

### 7. Functions
```ferret
// Simple function
fn greet() {
    io::Println("Hello!");
}

// With parameters and return
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

// Main function (entry point)
fn main() {
    greet();
    let result := add(5, 3);
}
```

### 8. Variable Assignment
```ferret
let x: i32 = 10;
x = 20;        // ✅ Reassign
x = x + 5;     // ✅ Reassign with expression
```

### 9. Native I/O Functions
```ferret
import "std/io";

io::Println("Hello");     // ✅ Print with newline
io::Print("Text");        // ✅ Print without newline
io::ReadInt();            // ✅ Read integer (returns i32 ! str)
io::ReadFloat();          // ✅ Read float (returns f64 ! str)
```

## ❌ What You CANNOT Write Yet

### Not Codegen'd (Will Type-Check but Won't Generate Code)
- **Loops**: `for`, `while` - Parsed ✅, Type-checked ✅, Codegen ❌
- **Arrays**: `[]T`, `[N]T` - Parsed ✅, Type-checked ✅, Codegen ❌
- **Structs**: `struct { ... }` - Parsed ✅, Type-checked ✅, Codegen ❌
- **Maps**: `map[K]V` - Parsed ✅, Type-checked ✅, Codegen ❌
- **Error Handling**: `E ! T`, `catch` - Parsed ✅, Codegen ❌
- **Optional Types**: `T?`, `??` - Parsed ✅, Type-checked ✅, Codegen ❌

### Type Errors (By Design)
- **String + Number**: `"value: " + 10` → Type error (str + i32 not supported)

## How to Compile and Run

### Method 1: Using `go run` (Development)

```bash
# Compile and build executable
go run main.go myprogram.fer

# Run the executable
./myprogram/bin/myprogram
```

### Method 2: Using Built Binary

```bash
# Build compiler first
go build -o bin/ferret main.go

# Compile your program
./bin/ferret myprogram.fer

# Run executable
./myprogram/bin/myprogram
```

### Complete Example

**1. Create `hello.fer`:**
```ferret
import "std/io";

fn main() {
    io::Println("Hello from Ferret!");
}
```

**2. Compile:**
```bash
go run main.go hello.fer
```

**3. Run:**
```bash
./hello/bin/hello
```

**Output:**
```
Hello from Ferret!
```

## What Gets Generated

When you compile `myprogram.fer`:

```
myprogram/
└── bin/
    ├── myprogram.c    # Generated C source code
    └── myprogram      # Executable binary (ready to run)
```

## Working Examples

### Example 1: Simple I/O
```ferret
import "std/io";

fn main() {
    io::Println("Hello, World!");
}
```

### Example 2: Variables and If-Else
```ferret
import "std/io";

fn main() {
    let x: i32 = 10;
    let y: i32 = 20;
    
    if x < y {
        io::Println("x is less than y");
    } else {
        io::Println("x is greater than or equal to y");
    }
}
```

### Example 3: Functions
```ferret
import "std/io";

fn calculate(x: i32, y: i32) -> i32 {
    return x * y;
}

fn main() {
    let result := calculate(5, 3);
    io::Println("Calculation done");
}
```

## Quick Reference

| Feature | Status | Notes |
|---------|--------|-------|
| Variables (`let`) | ✅ **Works** | Full support |
| Constants (`const`) | ✅ **Works** | Full support |
| If-else | ✅ **Works** | Full support |
| Functions | ✅ **Works** | Parameters & returns |
| Arithmetic | ✅ **Works** | +, -, *, /, % |
| Comparisons | ✅ **Works** | ==, !=, <, >, <=, >= |
| Logical ops | ✅ **Works** | &&, \|\|, ! |
| I/O (native) | ✅ **Works** | Println, Print, ReadInt, ReadFloat |
| For loops | ❌ **Not codegen'd** | Parsed but no codegen |
| While loops | ❌ **Not codegen'd** | Parsed but no codegen |
| Arrays | ❌ **Not codegen'd** | Parsed but no codegen |
| Structs | ❌ **Not codegen'd** | Parsed but no codegen |
| String + Number | ❌ **Type error** | By design (use separate prints) |

## Troubleshooting

### "cannot resolve import: std/io"
- ✅ Should work automatically (native modules auto-register)
- Check that `ferret_libs/std/io.fer` exists

### "build failed: runtime directory not found"
- ✅ Should auto-detect runtime path
- Ensure `runtime/io.c` exists in project root

### Executable not found
- Check compilation completed successfully
- Look for `yourfile/bin/yourfile` file
- Ensure it has execute permissions

## Next Steps

To add more features:
1. Extend `internal/codegen/cgen/cgen.go`
2. Add handlers for new AST nodes
3. Add runtime functions if needed
4. Test with examples
