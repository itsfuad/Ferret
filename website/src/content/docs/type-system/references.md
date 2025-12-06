---
title: Reference Types
description: Working with reference types in Ferret
---

Reference types in Ferret provide a way to pass data by reference rather than by value. When you use a reference type `&T`, you're working with a reference to a value of type `T` rather than a copy of the value.

## What Are Reference Types?

By default, Ferret passes values by copy (like Go and Rust). When you pass a struct to a function, it gets copied. Reference types let you explicitly pass a reference instead, avoiding the copy.

```ferret
type LargeData struct {
    .buffer: [1000]i32,
    .metadata: str,
};

// Without reference - copies the entire struct (4KB + string)
fn process_copy(data: LargeData) {
    // Works with a copy of data
}

// With reference - only passes a pointer (8 bytes)
fn process_ref(data: &LargeData) {
    // Works with the original data via reference
}
```

## Reference Type Syntax

Use `&` before a type to declare a reference type. The `&` is only used in type position:

```ferret
// Function parameter with reference type
fn process(data: &LargeData) {
    // data is automatically a reference
}

// Variable with reference type
let config_ref: &Config = get_config();
```

When you call a function with a reference parameter, Ferret automatically passes the argument by reference - no special syntax needed!

## When to Use References

### Performance Optimization

References avoid copying large data structures:

```ferret
type GameState struct {
    .players: [100]Player,
    .world: WorldMap,
    .physics: PhysicsEngine,
};

// Efficient - no copy
fn update_game(state: &GameState) {
    // Read and modify the game state
}
```

### Shared Access

Multiple parts of your code can reference the same data:

```ferret
type Config struct {
    .database_url: str,
    .max_connections: i32,
    .timeout: i32,
};

fn setup_database(config: &Config) {
    // Use config.database_url
}

fn setup_cache(config: &Config) {
    // Use config.max_connections
}

let config := { 
    .database_url: "localhost:5432",
    .max_connections: 100,
    .timeout: 5000
} as Config;

// Just pass the value - Ferret handles the reference automatically
setup_database(config);
setup_cache(config);
```

## References vs Values

Understanding when to use each:

| Aspect | Value (`T`) | Reference (`&T`) |
|--------|-------------|------------------|
| **Copying** | Creates a copy | Passes a pointer |
| **Size overhead** | Full size of T | Always 8 bytes (pointer) |
| **Mutation** | Can't affect original | Can affect original |
| **Safety** | Always safe | Must ensure validity |
| **Default** | ✅ Ferret default | Opt-in with `&` |

## Automatic Dereferencing

When you have a reference type parameter or variable, Ferret **automatically dereferences** it for field access and method calls. You don't need any special syntax:

```ferret
type Point struct {
    .x: i32,
    .y: i32,
};

fn (p: Point) distance() -> f64 {
    return math::sqrt(p.x * p.x + p.y * p.y);
}

fn process_point(p_ref: &Point) {
    // Automatic dereferencing - just use it naturally
    let x := p_ref.x;              // Access field directly
    let dist := p_ref.distance();  // Call method directly
    
    // No need for (*p_ref).x or (*p_ref).distance()
}
```

## References with Methods

Methods can take `self` by reference:

```ferret
type Counter struct {
    .value: i32,
};

fn (c: &Counter) get_value() -> i32 {
    c.value++; // original value changed
    return c.value;
}

let counter := { .value: 0 } as Counter;
counter.increment();        // Method receiver automatically uses appropriate reference
let value := counter.get_value();  // Method receiver automatically uses appropriate reference
```

## References and Optional Types

References can be optional, allowing functions to return a reference or `none`:

```ferret
type User struct {
    .name: str,
    .age: i32,
};

fn find_user(id: i32) -> &User? {
    // Returns optional reference to User
    if user_exists(id) {
        return some_ref_to_user();  // Returns &User wrapped in optional
    }
    return none;
}

let user_ref := find_user(42);
if user_ref != none {
    print(user_ref.name);  // Automatic dereferencing
}
```

## Common Patterns

### Builder Pattern with References

```ferret
type RequestBuilder struct {
    .url: str,
    .method: str,
    .headers: map[str]str,
};

fn (b: &RequestBuilder) set_url(url: str) -> &RequestBuilder {
    b.url = url;
    return b;
}

fn (b: &RequestBuilder) set_method(method: str) -> &RequestBuilder {
    b.method = method;
    return b;
}

fn (b: &RequestBuilder) build() -> Request {
    return { .url: b.url, .method: b.method } as Request;
}

let builder := { .url: "", .method: "GET", .headers: {} } as RequestBuilder;
let request := builder
    .set_url("https://api.example.com")
    .set_method("POST")
    .build();
```

## Best Practices

### Default to Values

Start with value types and only use references when needed:

```ferret
// Good - simple and safe
fn calculate(x: i32, y: i32) -> i32 {
    return x + y;
}

// Overkill - no benefit for small types
fn calculate(x: &i32, y: &i32) -> i32 {
    return x + y;  // Automatic dereferencing
}
```

### Use References for Large Types

Consider references when passing large structs:

```ferret
type HugeStruct struct {
    .data1: [10000]i32,
    .data2: [10000]f64,
    // ... many more fields
};

// Good - avoids copying 160KB+
fn process(data: &HugeStruct) {
    // ...
}

// Bad - copies 160KB+ on every call
fn process(data: HugeStruct) {
    // ...
}
```

### Don't Over-Reference

Not everything needs to be a reference:

```ferret
// Bad - unnecessary references for primitives
fn add(a: &i32, b: &i32) -> i32 {
    return a + b;
}

// Good - primitives are cheap to copy
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

## Comparison with Other Languages

Ferret's reference types are similar to:

- **Rust**: Similar `&T` and `&mut T` syntax with borrowing rules
- **C++**: Like C++ references (`T&`) but with safety checks
- **C#**: Similar to `ref` keyword but more explicit
- **Go**: More explicit than Go's automatic pointer handling

Unlike pointers in C/C++:
- ✅ No null references (use `&T?` for optional references)
- ✅ No dangling references (checked at compile time)
- ✅ No pointer arithmetic
- ✅ Automatic lifetime checking

## Next Steps

- [Learn about Methods](/type-system/methods) - Methods can use reference receivers
- [Explore Structs](/type-system/structs) - Common place to use references
- [Understand Optional Types](/type-system/optionals) - Combine with references for `&T?`
- [Master Ownership](TODO) - Deep dive into Ferret's ownership model
