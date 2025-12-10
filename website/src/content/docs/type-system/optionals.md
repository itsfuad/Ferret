---
title: Optional Types
description: Working with optional types and handling none values
---

# Optional Types

Optional types are one of Ferret's key safety features, helping you avoid null pointer exceptions.

## What are Optional Types?

An optional type `T?` can hold either a value of type `T` or `none`. This makes the possibility of missing values explicit in your type system.

```ferret
let someNumber: i32? = 42;      // Has a value
let noNumber: i32? = none;      // No value
```

## Type Narrowing

Ferret uses **flow-sensitive typing** to automatically narrow optional types in conditionals:

```ferret
let x: i32? = 10;

if x != none {
    // Inside this block, x is narrowed to i32 (not i32?)
    let doubled: i32 = x * 2;  // OK
} else {
    // Inside this block, x is narrowed to none
    let value: i32? = x;  // OK - can assign none to optional
    let num: i32 = x;     // ERROR - cannot assign none to i32
}
```

### With Equality Checks

```ferret
let opt: str? = "hello";

if opt == none {
    // opt is none here
    print("No value");
} else {
    // opt is str here (not str?)
    let length: i32 = opt.length;
}
```

## Coalescing Operator

The coalescing operator `??` provides a default value when an optional is `none`:

```ferret
let maybeValue: i32? = none;
let value: i32 = maybeValue ?? 0;  // value is 0

let someValue: i32? = 42;
let result: i32 = someValue ?? 0;  // result is 42
```

### Chaining Coalescing Operators

```ferret
let a: i32? = none;
let b: i32? = none;
let c: i32? = 42;

let result: i32 = a ?? b ?? c ?? 0;  // result is 42
```

## Assignment Rules

### Wrapping (T → T?)
You can assign a value to an optional type (automatic wrapping):

```ferret
let num: i32 = 42;
let optNum: i32? = num;  // OK - wrapped automatically
```

### Unwrapping (T? → T)
You cannot directly assign an optional to a non-optional:

```ferret
let optNum: i32? = 42;
let num: i32 = optNum;  // ERROR - must unwrap first
```

Instead, use type narrowing or the coalescing operator:

```ferret
// Option 1: Type narrowing
if optNum != none {
    let num: i32 = optNum;  // OK
}

// Option 2: Coalescing operator
let num: i32 = optNum ?? 0;  // OK
```

### None Assignment
`none` can only be assigned to optional types:

```ferret
let opt: i32? = none;  // OK
let num: i32 = none;   // ERROR
```

## Functions with Optional Return Types

```ferret
fn findUser(id: i32) -> User? {
    if userExists(id) {
        return getUser(id);
    }
    return none;
}

// Using the result
let maybeUser: User? = findUser(42);

if maybeUser != none {
    print(maybeUser.name);
} else {
    print("User not found");
}
```

## Best Practices

### Do
- Use optional types for values that might be absent
- Use type narrowing to safely access optional values
- Use coalescing operator for simple default values

### Don't
- Don't use optional types unnecessarily
- Don't try to use optional values without checking for none first
- Don't chain too many coalescing operators (readability)

## Examples

### Safe Division

```ferret
fn safeDivide(a: i32, b: i32) -> i32? {
    if b == 0 {
        return none;
    }
    return a / b;
}

let result: i32? = safeDivide(10, 2);
let value: i32 = result ?? 0;  // value is 5
```

### Working with Maps

Optionals and maps work together beautifully since map access always returns optional values:

```ferret
let scores := {
    "alice" => 95,
    "bob" => 87
} as map[str]i32;

// Map access returns i32?
let alice_score: i32? = scores["alice"];  // Some(95)
let carol_score: i32? = scores["carol"];  // none

// Use coalescing for defaults
let score1 := scores["alice"] ?? 0;  // 95
let score2 := scores["carol"] ?? 0;  // 0

// Chain lookups with multiple fallbacks
let primary := scores["primary"] ?? scores["backup"] ?? 0;
```

See the [Maps](/type-system/maps) section for more details on how maps use optionals for safety.

### Configuration with Defaults

```ferret
type Config struct {
    .port: i32?,
    .host: str?
};

let config := {
    .port: none,
    .host: "localhost"
} as Config;

let actualPort: i32 = config.port ?? 8080;
let actualHost: str = config.host ?? "0.0.0.0";
```

## Next Steps

- [Learn about Maps](/type-system/maps) - See optionals in action with collections
- [Explore error handling](/advanced/errors) - Handle errors safely
- [Understand Structs](/type-system/structs) - Create custom types


