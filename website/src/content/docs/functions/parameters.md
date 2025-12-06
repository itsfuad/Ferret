---
title: Parameters
description: Function parameters in Ferret
---

You can use multiple types of parameters in Ferret functions.

## Basic Parameters

```ferret
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

## Optional Parameters

Parameters can be optional:

```ferret
fn greet(name: str, title: str?) -> str {
    if title != none {
        return "Hello, " + title + " " + name;
    }
    return "Hello, " + name;
}
```

## Variadic Parameters

Accept multiple arguments:

```ferret
fn sum(numbers: ...i32) -> i32 {
    let total := 0;
    for n in numbers {
        total = total + n;
    }
    return total;
}

let result := sum(1, 2, 3, 4, 5);  // 15
```

## Next Steps

- [Learn about Structs](/structs)
- [Explore Methods](/type-system/methods)
