---
title: When Expressions
description: Pattern matching in Ferret
---

When expressions provide powerful pattern matching capabilities.

## Basic When

```ferret
let status := 200;

when status {
    200 => print("OK"),
    404 => print("Not Found"),
    500 => print("Server Error"),
    _ => print("Unknown Status"),
}
```

## When with Values

When expressions return values:

```ferret
let message := when status {
    200 => "Success",
    404 => "Not Found",
    _ => "Error",
};
```

## Pattern Matching with Enums

```ferret
type Status enum {
    Pending,
    Active,
    Done,
};

let status := Status::Active;

when status {
    Status::Pending => print("Waiting"),
    Status::Active => print("In Progress"),
    Status::Done => print("Complete"),
}
```

## Next Steps

- [Learn about Functions](/functions)
- [Explore Enums](/enums)
