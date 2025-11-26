---
title: Enums
description: Enumeration types in Ferret
---

Enums (enumerations) define a type with a fixed set of possible values.

## Basic Enum

```ferret
type Status enum {
    Pending,
    Active,
    Completed,
    Cancelled,
};
```

## Using Enums

```ferret
let status := Status::Active;

when status {
    Status::Pending => io::print("Waiting"),
    Status::Active => io::print("Running"),
    Status::Completed => io::print("Done"),
    Status::Cancelled => io::print("Aborted"),
}
```

## Next Steps

- [Learn about Interfaces](/interfaces)
- [Explore Match Expressions](/match)
