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

match status {
    Status::Pending => io::Println("Waiting"),
    Status::Active => io::Println("Running"),
    Status::Completed => io::Println("Done"),
    Status::Cancelled => io::Println("Aborted"),
}
```


