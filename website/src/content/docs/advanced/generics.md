---
title: Generics
description: Generic types and functions in Ferret

sidebar:
  badge:
    text: Upcomming
    variant: tip

banner:
  content: |
    This feature is coming soon! Stay tuned for updates.
---

We use fixed types in our programs, but sometimes we want to write code that can operate on different types without duplicating it. Generics allow you to write flexible, reusable code that works with multiple types.

## Generic Functions

To define a generic function, we use angle brackets `<T>` to specify type parameters.

```ferret
fn identity<T>(value: T) -> T {
    return value;
}

let num := identity(42);         // T = i32
let text := identity("hello");   // T = str
```

## Generic Structs

```ferret
type Box<T> struct {
    .value: T,
};

let int_box := Box<i32>{.value: 42};
let str_box := Box<str>{.value: "hello"};
```

## Generic Interfaces

```ferret
type Container<T> interface {
    get() -> T;
    set(value: T);
};
```

## Multiple Type Parameters

```ferret
type Pair<K, V> struct {
    .key: K,
    .value: V,
};

let entry := Pair<str, i32>{.key: "age", .value: 30};
```

## Generic Methods

```ferret
type Box<T> struct {
    .value: T,
};

fn (b: Box<T>) get() -> T {
    return b.value;
}
```