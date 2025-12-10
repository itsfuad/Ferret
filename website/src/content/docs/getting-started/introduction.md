---
title: Introduction to Ferret
description: Learn about Ferret, a modern type-safe programming language
editUrl: true
lastUpdated: true
---


Ferret is a modern, type-safe programming language designed for clarity, safety, and developer productivity.

### Type Safety
Ferret's powerful type system catches errors at compile-time, not runtime. With optional types, error types, and flow-sensitive type narrowing, you can write code with confidence.

### Modern Syntax
Clean, expressive syntax that's easy to learn and pleasant to write. Inspired by the best features of modern languages.

### Error Handling
First-class error handling with result types (`E ! T`) that make dealing with failures explicit and safe.

### Optional Types
No more null pointer exceptions! Optional types (`T?`) and the coalescing operator (`??`) make handling missing values a breeze.

## Quick Example

```ferret
// Variables with type inference
// Functions
fn greet(name: str) -> str {
    return "Hello, " + name + "!";
}

fn main() {
    let name: str = "Ferret";
    let version: i32 = 1;

    // Optional types
    let maybeValue: i32? = 42;

    if maybeValue != none {
        // Type narrowing - maybeValue is i32 here
        let doubled: i32 = maybeValue * 2;
    }

    // Coalescing operator for default values
    let value: i32 = maybeValue ?? 0;

    greet(name);
}

```