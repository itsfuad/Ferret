---
title: Functions
description: Defining and calling functions in Ferret
---

Functions are reusable blocks of code that perform specific tasks.

## Function Declaration

To declare a function, use the `fn` keyword followed by the function name. Let's define a function that greets a user:

```ferret
fn greet() {
    print("Hello!");
}
```

## Function Parameters

Functions can take inputs from outside which are called parameters. Parameters are just like variables that are defined in the function signature.

```ferret
fn greet(name: str) {
    print("Hello, " + name);
}
```

## Calling Functions

```ferret
let message := greet("World");
print(message);  // Hello, World
```

## Return Types

Functions can return values:

```ferret
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

let sum := add(5, 3);  // 8
```

## Void Functions

Functions that don't return a value:

```ferret
fn log_message(msg: str) {
    print("[INFO] " + msg);
}
```

## Unnamed or Anonymous Functions

Functions are values in ferret and can be assigned to variables or passed as arguments. We can define unnamed functions using the `fn` keyword without a name:

```ferret
let square = fn(x: i32) -> i32 {
    return x * x;
};

let result = square(5);  // 25
```
This allows for greater flexibility in how functions are used and composed.

## Next Steps

- [Learn about Parameters](/parameters)
- [Explore Error Handling](/advanced/errors)
