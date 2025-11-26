---
title: Error Handling
description: Working with errors and result types in Ferret
lastUpdated: 2022-08-09
---

Ferret uses explicit error handling with Error Types to manage failures safely.

## Result Types

Functions that can fail return `T ! E` (Error type). Here `T` is the normal return type, and `E` is the error type.

```ferret
fn divide(a: i32, b: i32) -> i32 ! str {
    if b == 0 {
        return "Division by zero"!; // Look at the `!` operator here. It constructs an error value.
    }
    return a / b;
}
```

You can skip the error type if you don't care about the specific error. Ferret will use `str` as the default error type. At least for now (Lol).

```ferret
fn divide(a: i32, b: i32) -> i32 ! {
    if b == 0 {
        return "Division by zero"!;
    }
    return a / b;
}
```

## Handling Errors

When calling a function that returns an error type, you cannot ignore the possibility of failure. For example, this won't compile:

```ferret
let result := divide(10, 2); // You cannot call the function without handling the error
```
So you must handle it using `catch` clause:

```ferret
let result := divide(10, 0) catch e { // e holds the error value
    // Handle error case
    print("Error occurred: " + e);
    return; // Early return
};

// These line won't reach if there was an error
print("Result: " + result); // won't run because the program will return early on error
```
But what if you want to move forward even with an error? You can provide a default value after the `catch` block. This can be either declared in the block or after it as literal value:
```ferret
let result := divide(10, 0) catch e {
    // handle error case and provide default
    print("Error occurred: " + e);
    let default_value := -1;
} default_value; // result will be -1
```
Or more concisely:
```ferret
let result := divide(10, 0) catch e {
    // handle error case and provide default
    print("Error occurred: " + e);
} -1 ; // default value if error occurs, result will be -1
```

## Shorthand
You can just provide the default value directly:

```ferret
let result := divide(10, 0) catch -1; // result will be -1 on error
```