---
title: Loops
description: Iteration with for and while loops in Ferret
---

Ferret provides several loop constructs for iteration.

## For Loops

### Range-based For Loops

```ferret
// Declare new loop variable with 'let'
for let i in 0..10 {
    print(i);
}
```
This loop iterates from 0 to 10, printing each number. The `let` keyword declares a new loop variable `i`.

**Using existing variables:**
```ferret
let i := 0;  // Pre-declared variable
for i in 0..5 {
    print(i);  // Uses the existing 'i' variable
}
// i is now 5 (last value from the loop)
```

Without `let`, the loop uses an existing variable. This is useful when you need to access the loop variable after the loop ends.

The range operator `..` generates an array, so this is equivalent to:
```ferret
let numbers := 0..10;
for let i in numbers {
    print(i);
}
```

You can specify the step/increment:
```ferret
for let i in 0..10:2 {
    print(i);  // Prints: 0, 2, 4, 6, 8, 10
}
```

### Index and Value Pairs

```ferret
let arr := [10, 20, 30];
for let i, val in arr {
    print(i, val);  // Prints: 0 10, 1 20, 2 30
}
```

**Note:** Loop variables require `let` to declare new variables. You cannot use `const` because loop variables must be mutable.

## While Loops

```ferret
let x := 0;
while x < 5 {
    print(x);
    x = x + 1;
}
```
This loop continues as long as the condition `x < 5` is true.

There is no do-while loop in Ferret; use a while loop with an initial condition instead.
