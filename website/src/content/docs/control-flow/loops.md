---
title: Loops
description: Iteration with for and while loops in Ferret
---

Ferret provides several loop constructs for iteration.

## For Loops

### Range-based For Loops

Ferret provides two range operators for loops:

- `..` - **Exclusive end**: loops from start to end-1
- `..=` - **Inclusive end**: loops from start to end

```ferret
// Exclusive: iterates 0 to 9 (10 iterations)
for let i in 0..10 {
    print(i);  // Prints: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
}

// Inclusive: iterates 0 to 10 (11 iterations)
for let i in 0..=10 {
    print(i);  // Prints: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
}
```

The `let` keyword declares a new loop variable `i` that's scoped to the loop body.

**Using existing variables:**
```ferret
let i := 0;  // Pre-declared variable
for i in 0..5 {
    print(i);  // Uses the existing 'i' variable
}
// i is now 4 (last value from the loop with exclusive end)

let j := 0;
for j in 0..=5 {
    print(j);  // Uses the existing 'j' variable
}
// j is now 5 (last value from the loop with inclusive end)
```

Without `let`, the loop uses an existing variable. This is useful when you need to access the loop variable after the loop ends.

The range operators generate arrays, so this is equivalent to:
```ferret
let numbers := 0..10;   // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
let numbers_inc := 0..=10;  // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

for let i in numbers {
    print(i);
}
```

You can specify the step/increment with both operators:
```ferret
// Exclusive with step
for let i in 0..10:2 {
    print(i);  // Prints: 0, 2, 4, 6, 8
}

// Inclusive with step
for let i in 0..=10:2 {
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
