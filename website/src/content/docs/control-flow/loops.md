---
title: Loops
description: Iteration with for and while loops in Ferret
---

Ferret provides several loop constructs for iteration.

## For Loops

```ferret
for i in 0..10 {
    print(i);
}
```
This loop iterates from 0 to 9, printing each number.

You can specify the step size:
```ferret
for i in 0..10:2 {
    print(i);
}
```
This will print 0, 2, 4, 6, 8.

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
