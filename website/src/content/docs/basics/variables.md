---

title: "Variables & Constants"
description: "Learn about variables and constants in Ferret"
---

Variables and constants let you store information in your program. If you're just getting started, you can think of them as labeled containers:

* A **variable** is a container you can change.
* A **constant** is a container you fill once and never change again.

Since Ferret can automatically figure out the type of a value, you don't need to introduce types yet. You only need to understand how to declare names and give them values.

Ferret provides two keywords for this: `let` for variables and `const` for constants.

## Variable Declaration

Use `let` to create a variable. A variable can change later. We use `:=` to declare with a value. You will learn about the `:=` (walrus) operator and why we did not use `=` later. Just remember to use `:=` when you are declaring a variable or constant for the first time with a value.

### Basic Variable Examples

```ferret
let name := "Ferret";
let age := 1;
let score := 10;
```

### Reassigning Variables

Variables can be updated using the `=` operator. It's called the assignment operator.

```ferret
let points := 0;
points = 15;
points = 30;
```

## Constants

Constants use `const`. Once a constant gets a value, it cannot change.

```ferret
const PI := 3.14159;
const MAX_RETRIES := 3;
const APP_NAME := "Ferret Compiler";
```

### Attempting to Reassign

```ferret
const VERSION := 1;
VERSION = 2;  // ERROR: Cannot assign to constant
```

## Naming Conventions

To keep code readable:

* Use `snake_case` for variables: `user_name`, `total_count`
* Use `SCREAMING_SNAKE_CASE` for constants: `MAX_VALUE`, `DEFAULT_PORT`

```ferret
let user_name := "Alice";
let total_count := 42;

const DEFAULT_PORT := 8080;
```

## Next Steps

* [Learn about Data Types](/types)
* [Explore Operators](/operators)
