---
title: Operators
description: Learn about operators in Ferret
---

Operators are special symbols that tell Ferret to perform specific operations on values. Think of them as the verbs of programming - they let you add, compare, combine, and transform data.

In this guide, you'll learn all the operators Ferret provides and how to use them.

## Arithmetic Operators

These operators perform math operations on numbers - just like a calculator!

| Operator | Name           | Example   | Result | What it does                 |
|----------|----------------|-----------|--------|------------------------------|
| `+`      | Addition       | `5 + 3`   | `8`    | Adds two numbers             |
| `-`      | Subtraction    | `5 - 3`   | `2`    | Subtracts second from first  |
| `*`      | Multiplication | `5 * 3`   | `15`   | Multiplies two numbers       |
| `/`      | Division       | `15 / 3`  | `5`    | Divides first by second      |
| `%`      | Modulo         | `17 % 5`  | `2`    | Gets remainder after division|
| `**`     | Exponentiation | `2 ** 3`  | `8`    | Raises first to power of second |

Let's see them in action:

```ferret
let sum := 10 + 5;          // 15
let difference := 10 - 5;   // 5
let product := 10 * 5;      // 50
let quotient := 10 / 5;     // 2
let remainder := 10 % 3;    // 1 (10 divided by 3 is 3 remainder 1)
let power := 2 ** 4;        // 16 (2 × 2 × 2 × 2)
```

The modulo operator `%` is particularly useful when you need to know if a number is even (remainder 0 when divided by 2) or for wrapping values around (like going from hour 23 to hour 0).

```ferret
let is_even := 10 % 2 == 0;  // true
let is_odd := 11 % 2 == 1;   // true
```

## Comparison Operators

Comparison operators compare two values and return either `true` or `false`. You use these constantly when making decisions in your code.

| Operator | Name                  | Example         | Result  | What it checks               |
|----------|----------------------|-----------------|---------|------------------------------|
| `==`     | Equal to             | `5 == 5`        | `true`  | Are they the same?           |
| `!=`     | Not equal to         | `5 != 3`        | `true`  | Are they different?          |
| `<`      | Less than            | `3 < 5`         | `true`  | Is first smaller?            |
| `>`      | Greater than         | `5 > 3`         | `true`  | Is first bigger?             |
| `<=`     | Less than or equal   | `5 <= 5`        | `true`  | Is first smaller or same?    |
| `>=`     | Greater than or equal| `5 >= 3`        | `true`  | Is first bigger or same?     |

Here are some practical examples:

```ferret
let age := 18;

let is_adult := age >= 18;        // true
let is_teenager := age >= 13 and age < 20;  // true
let can_vote := age >= 18;        // true
let needs_guardian := age < 18;   // false

let score := 85;
let passed := score >= 60;        // true
let perfect := score == 100;      // false
let needs_improvement := score < 70;  // false
```

Remember: Use `==` to check if things are equal, not `=`. The single `=` is for assignment (putting a value into a variable).

## Logical Operators

Logical operators combine or modify boolean values (`true` and `false`). They're essential for building complex conditions.

| Operator | Name        | Example                | Result  | What it does                          |
|----------|-------------|------------------------|---------|---------------------------------------|
| `and`    | Logical AND | `true and false`       | `false` | Both must be true                     |
| `or`     | Logical OR  | `true or false`        | `true`  | At least one must be true             |
| `not`    | Logical NOT | `not true`             | `false` | Flips true to false and vice versa    |

### Understanding `and`

The `and` operator returns `true` only when **both** sides are true:

```ferret
let has_ticket := true;
let has_id := true;

let can_enter := has_ticket and has_id;  // true (both are true)

let has_money := false;
let has_time := true;

let can_go_shopping := has_money and has_time;  // false (one is false)
```

### Understanding `or`

The `or` operator returns `true` when **at least one** side is true:

```ferret
let is_weekend := true;
let is_holiday := false;

let can_relax := is_weekend or is_holiday;  // true (one is true)

let is_raining := false;
let is_snowing := false;

let need_umbrella := is_raining or is_snowing;  // false (both are false)
```

### Understanding `not`

The `not` operator flips the boolean value:

```ferret
let is_logged_in := true;
let is_guest := not is_logged_in;  // false

let has_errors := false;
let is_valid := not has_errors;    // true
```

### Combining Logical Operators

You can combine multiple logical operators to create complex conditions:

```ferret
let age := 25;
let has_license := true;
let has_insurance := true;

let can_drive := age >= 16 and has_license and has_insurance;

let is_student := false;
let is_senior := false;

let gets_discount := is_student or is_senior;
```

### Short-Circuit Evaluation

Ferret evaluates logical expressions from left to right and stops as soon as it knows the answer:

```ferret
let x := 0;

// Safe: if x is 0, the second part never runs
let safe := x != 0 and 10 / x > 1;

// This would crash if x is 0!
// let unsafe := 10 / x > 1 and x != 0;
```

This is called "short-circuit evaluation" - if the first part of `and` is false, Ferret doesn't bother checking the second part because the result will be false anyway.

## Assignment Operators

Assignment operators put values into variables. They can also modify the existing value at the same time.

| Operator | Name                | Example   | Equivalent to | What it does                      |
|----------|---------------------|-----------|---------------|-----------------------------------|
| `=`      | Assignment          | `x = 5`   | -             | Put 5 into x                      |
| `+=`     | Add and assign      | `x += 3`  | `x = x + 3`   | Add 3 to x, store result in x     |
| `-=`     | Subtract and assign | `x -= 3`  | `x = x - 3`   | Subtract 3 from x, store in x     |
| `*=`     | Multiply and assign | `x *= 3`  | `x = x * 3`   | Multiply x by 3, store in x       |
| `/=`     | Divide and assign   | `x /= 3`  | `x = x / 3`   | Divide x by 3, store in x         |
| `%=`     | Modulo and assign   | `x %= 3`  | `x = x % 3`   | Get remainder of x/3, store in x  |

The compound operators (`+=`, `-=`, etc.) are shortcuts. They modify a variable based on its current value:

```ferret
let score := 10;

score += 5;   // score is now 15 (same as: score = score + 5)
score -= 3;   // score is now 12 (same as: score = score - 3)
score *= 2;   // score is now 24 (same as: score = score * 2)
score /= 4;   // score is now 6  (same as: score = score / 4)
score %= 5;   // score is now 1  (same as: score = score % 5)
```

These compound operators make your code shorter and often easier to read:

```ferret
let lives := 3;
lives -= 1;  // Lose a life - cleaner than: lives = lives - 1

let points := 100;
points *= 2;  // Double points - cleaner than: points = points * 2
```

## Special Operators

### Walrus Operator (`:=`)

You've seen this one already! The walrus operator declares a variable or constant and lets Ferret automatically figure out its type.

```ferret
let count := 42;          // Type inferred as i32
let name := "Ferret";     // Type inferred as str
let active := true;       // Type inferred as bool
let price := 19.99;       // Type inferred as f64
```

It's called the "walrus" operator because `:=` looks like a walrus if you tilt your head! 🦭

### Coalescing Operator (`??`)

The coalescing operator gives you a way to provide a default value when dealing with optional types. If the left side is `none`, it uses the right side instead.

```ferret
let maybe_value: i32? = none;
let value := maybe_value ?? 0;  // value is 0 (because maybe_value is none)

let some_value: i32? = 42;
let result := some_value ?? 0;  // result is 42 (because some_value has a value)
```

It's super useful for providing defaults:

```ferret
let username: str? = get_input();
let display_name := username ?? "Guest";  // Show "Guest" if no username

let max_items: i32? = get_config("max");
let limit := max_items ?? 100;  // Default to 100 if not configured
```

This is especially useful with maps, since indexing returns optional values:

```ferret
let scores := { "Alice" => 95, "Bob" => 87 } as map[str]i32;
let alice_score := scores["Alice"] ?? 0;   // 95
let charlie_score := scores["Charlie"] ?? 0;  // 0 (key not found)
```

Learn more about [Optional Types](/optionals) and [Maps](/type-system/maps).

### Range Operators (`..` and `..=`)

Range operators create sequences of numbers. Ferret provides two variants:

- `..` - **Exclusive end**: doesn't include the end value
- `..=` - **Inclusive end**: includes the end value

```ferret
let exclusive := 0..5;   // [0, 1, 2, 3, 4] - stops before 5
let inclusive := 0..=5;  // [0, 1, 2, 3, 4, 5] - includes 5
```

This distinction is important when iterating:

```ferret
// Exclusive: iterates 0 to 4 (5 iterations)
for i in 0..5 {
    print(i);  // Prints: 0, 1, 2, 3, 4
}

// Inclusive: iterates 0 to 5 (6 iterations)
for i in 0..=5 {
    print(i);  // Prints: 0, 1, 2, 3, 4, 5
}
```

You can also specify a step/increment value with both operators:

```ferret
let evens := 0..10:2;     // [0, 2, 4, 6, 8] - exclusive, step by 2
let evens_inc := 0..=10:2; // [0, 2, 4, 6, 8, 10] - inclusive, step by 2
let odds := 1..10:2;      // [1, 3, 5, 7, 9] - exclusive
let odds_inc := 1..=10:2; // [1, 3, 5, 7, 9] - inclusive (10 not divisible by step)
```

**When to use which?**

- Use `..` when you want to exclude the end (like array indices: `0..arr.length`)
- Use `..=` when you want to include the end (like counting days: `1..=7` for a week)

## Member Access Operators

### Dot Operator (`.`)

The dot operator lets you access fields (data) and methods (functions) that belong to a value.

```ferret
struct Point {
    .x: i32,
    .y: i32,
}

let p := Point{ .x: 10, .y: 20 };
let x_value := p.x;  // Access the x field: 10
let y_value := p.y;  // Access the y field: 20
```

You'll use the dot operator constantly when working with strings, arrays, and custom types:

```ferret
let message := "Hello";
let length := message.length;  // Get the length of the string

let numbers := [1, 2, 3, 4, 5];
let first := numbers[0];  // Get the first element
```

## Bitwise Operators

Bitwise operators work with the individual bits (0s and 1s) that make up numbers. These are advanced operators you'll use when doing low-level programming, working with flags, or optimizing performance.

| Operator | Name        | Example           | Result      | What it does               |
|----------|-------------|-------------------|-------------|----------------------------|
| `&`      | Bitwise AND | `5 & 3`           | `1`         | Bits on in both            |
| `\|`     | Bitwise OR  | `5 \| 3`          | `7`         | Bits on in either          |
| `^`      | Bitwise XOR | `5 ^ 3`           | `6`         | Bits on in one, not both   |
| `<<`     | Left shift  | `5 << 1`          | `10`        | Shift bits left (×2)       |
| `>>`     | Right shift | `5 >> 1`          | `2`         | Shift bits right (÷2)      |

Here's a quick example using binary literals (numbers starting with `0b`):

```ferret
let a := 0b1010;  // 10 in binary
let b := 0b1100;  // 12 in binary

let and_result := a & b;   // 0b1000 (8)
let or_result := a | b;    // 0b1110 (14) 
let xor_result := a ^ b;   // 0b0110 (6)
let shifted_left := a << 1;  // 0b10100 (20)
let shifted_right := a >> 1; // 0b101 (5)
```

Don't worry if this seems complex - most of the time you won't need bitwise operators. They're mainly used for:
- Working with hardware or network protocols
- Storing multiple flags efficiently
- Performance-critical code

## Operator Precedence

When you have multiple operators in one expression, Ferret follows specific rules about which one to do first. This is called "operator precedence" or "order of operations" - just like in math class!

Here's the order from highest priority (done first) to lowest (done last):

1. **Member access** - `.`, `?.`
2. **Unary operators** - `not`, `-`, `+`
3. **Exponentiation** - `**`
4. **Multiplication, Division, Modulo** - `*`, `/`, `%`
5. **Addition, Subtraction** - `+`, `-`
6. **Bit shifts** - `<<`, `>>`
7. **Comparison** - `<`, `>`, `<=`, `>=`
8. **Equality** - `==`, `!=`
9. **Bitwise AND** - `&`
10. **Bitwise XOR** - `^`
11. **Bitwise OR** - `|`
12. **Logical AND** - `and`
13. **Logical OR** - `or`
14. **Coalescing operator** - `??`
15. **Assignment** - `=`, `+=`, `-=`, etc.

Let's see this in action:

```ferret
let result := 2 + 3 * 4;      // 14 (multiply first: 3*4=12, then: 2+12=14)
let explicit := (2 + 3) * 4;  // 20 (parentheses first: 2+3=5, then: 5*4=20)

let complex := 10 + 5 * 2 - 3;  // 17 (5*2=10, 10+10=20, 20-3=17)
let clearer := 10 + (5 * 2) - 3; // Same, but easier to read
```

### Pro Tip: Use Parentheses

When in doubt, use parentheses `( )` to make your intention clear. It doesn't hurt and makes your code easier to understand:

```ferret
// Harder to read
let score := points * multiplier + bonus - penalty;

// Easier to read - same result!
let score := (points * multiplier) + bonus - penalty;
```

Your future self (and your teammates) will thank you!

## Summary

You've learned about all the operators in Ferret! Here's what we covered:

* **Arithmetic operators** for math: `+`, `-`, `*`, `/`, `%`, `**`
* **Comparison operators** for checking relationships: `==`, `!=`, `<`, `>`, `<=`, `>=`
* **Logical operators** for combining conditions: `and`, `or`, `not`
* **Assignment operators** for modifying variables: `=`, `+=`, `-=`, etc.
* **Special operators** like `:=` for type inference and `??` for defaults
* **Bitwise operators** for low-level bit manipulation
* **Operator precedence** and when to use parentheses

## Next Steps

Now that you know how to work with values using operators, you're ready to control the flow of your programs:

* [Learn about If Statements](/if-statements)  -  Make decisions in your code
* [Explore Loops](/loops)  -  Repeat actions efficiently
* [Understand Optional Types](/optionals)  -  Master safe handling of missing values