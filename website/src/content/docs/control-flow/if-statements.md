---
title: If Statements
description: Learn about conditional logic in Ferret
---

Programs often need to make decisions: "If the user is logged in, show their profile. Otherwise, show the login page." That's where **if statements** come in.

If statements let your code choose different paths based on conditions. They're one of the most fundamental building blocks of programming.

## Basic If Statement

The simplest form checks one condition. If it's true, the code inside the curly braces `{ }` runs. If it's false, the code is skipped:

```ferret
let age := 18;

if age >= 18 {
    print("You are an adult");
}

// Program continues here either way
```

The pattern is:
```ferret
if condition {
    // code that runs when condition is true
}
```

Let's see more examples:

```ferret
let temperature := 75;

if temperature > 80 {
    print("It's hot outside!");
}

let has_permission := true;

if has_permission {
    print("Access granted");
}

let score := 95;

if score >= 90 {
    print("Excellent work!");
}
```

## If-Else: Choosing Between Two Paths

Often you want to do one thing if a condition is true, and something different if it's false. That's what `else` is for:

```ferret
let score := 75;

if score >= 60 {
    print("You passed!");
} else {
    print("You failed. Try again!");
}
```

The pattern is:
```ferret
if condition {
    // runs when condition is true
} else {
    // runs when condition is false
}
```

**Exactly one** of these blocks will run - never both, never neither.

More examples:

```ferret
let is_weekend := true;

if is_weekend {
    print("Time to relax!");
} else {
    print("Back to work");
}

let balance := 50;
let price := 30;

if balance >= price {
    print("Purchase approved");
    balance -= price;
} else {
    print("Insufficient funds");
}
```

## If-Else If-Else: Choosing Between Multiple Paths

When you have more than two possibilities, use `else if` to add extra conditions:

```ferret
let grade := 85;

if grade >= 90 {
    print("A - Excellent!");
} else if grade >= 80 {
    print("B - Good job!");
} else if grade >= 70 {
    print("C - Passing");
} else if grade >= 60 {
    print("D - Needs improvement");
} else {
    print("F - Failed");
}
```

Ferret checks each condition **in order** from top to bottom:
1. First it checks `grade >= 90`
2. If that's false, it checks `grade >= 80`
3. If that's false, it checks `grade >= 70`
4. And so on...
5. If all conditions are false, it runs the `else` block

**Important:** As soon as one condition is true, that block runs and the rest are skipped.

```ferret
let time_of_day := 14;  // 2 PM in 24-hour format

if time_of_day < 12 {
    print("Good morning!");
} else if time_of_day < 17 {
    print("Good afternoon!");
} else if time_of_day < 21 {
    print("Good evening!");
} else {
    print("Good night!");
}
```

You can have as many `else if` blocks as you need. The final `else` is optional - if you leave it out and all conditions are false, nothing happens:

```ferret
let age := 15;

if age >= 18 {
    print("You can vote");
} else if age >= 16 {
    print("You can drive");
}
// If age is less than 16, nothing prints
```

## Type Narrowing with Optionals

Here's where Ferret gets really clever. When you check if an optional value is `none`, Ferret automatically adjusts the type inside each branch.

Remember optional types from the Types lesson? They can be either a value or `none`. Normally, you can't use them directly:

```ferret
let maybe_value: i32? = 42;

// This won't work:
// let doubled := maybe_value * 2;  // ERROR: Can't multiply i32?
```

But after checking for `none`, Ferret knows the type more precisely:

```ferret
let maybe_value: i32? = 42;

if maybe_value != none {
    // Inside here, maybe_value is treated as i32 (not i32?)
    // because Ferret knows it's not none
    let doubled: i32 = maybe_value * 2;  // Works!
    print(doubled);  // Prints: 84
} else {
    // Inside here, maybe_value is known to be none
    print("No value");
}
```

This is called **type narrowing** or **flow-sensitive typing**. Ferret understands your checks and makes the type more specific in each branch.

Another example:

```ferret
let username: str? = get_user_input();

if username != none {
    // username is str here
    print("Hello, " + username + "!");
    let length: i32 = username.length;
} else {
    // username is none here
    print("Please enter a username");
}
```

You can also use `==` to check:

```ferret
let opt_count: i32? = get_count();

if opt_count == none {
    // opt_count is none here
    print("No count available");
} else {
    // opt_count is i32 here
    print("Count: " + opt_count.to_string());
}
```

This feature makes working with optional types safe and convenient. You're forced to handle the `none` case, preventing null pointer errors!


## Nested If Statements

You can put if statements inside other if statements:

```ferret
let age := 25;
let has_license := true;

if age >= 18 {
    if has_license {
        print("You can drive");
    } else {
        print("You need a license");
    }
} else {
    print("You're too young to drive");
}
```

However, it's often clearer to use logical operators instead:

```ferret
let age := 25;
let has_license := true;

if age >= 18 and has_license {
    print("You can drive");
} else if age >= 18 {
    print("You need a license");
} else {
    print("You're too young to drive");
}
```

## Summary

You've learned how to make decisions in your code:

* Use `if` to run code conditionally
* Use `if-else` to choose between two paths
* Use `if-else if-else` to choose between multiple paths
* Ferret narrows optional types automatically in if statements
* You can nest if statements or use logical operators for complex conditions

## Next Steps

Now that you can make decisions, learn how to repeat actions:

* [Learn about Loops](/loops)  -  Repeat code efficiently
* [Explore Match Expressions](/match)  -  Advanced pattern matching
* [Understand Optional Types](/optionals)  -  Master safe value handling
