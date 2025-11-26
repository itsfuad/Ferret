---
title: Comments
description: Learn how to write comments in Ferret
---

Comments are notes you write in your code for yourself and other programmers. They explain **why** you wrote the code a certain way, not just **what** the code does.

The Ferret compiler completely ignores comments - they're purely for humans!

## Why Use Comments?

Comments help you:
* Explain tricky or complex logic
* Document why you made certain decisions
* Leave notes for yourself or your team
* Temporarily disable code while testing

## Single-Line Comments

Use `//` for single-line comments. Everything after `//` on that line is ignored:

```ferret
// This entire line is a comment
let x := 42;  // You can also add comments at the end of lines

let y := 10;  // Calculate the base value
let z := y * 2;  // Double it for the final result
```

Single-line comments are perfect for quick notes and short explanations.

## Multi-Line Comments

For longer explanations, use `/* */` to create multi-line comments. Everything between `/*` and `*/` is ignored, even if it spans many lines:

```ferret
/* 
This is a multi-line comment.
You can write as much as you want here.
It's useful for longer explanations or temporarily
disabling multiple lines of code.
*/

let y := 10;

/* 
Calculate the user's discount based on their loyalty level:
- Bronze: 5% off
- Silver: 10% off  
- Gold: 15% off
- Platinum: 20% off
*/
let discount := calculate_discount(user.loyalty_tier);
```

## Documentation Comments

Use `///` for documentation comments. These special comments can be extracted by documentation tools to create automatic documentation for your code.

Documentation comments typically go right before functions, types, or other important declarations:

```ferret
/// Calculates the sum of two numbers.
/// 
/// This function takes two integers and returns their sum.
/// 
/// # Parameters
/// - `a`: The first number to add
/// - `b`: The second number to add
/// 
/// # Returns
/// The sum of `a` and `b` as an i32
/// 
/// # Example
/// ```ferret
/// let result := add(5, 3);  // Returns 8
/// ```
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

You don't need to write documentation comments for everything - save them for public functions and important types that others (or future you) will need to understand.

## Best Practices

### Do Write Comments That Explain "Why"

Good comments explain your reasoning, not what the code obviously does:

```ferret
// Good: Explains why
// Use exponential backoff to avoid overwhelming the server during retries
let retry_delay := base_delay * (2 ** attempt_count);

// Bad: Just repeats what the code says
// Multiply base_delay by 2 raised to attempt_count
let retry_delay := base_delay * (2 ** attempt_count);
```

### Do Comment Complex Logic

If something took you a while to figure out, it'll confuse others too:

```ferret
// Convert UTC timestamp to local timezone accounting for daylight saving
// Formula: local = utc + offset + (is_dst ? dst_adjust : 0)
let local_time := utc_time + timezone_offset + (is_daylight_saving ? 3600 : 0);
```

### Do Update Comments When You Update Code

Outdated comments are worse than no comments - they mislead people:

```ferret
// Bad: Comment doesn't match the code anymore
// Calculate 10% discount
let discount := price * 0.15;  // This gives 15%, not 10%!

// Good: Comment matches reality
// Calculate 15% discount for premium members
let discount := price * 0.15;
```

### Don't State the Obvious

Avoid comments that just repeat what the code clearly shows:

```ferret
// Bad: Obvious from the code
let x := 5;  // Set x to 5
let total := 0;  // Initialize total to zero

// Good: No comment needed - the code is self-explanatory
let retry_count := 5;
let sum := 0;
```

Use clear variable names instead of comments when possible:

```ferret
// Bad: Needs a comment because the name is unclear
let x := 3;  // Number of retries

// Good: Name is clear, no comment needed
let max_retries := 3;
```

### Don't Leave Commented-Out Code

Delete old code instead of commenting it out. Version control systems (like Git) keep your history:

```ferret
// Bad: Clutters the code
// let old_formula := price * 0.10;
// let old_tax := old_formula * 1.08;
let new_price := price * 0.15 * 1.08;

// Good: Just remove it
let new_price := price * 0.15 * 1.08;
```

### Don't Use Comments as a Substitute for Clear Code

If your code needs extensive comments to be understood, consider rewriting it to be clearer:

```ferret
// Bad: Needs comment because code is confusing
// Calculate final price with discount and tax
let p := (x * 0.85) * 1.08;

// Good: Clear names make the purpose obvious
let discounted_price := original_price * 0.85;
let final_price := discounted_price * 1.08;
```

## Temporary Debugging Comments

While debugging, you might add temporary comments to disable code:

```ferret
// Testing without validation
// validate_input(user_data);
process_data(user_data);
```

**Remember:** Remove these before committing your code!

## Summary

Comments are a powerful tool when used right:
* Use `//` for single-line comments
* Use `/* */` for multi-line comments
* Use `///` for documentation comments
* Explain **why**, not **what**
* Keep comments updated with your code
* Write clear code that needs fewer comments

## Next Steps

Now that you know how to document your code, continue learning:

* [Learn about Variables & Constants](/variables)  -  Store and name values
* [Explore Data Types](/types)  -  Understand what kinds of data you can work with
* [Master Operators](/operators)  -  Perform operations on your data
