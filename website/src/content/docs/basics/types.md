---

title: "Data Types"
description: "Learn about Ferret's built-in data types"
---

Now that you know how to create variables and constants, it's time to learn what kinds of values they can store. These are called **data types**.

Ferret comes with a set of built‑in types that let you work with numbers, text, true/false values, and more.

## Primitive Types

Primitive types are the simplest kinds of data. Internally they are just numbers. Unlike other languages, Ferret has a rich set of primitive types to give you more control over how data is stored and manipulated. We support maximum 256-bit integers and floating-point numbers with up to 71 decimal digits of precision! And it's built right into the language without needing any special libraries.

### Integer Types

These types store whole numbers.

| Type  | Size   | Range         | Description                 |
| ----- | ------ | ------------- | --------------------------- |
| `i8`  | 8‑bit  | -2⁷ to 2⁷‑1   | Small integer               |
| `i16` | 16‑bit | -2¹⁵ to 2¹⁵‑1 | Medium integer              |
| `i32` | 32‑bit | -2³¹ to 2³¹‑1 | Standard integer            |
| `i64` | 64‑bit | -2⁶³ to 2⁶³‑1 | Bigger integer              |
| `i128` | 128‑bit | -2¹²⁷ to 2¹²⁷‑1 | Very big integer            |
| `i256` | 256‑bit | -2²⁵⁵ to 2²⁵⁵‑1 | Extremely big integer       |
| `u8`  | 8‑bit  | 0 to 2⁸‑1     | Non‑negative small integer  |
| `u16` | 16‑bit | 0 to 2¹⁶‑1   | Non‑negative medium integer |
| `u32` | 32‑bit | 0 to 2³²‑1    | Non‑negative integer        |
| `u64` | 64‑bit | 0 to 2⁶⁴‑1    | Bigger non‑negative integer |
| `u128` | 128‑bit | 0 to 2¹²⁸‑1   | Very big non‑negative integer |
| `u256` | 256‑bit | 0 to 2²⁵⁶‑1   | Extremely big non‑negative integer |

Now if you are confused about the `i` and `u` prefixes, `i` stands for signed integers (can be negative) and `u` stands for unsigned integers (non-negative only). And the numbers `32` and `64` stand for the number of bits used to store the value. Other languages may use different names for these types, but the concepts are the same. So when you see `i32`, think of it as a 32-bit signed integer.

Now remember the `:=` operator you learned about in the Variables & Constants section? It is used for declaring variables and constants with type inference. Type inference means Ferret can automatically figure out the type based on the value you provide. But if you want to explicitly specify the type, you can do so using a colon `:` followed by the type name.

```ferret
let count: i32 = 42;
let small: i8 = -128;
let big_number: i64 = 9223372036854775807;
let positive: u32 = 4294967295;
let very_big: u128 = 340282366920938463463374607431768211455;
let huge: u256 = 115792089237316195423570985008687907853269984665640564039457584007913129639935;
```

### Floating‑Point Types

These types store numbers with decimal points. Think of them as numbers that can have fractional parts.

| Type  | Size   | Precision  | Description                |
| ----- | ------ | ---------- | -------------------------- |
| `f32` | 32‑bit | ~7 digits  | Single precision float     |
| `f64` | 64‑bit | ~15 digits | Double precision (default) |
| `f128` | 128‑bit | ~34 digits | Quadruple precision float  |
| `f256` | 256‑bit | ~71 digits | Octuple precision float    |

The `f` stands for floating-point, and the numbers `32` and `64` represent the bits used to store the value. The bigger the number, the more precise your decimal calculations will be.

When you write a number with a decimal point without specifying a type, Ferret automatically uses `f64` because it gives you better precision.

```ferret
let pi: f32 = 3.14159;
let e: f64 = 2.718281828459045;
let price := 19.99;  // Inferred as f64
let large_value: f128 = 1.2345678901234567890123456789012345;
let precise_value: f256 = 1.2345678901234567890123456789012345678901234567890123456789012345678901234567890;
```

### String Type

Strings store text - anything from single letters to entire paragraphs. In Ferret, strings are represented by the `str` type.

You create strings by wrapping text in double quotes `"`.

```ferret
let name: str = "Ferret";
let greeting: str = "Hello, World!";
let emoji: str = "🦦";  // Strings support Unicode, including emojis!

// Strings can span multiple lines
let multiline: str = "Hello
World";
```

Strings are one of the most common types you'll work with. They're perfect for storing names, messages, file paths, and any other text data.

### Boolean Type

Booleans represent yes/no, on/off, or true/false values. There are only two possible values: `true` and `false`.

The type name is `bool`, and booleans are essential for making decisions in your code.

```ferret
let is_active: bool = true;
let is_complete: bool = false;
let has_permission := true;  // Inferred as bool
```

You'll use booleans constantly when writing conditions, like "if the user is logged in" or "while the game is running."

### Character Type

A character represents a single letter, symbol, or emoji. Unlike strings that can hold multiple characters, the `byte` type holds exactly one character. It is called byte because it typically uses one byte (8 bits) of memory to store the value. Internally `byte` and `u8` are the same.

Characters are created using single quotes `'` instead of double quotes.

```ferret
let letter: byte = 'A';
let newline: byte = '\n';  // Special characters use backslash
```

Think of a `byte` as a single building block, while a `str` (string) is like a sequence of these blocks.

## Compound Types

Compound types are built by combining other types together. They let you group related data.

### Arrays

Arrays are collections that store multiple values of the same type in a specific order. Think of them as numbered containers where each slot holds one value.

There are two kinds of arrays in Ferret:

**Dynamic arrays** can grow or shrink:

```ferret
let numbers: []i32 = [1, 2, 3, 4, 5];
let names: []str = ["Alice", "Bob", "Charlie"];
let scores := [95, 87, 92];  // Inferred as []i32
```

Notice the `[]` before the type - this means "an array of" that type.

**Fixed-size arrays** have a set number of elements:

```ferret
let days: [7]str = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];
let coordinates: [3]f64 = [1.0, 2.5, 3.7];
```

The number in brackets `[7]` tells you exactly how many items the array holds. This can't change after you create it.

## Optional Types

Sometimes you need to represent "I might have a value, or I might not." That's what optional types do.

You make any type optional by adding a question mark `?` after it. An optional type can hold either a real value or `none` (which means "no value").

```ferret
let maybe_number: i32? = 42;      // Has a value
let no_value: str? = none;         // No value
let age: i32? = none;              // Starts with no value
```

Optional types help prevent bugs. Instead of crashing when something is missing, Ferret forces you to check if a value exists before using it.

```ferret
let username: str? = get_username();

if username != none {
    // Safe to use username here
    print("Hello, " + username);
} else {
    print("No username provided");
}
```

This is much safer than many other languages where missing values can cause crashes!

## Custom Types

### Type Aliases

Sometimes you want to give a type a more meaningful name for your specific use case. Type aliases let you create a new name for an existing type.

Think of it like giving someone a nickname - the person is the same, but the name helps clarify their role in a specific context.

```ferret
type UserId = i64;
type Email = str;
type Distance = f64;

let user_id: UserId = 12345;
let email: Email = "user@example.com";
let miles: Distance = 42.5;
```

This makes your code more readable. When you see `UserId`, you immediately know it's an ID for a user, not just any random number. The underlying type is still `i64`, but the name gives it meaning.

## Type Conversion

Sometimes you need to convert a value from one type to another. Ferret requires you to do this explicitly - it won't do it automatically behind your back.

### Casting Between Number Types

Use the `as` keyword to convert between number types:

```ferret
let small: i32 = 42;
let big: i64 = small as i64;    // Convert to bigger integer

let whole: i32 = 100;
let decimal: f64 = whole as f64;  // Convert to floating-point

let pi: f64 = 3.14159;
let rounded: i32 = pi as i32;     // Becomes 3 (decimal part removed)
```

:::caution
Converting from floating-point to integer drops the decimal part - it doesn't round!
:::

### Converting To and From Strings
There is no built-in way to convert between strings and other types yet. This is done via standard library functions which will be covered later.

## Summary

You've learned about Ferret's type system! Here's what we covered:

* **Primitive types**: Integers (`i32`, `i64`, `u32`, `u64`), floats (`f32`, `f64`), strings (`str`), booleans (`bool`), and characters (`byte`)
* **Compound types**: Arrays that hold multiple values
* **Optional types**: Types that can be a value or `none`
* **Type aliases**: Custom names for existing types
* **Type inference**: Letting Ferret figure out types automatically
* **Type conversion**: Explicitly changing between types

## Next Steps

Now that you know about types, you're ready to learn what you can do with them:

* [Learn about Operators](/operators)  -  Do math, compare values, and more
* [Explore Optional Types in depth](/optionals)  -  Master safe handling of missing values
* [Understand Structs](/structs)  -  Create your own custom types
