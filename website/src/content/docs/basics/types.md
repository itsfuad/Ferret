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

When you write a number with a decimal point without specifying a type, Ferret automatically uses the default `f32` because it gives you better precision. But if the value requires more precision, it will promote it to `f64` and so on.

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

Strings are indexable. Indexing returns a `byte` (not a `str`), is byte-based (not Unicode code points), and uses runtime bounds checks. Negative indices count from the end (`-1` is last byte):

```ferret
let s: str = "Hello";
let first: byte = s[0];
let last: byte = s[-1]; // last byte
let n: i32 = len(s);
```

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

When printing, `byte` displays as a character while `u8` prints as a number.

## Compound Types

Compound types are built by combining other types together. They let you group related data.

### Arrays

Arrays are collections that store multiple values of the same type in a specific order. Think of them as numbered containers where each slot holds one value.

There are two kinds of arrays in Ferret:

**Dynamic arrays** automatically grow when you access or assign beyond their current size:

```ferret
let numbers: []i32 = [1, 2, 3, 4, 5];
let names: []str = ["Alice", "Bob", "Charlie"];
let scores := [95, 87, 92];  // Inferred as []i32

// Dynamic arrays grow automatically
let arr := [1, 2, 4];  // size 3
arr[5] = 43;           // grows to [1, 2, 4, 0, 0, 43]
```

Notice the `[]` before the type - this means "an array of" that type. Dynamic arrays have **no bounds checking** - they grow to accommodate any index you use.

**Fixed-size arrays** have a set number of elements that cannot change:

```ferret
let days: [7]str = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];
let coordinates: [3]f64 = [1.0, 2.5, 3.7];
```

The number in brackets `[7]` tells you exactly how many items the array holds. Fixed-size arrays have **compile-time bounds checking** for constant indices:

```ferret
let arr: [5]i32 = [1, 2, 3, 4, 5];
let x := arr[2];   // OK - index 2 is valid
let y := arr[10];  // Compile error: constant index 10 is out of bounds!

// Runtime indices are safe - return last item if out of bounds
let i := 10;
let z := arr[i];   // No error - returns arr[4] (last item)
```

#### Negative Indexing

Both array types support **negative indices** to access elements from the end:

```ferret
let numbers: [5]i32 = [10, 20, 30, 40, 50];
let last := numbers[-1];        // 50 (last element)
let second_last := numbers[-2]; // 40 (second to last)
```

Negative indices count backwards: `-1` is the last element, `-2` is second to last, and so on. For fixed-size arrays, out-of-bounds negative **constant** indices are caught at compile-time:

```ferret
let arr: [5]i32 = [1, 2, 3, 4, 5];
let valid := arr[-5];   // OK - first element
let invalid := arr[-6]; // Compile error: constant index -6 is out of bounds!

// Runtime negative indices safely clamp to bounds
let i := -10;
let safe := arr[i];     // No error - returns arr[0] (first item)
```

#### Array Safety Summary

| Array Type | Constant Indices | Runtime Indices | Negative Indexing |
|------------|------------------|-----------------|-------------------|
| Fixed-size `[N]T` | ✅ Compile error if out of bounds | Returns last/first item (safe clamp) | ✅ Same behavior |
| Dynamic `[]T` | No checking (auto-grows) | Auto-grows to fit | ✅ No checking |

This approach gives you both safety and flexibility: fixed-size arrays catch constant index errors at compile-time and safely clamp runtime indices, while dynamic arrays automatically grow to accommodate any index.

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

### Maps

Maps are collections that store key-value pairs. Think of them like a dictionary where you look up values using keys instead of positions.

Unlike arrays which use numbers (0, 1, 2...) to access elements, maps let you use any type as a key - strings, numbers, or even custom types!

```ferret
let scores: map[str]i32 = {
    "alice" => 95,
    "bob" => 87,
    "charlie" => 92
} as map[str]i32;

let prices: map[str]f64 = {
    "apple" => 1.99,
    "banana" => 0.99
} as map[str]f64;
```

The syntax `map[KeyType]ValueType` tells Ferret what types the keys and values should be.

#### Accessing Map Values

Here's something really important: when you access a map value, you get an **optional type** back. Why? Because the key might not exist in the map!

```ferret
let ages := {"alice" => 25, "bob" => 30} as map[str]i32;

// Returns i32? (optional i32) - key might not exist!
let alice_age: i32? = ages["alice"];  // Some(25)
let missing: i32? = ages["unknown"];  // none
```

This is a safety feature! It forces you to think about what happens when a key doesn't exist, preventing crashes.

#### The Coalescing Operator with Maps

The coalescing operator `??` is perfect for providing default values:

```ferret
let scores := {"alice" => 95} as map[str]i32;

let alice_score := scores["alice"] ?? 0;    // 95
let bob_score := scores["bob"] ?? 0;        // 0 (key doesn't exist)
```

This pattern is so common you'll use it all the time when working with maps!

**Learn more:** Maps are covered in detail in the [Type System section](/type-system/maps).

## Reference Types

:::caution[Experimental Feature]
Reference types are currently being implemented. This is a preview of planned functionality.
:::

Reference types let you pass data by reference rather than by copy. Add `&` before a type to make it a reference:

```ferret
type LargeData struct {
    .buffer: [1000]i32,
    .metadata: str,
};

// Passes by copy (copies entire struct)
fn process_copy(data: LargeData) { }

// Passes by reference (only copies pointer)
fn process_ref(data: &LargeData) { }
```

References are useful for:
- Avoiding expensive copies of large data
- Sharing data between functions

**Learn more:** References are covered in detail in the [Type System section](/type-system/references).

## Custom Types

Ferret lets you define your own types beyond the built-in ones. The most common custom types are structs, enums, and interfaces.

### Defining Custom Types

You use the `type` keyword to define new structured types:

```ferret
// Define a struct type
type Point struct {
    .x: f64,
    .y: f64
};

// Define an enum type
type Color enum {
    Red,
    Green,
    Blue
};

let point: Point = { .x: 10.0, .y: 20.0 } as Point;
let color: Color = Color::Red;
```

These custom types make your code more organized and type-safe. We'll dive deeper into structs, enums, and interfaces in the Type System section.

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
* **Compound types**: Arrays and maps that hold multiple values
* **Optional types**: Types that can be a value or `none`
* **Reference types**: Pass data by reference with `&T` (experimental)
* **Type inference**: Letting Ferret figure out types automatically
* **Type conversion**: Explicitly changing between types

## Next Steps

Now that you know about types, you're ready to learn what you can do with them:

* [Learn about Operators](/operators)  -  Do math, compare values, and more
* [Explore Optional Types in depth](/optionals)  -  Master safe handling of missing values
* [Understand Structs](/structs)  -  Create your own custom types
