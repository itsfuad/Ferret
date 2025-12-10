---
title: Maps
description: Working with key-value collections in Ferret
---

# Maps

Maps are powerful collections that store key-value pairs, like a real-world dictionary where you look up definitions (values) using words (keys). They're perfect when you need to associate related data together!

## What are Maps?

A map is a collection where each **key** maps to exactly one **value**. Think of it like:
- A phone book: names (keys) → phone numbers (values)
- A scoreboard: player names (keys) → scores (values)  
- A product catalog: product IDs (keys) → prices (values)

## Creating Maps

### Map Type Syntax

Maps use the syntax `map[KeyType]ValueType`:

```ferret
// Map from strings to integers
let ages: map[str]i32;

// Map from integers to strings
let id_to_name: map[i32]str;

// Map from strings to floats
let prices: map[str]f64;
```

The key type comes first in brackets `[KeyType]`, followed by the value type.

### Map Literals

Create maps using curly braces with the `=>` arrow operator:

```ferret
let scores := {
    "alice" => 95,
    "bob" => 87,
    "charlie" => 92
}; // types are automatically inferred as map[str]i32

let prices := {
    "apple" => 1.99,
    "banana" => 0.99,
    "orange" => 1.49
} as map[str]f64; // explicit type annotation
```

The `=> ` operator clearly shows "this key maps to this value."

:::tip
The `as map[K]V` cast at the end is required to specify the exact key and value types. This helps Ferret catch type errors early!
:::

### Empty Maps

Create an empty map by casting an empty literal:

```ferret
let empty_scores := {} as map[str]i32;
let user_data := {} as map[i32]str;
```

## Accessing Values

Here's the most important thing about maps in Ferret: **accessing a value always returns an optional type**!

### Why Optional Returns?

When you access a map with a key, that key might not exist. Instead of crashing your program, Ferret returns an optional type (`T?`) to force you to handle the missing case:

```ferret
let ages := {"alice" => 25, "bob" => 30};

// Returns i32? (optional), not i32!
let alice_age: i32? = ages["alice"];    // Some(25)
let unknown_age: i32? = ages["nobody"]; // none
```

This design prevents common bugs! You can't forget to check if a key exists because the type system reminds you.

### The Coalescing Operator Pattern

The most common way to work with map values is using the coalescing operator `??` to provide a default:

```ferret
let scores := {
    "alice" => 95,
    "bob" => 87
};

// Get value or use default
let alice_score := scores["alice"] ?? 0;   // 95
let carol_score := scores["carol"] ?? 0;   // 0 (key doesn't exist)
```

This pattern is so useful because:
- It's concise - just one line
- It's safe - no crashes
- It's clear - the default value is right there

### Checking Before Using

You can also check if a value exists before using it:

```ferret
let user_emails := {
    "alice" => "alice@example.com",
    "bob" => "bob@example.com"
};

let email: str? = user_emails["alice"];

if email != none {
    // Inside this block, email is str (not str?)
    send_email(email);
} else {
    print("No email found");
}
```

When you check `!= none`, Ferret knows the value exists and automatically narrows the type!

## Working with Different Key Types

### String Keys

String keys are super common for configuration, user data, etc:

```ferret
let config := {
    "api_url" => "https://api.example.com",
    "timeout" => "30s",
    "retry_count" => "3"
};

let api_url := config["api_url"] ?? "https://default.com";
```

### Numeric Keys

Perfect for lookup tables and ID mappings:

```ferret
let id_to_name := {
    1 => "Alice",
    2 => "Bob",
    3 => "Charlie"
};

let name := id_to_name[1] ?? "Unknown";
```

### Mixed Type Values

Values can be different types than keys:

```ferret
let user_scores := {
    "alice" => 95,
    "bob" => 87
} as map[str]i32;

let product_prices := {
    "laptop" => 999.99,
    "mouse" => 24.99
} as map[str]f64;
```

## Real-World Examples

### User Database

```ferret
fn main() {
    // Store user ages
    let user_ages : map[str]i32 = {
        "alice" => 25,
        "bob" => 30,
        "charlie" => 35
    };

    // Safe access with defaults
    let alice_age := user_ages["alice"] ?? 0;
    let unknown_age := user_ages["nobody"] ?? 18; // Default to 18

    print("Alice is " + alice_age + " years old");
}
```

### Product Inventory

```ferret
fn main() {
    let inventory : map[str]i32 = {
        "apples" => 50,
        "bananas" => 30,
        "oranges" => 40
    };

    // Check stock levels
    let apple_count := inventory["apples"] ?? 0;
    let grape_count := inventory["grapes"] ?? 0;  // Not in stock

    if apple_count > 0 {
        print("We have apples!");
    }
}
```

### Configuration Settings

```ferret
fn main() {
    let settings := {
        "theme" => "dark",
        "font_size" => "14",
        "auto_save" => "true"
    };

    let theme := settings["theme"] ?? "light";
    let font := settings["font_size"] ?? "12";

    print("Theme: " + theme);
}
```

## Type Safety Benefits

### Compile-Time Checks

Ferret validates your map operations at compile time:

```ferret
let scores := {"alice" => 95};

// ✅ Correct: returns i32?
let score: i32? = scores["alice"];

// ✅ Correct: unwrap with coalescing
let value: i32 = scores["alice"] ?? 0;

// ❌ Error: can't assign i32? to i32
let bad: i32 = scores["alice"];
```

### Preventing Null Pointer Crashes

Unlike many languages, Ferret maps **never crash** when accessing missing keys:

```ferret
let data := {"key1" => 100};

// In other languages, this might crash!
// In Ferret, you get 'none' safely
let missing := data["key_does_not_exist"] ?? -1;  // Returns -1
```

This design eliminates an entire class of runtime errors!

## Best Practices

### Always Provide Defaults

When using coalescing operator, always think about what default makes sense:

```ferret
let scores := {"alice" => 95};

// ✅ Good: meaningful default
let score := scores["bob"] ?? 0;

// ✅ Good: indicates "not found"
let score := scores["bob"] ?? -1;
```

### Use Descriptive Key Types

Make your map types self-documenting:

```ferret
// ✅ Clear what this map represents
let user_scores: map[str]i32;

// ✅ Clear ID mapping
let id_to_email: map[i32]str;
```

### Handle Missing Keys Explicitly

Always consider what happens when a key doesn't exist:

```ferret
let cache := {"page1" => "content"};

// Option 1: Provide default
let content := cache["page1"] ?? "Loading...";

// Option 2: Check explicitly
let maybe_content: str? = cache["page2"];
if maybe_content != none {
    display(maybe_content);
} else {
    load_from_server("page2");
}
```

## Comparison with Other Languages

Understanding how Ferret maps differ from other languages:

| Language | Missing Key Behavior |
|----------|---------------------|
| **Ferret** | Returns `V?` (optional) ✅ |
| **Rust** | `HashMap::get()` returns `Option<&V>` |
| **Swift** | `Dictionary[key]` returns `V?` |
| **Go** | Dual return `value, ok := map[key]` |
| **Python** | Raises `KeyError` exception ❌ |
| **JavaScript** | Returns `undefined` |

Ferret's approach is similar to Rust and Swift - it makes missing values **impossible to ignore** through the type system.

## Common Patterns

### Default Value Pattern

Most common - use coalescing for instant defaults:

```ferret
let value := mymap[key] ?? default_value;
```

### Chaining Lookups

Try multiple keys until one works:

```ferret
let config := {"env" => "prod"};

let env := config["environment"] ?? 
           config["env"] ?? 
           "development";  // Ultimate fallback
```

### Optional Storage

Store the optional for later checking:

```ferret
let user_data := {"id" => 123};

let maybe_id: i32? = user_data["id"];

// Check later
if maybe_id != none {
    process(maybe_id);
}
```

## Summary

Maps in Ferret are:
- **Safe**: Optional returns prevent crashes
- **Explicit**: You must handle missing keys
- **Type-safe**: Keys and values have specific types
- **Ergonomic**: Coalescing operator makes defaults easy

Key takeaways:
- Map syntax: `map[KeyType]ValueType`
- Map literals: `{ key => value } as map[K]V`
- Accessing values returns optionals: `map[key]` → `V?`
- Use coalescing for defaults: `map[key] ?? default`
- No crashes - missing keys return `none`

## What's Next?

Now that you understand maps, explore:
- [Optional Types](/type-system/optionals) - Master optional handling
- [Structs](/type-system/structs) - Create complex data structures
- [Enums](/type-system/enums) - Define sets of possible values

Maps, along with arrays, structs, and enums, form the foundation of Ferret's data modeling capabilities!
