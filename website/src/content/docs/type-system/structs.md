---
title: Structs
description: Defining and using structs in Ferret
---

Structs are custom data types that group related data together.

## Struct Definition

```ferret
type Person struct {
    .name: str,
    .age: i32,
    .email: str,
};
```

## Creating Instances

Create struct instances using composite literal syntax with the `as` cast:

```ferret
let person := {
    .name: "Alice",
    .age: 30,
    .email: "alice@example.com"
} as Person;

// Or with explicit type annotation
let another_person: Person = {
    .name: "Bob",
    .age: 25,
    .email: "bob@example.com"
} as Person;
```

:::tip
Notice we use `:` (colon) in struct literals, not `=` (equals). The `.` prefix indicates a struct field.
:::

## Accessing Fields

```ferret
let name := person.name;   // Alice
let age := person.age;     // 30
```

Structs can also have methods that define behavior. Since methods work with any named type in Ferret, they're covered in detail on a separate page—see [Methods](/type-system/methods).

## Nested Structs

```ferret
type Address struct {
    .street: str,
    .city: str
};

type Person struct {
    .name: str,
    .address: Address
};

let person := {
    .name: "Carol",
    .address: {
        .street: "123 Main St",
        .city: "Springfield"
    } as Address
} as Person;

// Access nested fields
let city := person.address.city;  // "Springfield"
```

## Anonymous Structs

You can create structs without defining a named type. This is useful for temporary data structures:

```ferret
// Anonymous struct with explicit type
let point: struct{ .x: i32, .y: i32 } = {
    .x: 10,
    .y: 20
} as struct{ .x: i32, .y: i32 };

// Inferred anonymous struct (Ferret figures out the type)
let coordinate := {
    .x: 5,
    .y: 15
};  // Type is inferred from the literal
```

Anonymous structs are great for one-off data grouping when you don't need a named type.

## Next Steps

- [Learn about Methods](/type-system/methods) - Add behavior to your types
- [Explore Enums](/type-system/enums) - Define sets of named values
- [Understand Maps](/type-system/maps) - Store key-value pairs
- [Master Interfaces](/type-system/interfaces) - Define behavior contracts
