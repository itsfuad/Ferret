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

```ferret
let person := Person{
    .name = "Alice",
    .age = 30,
    .email = "alice@example.com",
};
```

## Accessing Fields

```ferret
let name := person.name;   // Alice
let age := person.age;     // 30
```

## Methods

Define methods on structs using receiver syntax:

```ferret
type Person struct {
    .name: str,
    .age: i32,
};

fn (p: Person) greet() -> str {
    return "Hello, I'm " + p.name;
}

fn (p: Person) is_adult() -> bool {
    return p.age >= 18;
}

let person := Person{.name = "Bob", .age = 25};
let greeting := person.greet();
```

## Nested Structs

```ferret
type Address struct {
    .street: str,
    .city: str,
};

type Person struct {
    .name: str,
    .address: Address,
};

let person := Person{
    .name = "Carol",
    .address = Address{
        .street = "123 Main St",
        .city = "Springfield",
    },
};
```

## Annonymous Structs

You can just use structs without defining a type:

```ferret
let point : struct{ .x: i32, .y: i32}; // Define an anonymous struct type
```

You can use `.{}` to create instances of anonymous structs:

```ferret 
let point := .{ .x = 10, .y = 20 };
```

## Next Steps

- [Learn about Enums](/enums)
- [Explore Interfaces](/interfaces)
