---
title: Methods
description: Define behavior for your types with methods in Ferret
---

Methods allow you to define behavior for any named type in Ferret. Unlike some languages where methods are tied only to classes or structs, Ferret lets you attach methods to any type you define—structs, enums, or even type aliases.

## What Are Methods?

A method is a function associated with a specific type. Methods are defined using receiver syntax, where you specify which type the method belongs to.

```ferret
type Counter struct {
    .value: i32,
};

fn (c: Counter) increment() -> Counter {
    return { .value: c.value + 1 } as Counter;
}

fn (c: Counter) get_value() -> i32 {
    return c.value;
}
```

## Method Syntax

Methods use receiver syntax with the type in parentheses before the method name:

```ferret
fn (receiver: Type) method_name(parameters) -> ReturnType {
    // method body
}
```

The receiver is like the first parameter—it's the value the method operates on. Inside the method, you can access the receiver's fields and call other methods.

## Methods on Structs

The most common use case is defining methods on structs:

```ferret
type Rectangle struct {
    .width: f64,
    .height: f64,
};

fn (r: Rectangle) area() -> f64 {
    return r.width * r.height;
}

fn (r: Rectangle) perimeter() -> f64 {
    return 2.0 * (r.width + r.height);
}

fn (r: Rectangle) is_square() -> bool {
    return r.width == r.height;
}

let rect := { .width: 10.0, .height: 5.0 } as Rectangle;
let area := rect.area();          // 50.0
let perimeter := rect.perimeter();  // 30.0
let square := rect.is_square();    // false
```

## Methods on Enums

You can also define methods on enums, which is particularly useful for adding behavior to variants:

```ferret
type Status enum {
    Pending,
    Active,
    Completed,
    Failed,
};

fn (s: Status) is_finished() -> bool {
    match s {
        Status::Completed => return true,
        Status::Failed => return true,
        _ => return false,
    }
}

fn (s: Status) to_string() -> str {
    match s {
        Status::Pending => return "Pending",
        Status::Active => return "Active",
        Status::Completed => return "Completed",
        Status::Failed => return "Failed",
    }
}

let status := Status::Completed;
let finished := status.is_finished();  // true
let text := status.to_string();        // "Completed"
```

## Methods on Type Aliases

Even type aliases can have methods! This is useful for adding domain-specific behavior to primitive types:

```ferret
type UserId i32;

fn (id: UserId) is_valid() -> bool {
    return id > 0;
}

fn (id: UserId) to_string() -> str {
    // internal logic
    return "";
}

let user_id := 42 as UserId;
let valid := user_id.is_valid();      // true
let display := user_id.to_string();   // "User#42"
```

## Methods with Parameters

Methods can take additional parameters beyond the receiver:

```ferret
type Vector2D struct {
    .x: f64,
    .y: f64,
};

fn (v: Vector2D) add(other: Vector2D) -> Vector2D {
    return {
        .x: v.x + other.x,
        .y: v.y + other.y
    } as Vector2D;
}

fn (v: Vector2D) scale(factor: f64) -> Vector2D {
    return {
        .x: v.x * factor,
        .y: v.y * factor
    } as Vector2D;
}

fn (v: Vector2D) distance_to(other: Vector2D) -> f64 {
    let dx := other.x - v.x;
    let dy := other.y - v.y;
    return math::sqrt(dx * dx + dy * dy);
}

let v1 := { .x: 3.0, .y: 4.0 } as Vector2D;
let v2 := { .x: 6.0, .y: 8.0 } as Vector2D;

let sum := v1.add(v2);              // { .x: 9.0, .y: 12.0 }
let scaled := v1.scale(2.0);        // { .x: 6.0, .y: 8.0 }
let distance := v1.distance_to(v2); // 5.0
```

## Methods vs Functions

You might wonder: when should you use a method versus a standalone function?

**Use methods when:**
- The operation is primarily about the type itself
- It makes the code more readable with dot notation
- You're building an API for your type

**Use standalone functions when:**
- The operation involves multiple unrelated types equally
- It's a utility that doesn't belong to any specific type
- You're implementing generic algorithms

```ferret
// Method - operation on a type
fn (p: Point) distance_from_origin() -> f64 {
    return math::sqrt(p.x * p.x + p.y * p.y);
}

// Function - operation on multiple types
fn distance_between(p1: Point, p2: Point) -> f64 {
    let dx := p2.x - p1.x;
    let dy := p2.y - p1.y;
    return math::sqrt(dx * dx + dy * dy);
}

let p := { .x: 3.0, .y: 4.0 } as Point;
p.distance_from_origin();  // Clear: distance of p from origin

let p1 := { .x: 0.0, .y: 0.0 } as Point;
let p2 := { .x: 3.0, .y: 4.0 } as Point;
distance_between(p1, p2);  // Clear: distance between two points
```

## Method Chaining

Methods that return the same type enable method chaining, creating fluent APIs:

```ferret
type StringBuilder struct {
    .content: str,
};

fn (sb: StringBuilder) append(text: str) -> StringBuilder {
    return { .content: sb.content + text } as StringBuilder;
}

fn (sb: StringBuilder) append_line(text: str) -> StringBuilder {
    return { .content: sb.content + text + "\n" } as StringBuilder;
}

fn (sb: StringBuilder) to_string() -> str {
    return sb.content;
}

let result := { .content: "" } as StringBuilder
    .append("Hello, ")
    .append("World!")
    .append_line("")
    .append("From Ferret")
    .to_string();
```

## Methods and Optional Types

Methods work seamlessly with optional types. When you have an optional value, you can use the coalescing operator to provide a default:

```ferret
type User struct {
    .name: str,
    .email: str,
};

fn (u: User) get_display_name() -> str {
    return u.name + " <" + u.email + ">";
}

let maybe_user: User? = get_user();
let display := maybe_user?.get_display_name() ?? "Guest";
```

## Methods with Maps

Since map indexing returns optional values, methods on the value type combine nicely:

```ferret
type Config struct {
    .timeout: i32,
    .retries: i32,
};

fn (c: Config) is_aggressive() -> bool {
    return c.timeout < 1000 && c.retries > 5;
}

let configs := {
    "production" => { .timeout: 5000, .retries: 3 },
    "development" => { .timeout: 500, .retries: 10 }
} as map[str]Config;

// Get config and call method with default
let prod_config := configs["production"] ?? { .timeout: 3000, .retries: 2 } as Config;
let aggressive := prod_config.is_aggressive();  // false
```

## Implementing Interfaces with Methods

Methods are how you implement interfaces. When you define methods that match an interface's signatures, your type automatically implements that interface:

```ferret
type Drawable interface {
    draw();
    get_bounds() -> struct {
        .x_min: i32,
        .y_min: i32,
        .x_max: i32,
        .y_max: i32
    };
};

type Circle struct {
    .x: i32,
    .y: i32,
    .radius: i32,
};

// Implementing Drawable for Circle
fn (c: Circle) draw() {
    print("Drawing circle at (" + c.x + ", " + c.y + ")");
}

fn (c: Circle) get_bounds() -> struct {
    .x_min: i32,
    .y_min: i32,
    .x_max: i32,
    .y_max: i32
} {
    return {
        .x_min: c.x - c.radius,
        .y_min: c.y - c.radius,
        .x_max: c.x + c.radius,
        .y_max: c.y + c.radius
    };
}

// Circle now implements Drawable!
let circle := { .x: 10, .y: 20, .radius: 5 } as Circle;
circle.draw();
```

## Best Practices

### Keep Methods Focused

Each method should do one thing well:

```ferret
// Good - focused methods
fn (u: User) get_full_name() -> str { ... }
fn (u: User) is_admin() -> bool { ... }
fn (u: User) send_email(message: str) { ... }

// Not ideal - method doing too much
fn (u: User) check_permissions_and_send_notification() { ... }
```

### Use Clear Naming

Method names should clearly describe what they do:

```ferret
// Good
fn (order: Order) calculate_total() -> f64 { ... }
fn (order: Order) is_paid() -> bool { ... }
fn (order: Order) apply_discount(percent: f64) -> Order { ... }

// Less clear
fn (order: Order) total() -> f64 { ... }
fn (order: Order) paid() -> bool { ... }
fn (order: Order) discount(percent: f64) -> Order { ... }
```

### Consider Immutability

Since Ferret encourages immutability, methods that "modify" data typically return a new instance:

```ferret
type Counter struct {
    .value: i32,
};

// Returns new Counter instead of modifying in place
fn (c: Counter) increment() -> Counter {
    return { .value: c.value + 1 } as Counter;
}

fn (c: Counter) add(amount: i32) -> Counter {
    return { .value: c.value + amount } as Counter;
}

let counter := { .value: 0 } as Counter;
let counter2 := counter.increment();      // counter is still 0, counter2 is 1
let counter3 := counter2.add(5);          // counter3 is 6
```

## Next Steps

Now that you understand methods, explore how they work with other features:

- [Learn about Interfaces](/type-system/interfaces) - Define contracts with methods
- [Explore Structs](/type-system/structs) - The most common place to use methods
- [Understand Enums](/type-system/enums) - Add behavior to enum variants
- [Master Generics](/advanced/generics) - Write generic methods for any type
