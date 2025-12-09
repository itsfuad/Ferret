---
title: Interfaces
description: Define behavior contracts with interfaces in Ferret
---

Interfaces are one of the most powerful features in Ferret. They let you define **what** a type can do without specifying **how** it does it. Think of an interface as a contract: "If you want to be this type, you must be able to do these things."

## What Are Interfaces?

An interface is a collection of method signatures. When a type has methods that match all the signatures in an interface, that type **implements** the interface automatically. You don't need to explicitly declare that a type implements an interface—it happens automatically if the methods match!

This is called **duck typing** or **structural typing**: "If it walks like a duck and quacks like a duck, it's a duck." In Ferret, if a type has the methods an interface requires, it implements that interface.

## Why Use Interfaces?

Interfaces provide several benefits:

1. **Polymorphism**: Write code that works with multiple types as long as they implement the same interface
2. **Abstraction**: Focus on what operations are available, not the specific implementation
3. **Flexibility**: Change implementations without changing the code that uses them
4. **Testability**: Easily create mock implementations for testing

## Defining an Interface

You define an interface using the `interface` keyword, followed by a list of method signatures:

```ferret
type Shape interface {
    area() -> i32,
    move(x: i32, y: i32)
};
```

This interface says: "Any type that implements `Shape` must have:
- An `area()` method that returns an `i32`
- A `move()` method that takes two `i32` parameters and returns nothing (void)"

Notice that:
- Method signatures don't include the receiver (the type itself)
- Methods without a return type are void (return nothing)
- Methods are separated by commas

## Implementing Interfaces

In Ferret, you don't explicitly say "this type implements that interface." Instead, if your type has all the required methods with matching signatures, it automatically implements the interface!

Let's create a `Point` type that implements our `Shape` interface:

```ferret
type Point struct {
    .X: i32,
    .Y: i32
};

// Implement the area method
fn (p: Point) area() -> i32 {
    return 0;  // Points have no area
}

// Implement the move method
fn (p: Point) move(x: i32, y: i32) {
    // Note: This doesn't modify p since it's passed by value
    // In a real implementation, you'd return a new Point
}
```

That's it! `Point` now implements `Shape` because it has both required methods with the correct signatures.

Let's create another type that also implements `Shape`:

```ferret
type Circle struct {
    .center: Point,
    .radius: i32
};

// Implement area for Circle
fn (c: Circle) area() -> i32 {
    return 3 * c.radius * c.radius;  // Approximate: π ≈ 3
}

// Implement move for Circle
fn (c: Circle) move(x: i32, y: i32) {
    // Move the circle's center
    // (Again, this would typically return a new Circle)
}
```

Now both `Point` and `Circle` implement `Shape`, even though they're completely different types!

## Using Interfaces

Once a type implements an interface, you can use it anywhere that interface is expected. This is where interfaces become powerful.

### Interface Variables

You can declare variables with an interface type:

```ferret
let shape1: Shape = { .X: 10, .Y: 20 } as Point;
let shape2: Shape = { .center: { .X: 0, .Y: 0 }, .radius: 5 } as Circle;
```

Here, `shape1` and `shape2` are both of type `Shape`, but they hold different concrete types (`Point` and `Circle`).

### Explicit Casting

You can explicitly cast a value to an interface type using the `as` keyword:

```ferret
let p: Point = { .X: 10, .Y: 20 };
let shape: Shape = p as Shape;  // Explicit cast (though not required)
```

The explicit cast is optional—Ferret will automatically allow the assignment if the type implements the interface. However, using `as` can make your intent clearer.

### Calling Interface Methods

Once you have a variable of interface type, you can call any method defined in that interface:

```ferret
let shape1: Shape = { .X: 10, .Y: 20 } as Point;
let shape2: Shape = { .center: { .X: 0, .Y: 0 }, .radius: 5 } as Circle;

// Call methods through the interface
let area1 := shape1.area();  // Calls Point's area method
let area2 := shape2.area();  // Calls Circle's area method

shape1.move(100, 200);  // Calls Point's move method
shape2.move(300, 400);  // Calls Circle's move method
```

The compiler ensures that whatever concrete type is stored in the interface variable, it has all the required methods.

### Functions Accepting Interfaces

One of the most powerful uses of interfaces is writing functions that work with any type that implements a specific interface:

```ferret
fn print_area(shape: Shape) {
    let area := shape.area();
    io::Println("Area: " + area);
}

let point := { .X: 10, .Y: 20 } as Point;
let circle := { .center: { .X: 0, .Y: 0 }, .radius: 5 } as Circle;

print_area(point);   // Works! Point implements Shape
print_area(circle);  // Works! Circle implements Shape
```

This function doesn't care whether you pass a `Point`, a `Circle`, or any other type—as long as it implements `Shape`, the function will work!

## The Empty Interface

Ferret supports the **empty interface**, written as `interface{}`. This is a special interface with no methods, which means **every type implements it**!

```ferret
type AnyType interface {};

let any1: AnyType = { .X: 10, .Y: 20 } as Point;
let any2: AnyType = { .center: { .X: 0, .Y: 0 }, .radius: 5 } as Circle;
let any3: AnyType = 42;
let any4: AnyType = "hello";
let any5: AnyType = true;
```

Since the empty interface has no requirements, any type can be assigned to it. This is useful when you need to work with values of unknown or mixed types.

:::tip
The empty interface `interface{}` is similar to `any` in TypeScript or `Object` in Java. It represents "any type at all."
:::

## Real-World Example: Drawing Shapes

Let's build a more complete example that demonstrates the power of interfaces:

```ferret
// Define what it means to be "drawable"
type Drawable interface {
    draw(),
    get_bounds() -> struct {
        .x_min: i32,
        .y_min: i32,
        .x_max: i32,
        .y_max: i32
    }
};

// Implement Drawable for Rectangle
type Rectangle struct {
    .x: i32,
    .y: i32,
    .width: i32,
    .height: i32
};

fn (r: Rectangle) draw() {
    io::Println("Drawing rectangle at (" + r.x + ", " + r.y + 
                ") with size " + r.width + "x" + r.height);
}

fn (r: Rectangle) get_bounds() -> struct {
    .x_min: i32,
    .y_min: i32,
    .x_max: i32,
    .y_max: i32
} {
    return {
        .x_min: r.x,
        .y_min: r.y,
        .x_max: r.x + r.width,
        .y_max: r.y + r.height
    };
}

// Implement Drawable for Circle
type Circle struct {
    .center_x: i32,
    .center_y: i32,
    .radius: i32
};

fn (c: Circle) draw() {
    io::Println("Drawing circle at (" + c.center_x + ", " + c.center_y + 
                ") with radius " + c.radius);
}

fn (c: Circle) get_bounds() -> struct {
    .x_min: i32,
    .y_min: i32,
    .x_max: i32,
    .y_max: i32
} {
    return {
        .x_min: c.center_x - c.radius,
        .y_min: c.center_y - c.radius,
        .x_max: c.center_x + c.radius,
        .y_max: c.center_y + c.radius
    };
}

// A function that works with any Drawable
fn render_shape(shape: Drawable) {
    shape.draw();
    let bounds := shape.get_bounds();
    io::Println("Bounds: (" + bounds.x_min + ", " + bounds.y_min + 
                ") to (" + bounds.x_max + ", " + bounds.y_max + ")");
}

// Use it with different types
let rect := { .x: 10, .y: 20, .width: 100, .height: 50 } as Rectangle;
let circle := { .center_x: 50, .center_y: 50, .radius: 25 } as Circle;

render_shape(rect);    // Works with Rectangle
render_shape(circle);  // Works with Circle
```

## Interface Composition

You can create interfaces that combine multiple interfaces or add additional methods. This is called **interface composition**:

```ferret
// Base interfaces
type Readable interface {
    read() -> str
};

type Writable interface {
    write(content: str)
};

// Composed interface
type ReadWrite interface {
    read() -> str,
    write(content: str)
};

// Or you can think of it as extending
type Closable interface {
    read() -> str,
    write(content: str),
    close()
};
```

A type that implements `Closable` must have all three methods: `read()`, `write()`, and `close()`.

## Method Signature Matching

For a type to implement an interface, its methods must **exactly match** the interface's method signatures:

- **Method name** must match exactly
- **Parameter types** must match in order and type
- **Return type** must match (or be void if the interface method is void)
- **Parameter count** must match

```ferret
type Calculator interface {
    add(a: i32, b: i32) -> i32,
    multiply(x: i32, y: i32) -> i32
};

type BasicCalc struct {};

// ✅ Correct implementation
fn (c: BasicCalc) add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn (c: BasicCalc) multiply(x: i32, y: i32) -> i32 {
    return x * y;
}

// ❌ This would NOT implement Calculator:
// - Wrong return type
fn (c: BasicCalc) add(a: i32, b: i32) -> f64 { ... }

// - Wrong parameter count
fn (c: BasicCalc) add(a: i32) -> i32 { ... }

// - Wrong parameter type
fn (c: BasicCalc) add(a: f64, b: f64) -> i32 { ... }
```

## Common Patterns

### The Iterator Pattern

Interfaces are perfect for creating iterators:

```ferret
type Iterator interface {
    next() -> i32?,
    has_next() -> bool
};

type NumberRange struct {
    .current: i32,
    .end: i32
};

fn (r: NumberRange) next() -> i32? {
    if r.current < r.end {
        let value := r.current;
        // In real code, you'd update r.current
        return value;
    }
    return none;
}

fn (r: NumberRange) has_next() -> bool {
    return r.current < r.end;
}
```

### The Strategy Pattern

Use interfaces to implement different algorithms:

```ferret
type SortStrategy interface {
    sort(items: []i32) -> []i32
};

type QuickSort struct {};
type MergeSort struct {};

fn (q: QuickSort) sort(items: []i32) -> []i32 {
    // Quick sort implementation
    return items;
}

fn (m: MergeSort) sort(items: []i32) -> []i32 {
    // Merge sort implementation
    return items;
}

fn sort_data(data: []i32, strategy: SortStrategy) -> []i32 {
    return strategy.sort(data);
}
```

## Best Practices

### Keep Interfaces Small

Interfaces should define the minimum set of methods needed. This makes them easier to implement and more flexible:

```ferret
// ✅ Good - focused interface
type Reader interface {
    read() -> str
};

// ❌ Not ideal - too many responsibilities
type FileOperations interface {
    read() -> str,
    write(content: str),
    delete(),
    rename(new_name: str),
    get_size() -> i64,
    get_permissions() -> str
};
```

### Use Descriptive Names

Interface names should clearly describe what they represent:

```ferret
// ✅ Good names
type Drawable interface { ... }
type Readable interface { ... }
type Closable interface { ... }

// ❌ Less clear
type Thing interface { ... }
type Stuff interface { ... }
type Doer interface { ... }
```

### Prefer Interfaces Over Concrete Types

When writing functions, accept interface types when possible:

```ferret
// ✅ Good - accepts any Shape
fn calculate_total_area(shapes: []Shape) -> i32 {
    let total := 0;
    for shape in shapes {
        total = total + shape.area();
    }
    return total;
}

// ❌ Less flexible - only works with Rectangle
fn calculate_total_area(shapes: []Rectangle) -> i32 {
    // ...
}
```

### Document Interface Contracts

When defining interfaces, consider what behavior is expected:

```ferret
// Interface for types that can be serialized to JSON
type JSONSerializable interface {
    // to_json() should return a valid JSON string representation
    to_json() -> str
};

// Interface for types that can be compared
type Comparable interface {
    // compare() should return:
    // - negative number if this < other
    // - zero if this == other  
    // - positive number if this > other
    compare(other: Self) -> i32
};
```

## Interface vs Type Assertions

When you have an interface value, you can check what concrete type it holds, but this is generally discouraged in favor of using interface methods:

```ferret
let shape: Shape = { .X: 10, .Y: 20 } as Point;

// Prefer using interface methods
let area := shape.area();  // ✅ Good - works with any Shape

// Avoid checking concrete types
// (Type assertions aren't directly supported, but the principle applies)
// Prefer polymorphism over type checking
```

## Common Mistakes

### Forgetting Required Methods

If you forget to implement a required method, your type won't implement the interface:

```ferret
type Shape interface {
    area() -> i32,
    move(x: i32, y: i32)
};

type Point struct {
    .X: i32,
    .Y: i32
};

fn (p: Point) area() -> i32 {
    return 0;
}

// ❌ Missing move() method - Point does NOT implement Shape
// You'll get a compile error if you try to use Point as Shape
```

### Signature Mismatches

Even small differences in signatures prevent interface implementation:

```ferret
type Shape interface {
    area() -> i32
};

type Rectangle struct {
    .width: i32,
    .height: i32
};

// ❌ Wrong return type - won't implement Shape
fn (r: Rectangle) area() -> f64 {
    return r.width * r.height;
}

// ✅ Correct - matches interface
fn (r: Rectangle) area() -> i32 {
    return r.width * r.height;
}
```

:::note
Key points to remember:

1. **Interfaces define method signatures**, not implementations
2. **Types implement interfaces automatically** if they have matching methods
3. **You can use interface types** anywhere the interface is expected
4. **The empty interface `interface{}`** is implemented by all types
5. **Method signatures must match exactly** for implementation
:::

## Next Steps

Now that you understand interfaces, explore how they work with other features:

- [Learn about Methods](/type-system/methods) - Methods are how you implement interfaces
- [Explore Structs](/type-system/structs) - Common types that implement interfaces
- [Understand Enums](/type-system/enums) - Enums can also implement interfaces
- [Master Generics](/advanced/generics) - Combine interfaces with generic programming