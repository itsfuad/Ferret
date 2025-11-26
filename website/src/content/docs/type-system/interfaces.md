---
title: Interfaces
description: Interface types and polymorphism in Ferret
---

Interfaces define contracts that types can implement. In simple terms, an interface specifies a set of methods that a type must provide to "implement" that interface.

## Interface Definition

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
```

## Implementing Interfaces

```ferret
type Circle struct {
    .x: i32,
    .y: i32,
    .radius: i32,
};

fn (c: Circle) draw() {
    print("Drawing circle at (" + c.x + ", " + c.y + ")");
}

fn (c: Circle) get_bounds() -> struct {
    .x_min: i32,
    .y_min: i32,
    .x_max: i32,
    .y_max: i32
} {
    return .{
        .x_min = c.x - c.radius,
        .y_min = c.y - c.radius,
        .x_max = c.x + c.radius,
        .y_max = c.y + c.radius
    };
}
```

## Using Interfaces

```ferret
fn render(shape: Drawable) {
    shape.draw();
}

let circle := Circle{.x = 10, .y = 20, .radius = 5};
render(circle);
```

## Next Steps

- [Learn about Generics](/generics)
- [Explore Error Handling](/errors)
