# Ferret Programming Language
Welcome to Ferret! Ferret is a statically typed, beginner-friendly programming language designed to bring clarity, simplicity, and expressiveness to developers. With a focus on readability and a clean syntax, Ferret makes it easier to write clear, maintainable code while embracing modern programming principles.

Key Features
- Statically Typed: Strong typing ensures that errors are caught early, making your code more predictable and robust.
- Beginner-Friendly: Ferret’s syntax is designed to be easy to read and understand, even for new developers.
- Expressive Code: With simple syntax and clear semantics, Ferret is made to be highly expressive without unnecessary complexity.
- First-Class Functions: Functions are treated as first-class citizens, enabling functional programming paradigms while maintaining simplicity.
- Clear Structs and Interfaces: Structs have methods and are used for simplicity, with implicit interface implementation for cleaner code.

## Basic Syntax (subject to change any time)
```rs
//single variable with type inference
let x = 10;
let y: f32;
let myname: str = "John";

//multiple variables with type
let p, q, r: i32, f32, str = 10, 20.0, "hello";
let p, q: i32, str = 10, "hello";

//multiple variables with type inference
let p, q, r = 10, 20.0, "hello";
let p, q = 10, "hello";

// assignments: single variable
x = 15;

// assignments: multiple variables
p, q = 10, "hello";
p, q, r = 10, 20.0, "hello";

a; // single expression

// array
let arr1: []i32;
let arr2: []str = ["hello", "world"];

a = (a + b) * c;
a = x++;
a = x--;
a = ++x;
a = --x;

//array access
arr1[0];
arr1[0] = 10;
a = arr1[0];

//single variable with type
let x: i32 = 10; //explicit type and value

//multiple variables with type inference
let x, y = 10, 20; //infer from values

//multiple variables with type
let x, y, z: i32 = 10, 20, 3; // same type
let p, q, r: i32, f32, str = 10, 20.0, "Hello"; // different types

//multiple variables with type inference and type
let x, y = 10, 20;

let arr, other: []i32, e = [1, 2, 3], 3;
let arr2d: [][]i32 = [[1, 2], [4, 5, 6]];

//user defined types
type Integer i32;

const user: {
    name: str,
    age: i32
} = {
    name: "John",
    age: 20
};

type Point {
    x: i32,
    y: i32
};

let point: Point = {x: 10, y: 20};
```

## Roadmap
- [x] Basic syntax
- [x] Tokenizer
- [x] Parser
- [x] Variable declaration and assignment
- [x] Expressions
- [ ] Data structures
- [ ] Conditionals
- [ ] Functions
- [ ] User-defined types
- [ ] Structs
- [ ] Interfaces
- [x] Unary operators
- [x] Increment/Decrement operators
- [x] Assignment operators
- [x] Grouping
- [ ] Type casting
- [x] Array
- [ ] Map
- [ ] Range
- [ ] Rich error reporting
- [ ] Branch analysis
- [ ] For loops
- [ ] While loops
- [ ] Switch statements
- [ ] Imports and modules
- [ ] Nullable or optional types or pointers or references
- [ ] Generics
- [ ] Advanced code generation
- [ ] Error handling
