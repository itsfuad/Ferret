---
title: Hello World
description: Your first Ferret program
---

Let's write your first Ferret program!

## Create a File

Create a file named `hello.fer`:

```ferret
// Your first Ferret program
import "std/io";

fn main() {
    let greeting: str = "Hello, World!";
    io::Println(greeting);
}
```

## Run the Program

Compile and run your program:

```bash
.\bin\ferret.exe hello.fer
```

## Understanding the Code

Let's break down what's happening:

- `fn main()` - Every Ferret program starts with a `main` function
- `let greeting: str = "Hello, World!";` - Declares a variable with type annotation
- `print(greeting);` - Outputs the greeting to the console

## Try It Yourself

Try modifying the program:

```ferret
fn greet(name: str) -> str {
    return "Hello, " + name + "!";
}

fn main() {
    let name: str = "Ferret";
    let message: str = greet(name);
    print(message);
}
```

## Next Steps

- [Learn about Ferret's syntax](/syntax)
- [Explore the type system](/types)
- [See more examples](/examples)