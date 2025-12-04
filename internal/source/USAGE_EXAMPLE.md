# Using Location.GetText() for Debugging

The `Location.GetText()` method extracts the actual source code text from a file for a given location. This is useful for debugging and error reporting.

## Basic Usage

```go
// Given a location from an AST node
location := expr.Loc()

// Get the source code text
sourceText := location.GetText()
fmt.Printf("Expression text: %s\n", sourceText)
```

## Example: Debug Type Checking

```go
func debugTypeError(ctx *context_v2.CompilerContext, node ast.Expression) {
    inferredType := inferExprType(ctx, mod, node)
    sourceText := node.Loc().GetText()
    
    fmt.Printf("DEBUG: Expression '%s' has type %s\n", 
        sourceText, inferredType.String())
}
```

## Example: Enhanced Error Messages

```go
// Instead of just showing locations, show the actual code
loc := expr.Loc()
ctx.Diagnostics.Add(
    diagnostics.NewError("type mismatch").
        WithPrimaryLabel(loc, fmt.Sprintf("this expression: '%s'", loc.GetText())),
)
```

## Features

- **Single-line extraction**: Extracts code from start column to end column
- **Multi-line extraction**: Handles expressions spanning multiple lines
- **Safe error handling**: Returns empty string on invalid locations or file errors
- **No panics**: Never panics, always returns a string (empty on error)

## Use Cases

1. **Debugging type inference**: See what source code corresponds to a type
2. **Enhanced diagnostics**: Show actual code in error messages
3. **Testing**: Verify AST nodes map to correct source locations
4. **IDE features**: Show hover information with actual code snippets
