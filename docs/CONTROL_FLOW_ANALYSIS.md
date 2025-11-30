# Control Flow Analysis Implementation

## Overview

This implementation adds comprehensive control flow analysis to the Ferret compiler's semantic analysis phase. It includes:

1. **Parameter Validation** - Validates function parameters including variadic rules
2. **Control Flow Graph (CFG)** - Builds a graph representation of function execution paths  
3. **Return Path Analysis** - Ensures all code paths return values for non-void functions
4. **Unreachable Code Detection** - Identifies code that can never execute
5. **Break/Continue Validation** - Ensures loop control statements are used correctly

## Architecture

### Module Structure

```
internal/semantics/controlflow/
├── cfg.go          # Control flow graph builder and return analysis
├── cfg_test.go     # CFG and return path tests
├── params.go       # Parameter validation
└── params_test.go  # Parameter validation tests
```

### Integration Point

The control flow analysis hooks into the typechecker phase via `checkFuncDecl()` in `internal/semantics/typechecker/typechecker.go`:

```go
func (tc *typeChecker) checkFuncDecl(decl *ast.FuncDecl) {
    // 1. Validate parameters
    controlflow.ValidateFunctionParams(tc.ctx, tc.mod, decl)
    
    // 2. Create parameter scope and type check body
    // ... parameter scoping and type checking ...
    
    // 3. Build CFG and analyze control flow
    cfgBuilder := controlflow.NewCFGBuilder(tc.ctx, tc.mod)
    cfg := cfgBuilder.BuildFunctionCFG(decl)
    controlflow.AnalyzeReturns(tc.ctx, tc.mod, decl, cfg)
}
```

## Features

### 1. Parameter Validation

**Validates:**
- Variadic parameters must be the last parameter
- Only one variadic parameter per function
- No duplicate parameter names
- All parameters have type annotations

**Example Errors:**

```ferret
// ERROR: Variadic parameter must be last
fn test(args: ...i32, x: i32) -> i32 { }

// ERROR: Multiple variadic parameters
fn test(args1: ...i32, args2: ...i32) -> i32 { }

// ERROR: Duplicate parameter name
fn test(x: i32, x: i32) -> i32 { }
```

### 2. Control Flow Graph

The CFG represents function execution as a graph of basic blocks:

```go
type BasicBlock struct {
    ID           int           // Unique identifier
    Nodes        []ast.Node    // Statements in this block
    Successors   []*BasicBlock // Possible next blocks
    Predecessors []*BasicBlock // Blocks that can reach this
    Terminator   ControlFlowKind // How block ends
    Reachable    bool          // Whether block is reachable
    Returns      bool          // Whether block always returns
    CanFallThru  bool          // Whether execution can fall through
}
```

**Handles:**
- Sequential statements
- Conditional branches (if-else, else-if chains)
- Loops (for, while)
- Early exits (return, break, continue)

### 3. Return Path Analysis

Ensures non-void functions return values in all code paths.

**Valid:**

```ferret
fn test(x: i32) -> i32 {
    if x > 0 {
        return 1;
    } else {
        return -1;
    }
    // All paths return
}
```

**Error:**

```ferret
fn test(x: i32) -> i32 {
    if x > 0 {
        return 1;
    }
    // ERROR: Missing return - else path doesn't return
}
```

**Diagnostic Output:**

```
error: missing return statement
  --> test.fer:1:1
   | 
 1 | fn test(x: i32) -> i32 {
   | ^ function 'test' must return a value in all code paths
 2 |     if x > 0 {
   |     - this branch doesn't return
   | 
   = help: ensure all code paths return a value
```

### 4. Unreachable Code Detection

Detects code after return, break, or continue statements.

**Warning:**

```ferret
fn test() -> i32 {
    return 42;
    let x := 10;  // WARNING: Unreachable code
    return x;     // WARNING: Unreachable code
}
```

**Diagnostic Output:**

```
warning: unreachable code
  --> test.fer:3:5
   | 
 3 |     let x := 10;
   |     ^^^^^^^^^^^^ this code will never execute
   | 
   = help: remove this code or restructure control flow
```

### 5. Break/Continue Validation

Ensures break and continue are only used inside loops.

**Error:**

```ferret
fn test() {
    break;  // ERROR: break outside loop
}
```

**Valid:**

```ferret
fn test() {
    for let i := 0; i < 10; i := i + 1 {
        if i == 5 {
            break;  // OK: inside loop
        }
        if i % 2 == 0 {
            continue;  // OK: inside loop
        }
    }
}
```

## Algorithm Details

### CFG Construction

1. **Entry Block**: Create entry point (always reachable)
2. **Sequential Flow**: Add statements to current block
3. **Branching**: Create separate blocks for then/else paths
4. **Loops**: Create header, body, and after blocks with back edges
5. **Terminators**: Mark blocks that end with return/break/continue
6. **Merge Points**: Join branches back to single execution path

### Return Analysis

1. **Entry Point**: Start from entry block
2. **DFS Traversal**: Explore all paths through the CFG
3. **Track Returns**: Mark blocks that execute return statements
4. **Check Exit**: If exit is reachable without return → error
5. **Report Branches**: Identify specific branches missing returns

### Unreachable Detection

1. **Mark Reachable**: Propagate reachability from entry
2. **Check Successors**: If current block terminates, successors unreachable
3. **Report**: Generate warnings for unreachable statements

## Test Coverage

### CFG Tests (`cfg_test.go`)
- Basic CFG construction
- Unreachable code after return
- If-else control flow
- Missing returns in branches
- Void functions (no return required)
- Break/continue in/outside loops
- Nested conditionals
- Complex branching scenarios
- While loops

### Parameter Tests (`params_test.go`)
- Variadic parameter position
- Multiple variadic parameters
- Valid variadic usage
- Duplicate parameter names
- Valid parameter scoping
- No parameters
- Anonymous functions

## Performance Considerations

1. **Per-Function Analysis**: CFG built once per function
2. **Linear Traversal**: Most operations are O(blocks + edges)
3. **Parallel Safe**: Uses module-level locks, safe with parallel parsing
4. **Memory Efficient**: CFG discarded after analysis

## Future Enhancements

### Potential Additions
1. **Definite Assignment** - Track variable initialization
2. **Dead Code Elimination** - Remove unreachable blocks during codegen
3. **Constant Propagation** - Use CFG for optimization passes
4. **Data Flow Analysis** - Track value flow through program
5. **Exhaustiveness Checking** - Verify all enum/pattern cases covered

### Integration Points
- **Anonymous Functions**: Already structured to handle temp IDs
- **Closures**: Can extend to track captured variables
- **Async/Await**: Can model suspension points in CFG
- **Error Handling**: Can track error propagation paths

## Usage Example

```go
// In typechecker
func (tc *typeChecker) checkFuncDecl(decl *ast.FuncDecl) {
    // Validate parameters
    controlflow.ValidateFunctionParams(tc.ctx, tc.mod, decl)
    
    // Create parameter scope
    funcScope := controlflow.CreateParameterScope(
        tc.ctx, tc.mod, tc.currentScope, decl.Type.Params,
    )
    
    // Type check function body
    // ... type checking code ...
    
    // Analyze control flow
    cfgBuilder := controlflow.NewCFGBuilder(tc.ctx, tc.mod)
    cfg := cfgBuilder.BuildFunctionCFG(decl)
    controlflow.AnalyzeReturns(tc.ctx, tc.mod, decl, cfg)
}
```

## References

- **Go AST**: Similar structure to Go's compiler
- **Rust Borrow Checker**: CFG approach inspired by Rust MIR
- **LLVM**: Basic block concept from LLVM IR
- **Dragon Book**: Classic algorithms for control flow analysis
