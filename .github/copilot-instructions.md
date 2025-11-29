# Ferret Compiler Development Guide

## Project Overview

Ferret is a Go-based compiler for a modern systems programming language with:
- Go-style AST architecture
- Rust-style error diagnostics with rich source context
- Module system with import path resolution (local, builtin, remote)
- Parallel compilation pipeline with bounded worker pools

**Module:** `compiler` (not a typical `github.com/...` path)

## Architecture

### Compilation Pipeline (internal/pipeline/)

The compiler uses a **3-state concurrent pipeline**: `Unseen → Parsing → Done`

```go
// Each module progresses through phases independently
PhaseNotStarted → PhaseLexed → PhaseParsed → PhaseCollected → PhaseResolved → PhaseTypeChecked → PhaseCodeGen
```

**Critical invariants** (enforced by `pipeline_test.go`):
- Each module is parsed exactly once (deduplicated by import path)
- Parallel parsing uses semaphore-bounded workers (`runtime.NumCPU()`)
- Diagnostic collection is thread-safe (`sync.Mutex` in `DiagnosticBag`)
- Phase advancement requires prerequisites (checked via `phasePrerequisites` map)

Example: A module at `PhaseResolved` requires all imports at least `PhaseParsed`.

### Module System (internal/context_v2/)

**Import path semantics** (not filesystem paths):
- `"test_project/utils"` → `{ProjectRoot}/test_project/utils.fer`
- `"std/math"` → `{BuiltinModulesPath}/std/math.fer`
- Future: `"github.com/user/pkg"` → Remote dependency

**Key types:**
- `CompilerContext`: Central state manager with `Modules` map (import path → Module)
- `Module`: Holds `ImportPath`, `FilePath`, `Type`, `Phase`, `AST`, `Symbols`, and `Mu` (mutex for field updates)
- `SymbolTable`: Lexical scope chain for symbol resolution

Import cycle detection uses `DepGraph` (map of import path → dependencies).
Dependencies are automatically deduplicated to avoid redundant edges.

### Error Diagnostics (internal/diagnostics/)

Rust-style error output with:
- Primary/secondary labels with `^^^` and `---` underlines
- Color-coded severity (Error, Warning, Info, Hint)
- Source context with line numbers
- Notes and help suggestions

```go
diagnostics.NewError("message").
    WithPrimaryLabel(filepath, location, "explanation").
    WithSecondaryLabel(otherFile, otherLoc, "context").
    WithHelp("try doing X instead")
```

Thread-safe collection via `DiagnosticBag.Add()` with internal mutex.
**SourceCache** (for error reporting) also uses `RWMutex` for concurrent access.

### Lexer (internal/frontend/lexer/)

Regex-based tokenizer that:
- Continues on errors instead of panicking (collects all errors in `Errors` slice)
- Reports unrecognized characters as diagnostics
- Allows parser to find additional errors downstream

### AST Structure (internal/frontend/ast/)

**Interface hierarchy:**
- `Node` (base) → `Expression`, `Statement`, `Decl`, `TypeNode`, `BlockConstruct`
- Locations stored as `source.Location` with `Start` and `End` positions

**Parser state tracking:**
- `seenNonImport` flag enforces imports-first ordering
- Error recovery via `synchronize()` at statement boundaries

## Development Workflows

### Build & Run

```bash
# Build native + WASM
./build.sh          # → bin/ferret + website/public/ferret.wasm

# Run directly (development)
go run main.go test_project/main.fer

# With debug output
go run main.go -d test_project/main.fer
```

### Testing

Standard Go tests with table-driven patterns:

```bash
go test ./...                          # All tests
go test -v ./internal/pipeline         # Verbose output
go test -run TestPipelineInvariants    # Specific test
```

**Test coverage:**
- `pipeline_test.go`: Phase invariants, cycle detection, parallel safety
- `*_test.go`: Unit tests follow Go conventions (no external frameworks)

### Module Resolution

Entry point resolution order:
1. Check `Content` field (in-memory/WASM mode)
2. Resolve import path to file path via `ImportPathToFilePath()`
3. Read file, tokenize, parse
4. Extract imports from AST, recursively process

**Builtin modules:** Located in `ferret_libs/` (e.g., `ferret_libs/std/math.fer`)

## Project Conventions

### Error Handling

- **Diagnostics for user errors:** Use `ctx.Diagnostics.Add()` with location info
- **Internal errors:** Use `ctx.ReportError()` for compiler bugs (no location)
- Never panic in parsing/compilation paths (only for assertions in tests)

### Concurrency Patterns

- Module state protected by `CompilerContext.mu` (RWMutex) for read/write access
- Module field updates protected by `Module.Mu` (Mutex) during parsing
- Diagnostic collection protected by `DiagnosticBag.mu`
- SourceCache protected by `sync.RWMutex` for concurrent file content access
- Pipeline uses `sync.WaitGroup` + semaphore channel for bounded parallelism
- Always check state before state transitions (3-state: Unseen/Parsing/Done)
- `GetModulePhase()` uses `RLock`, `SetModulePhase()` uses `Lock`

### Naming Conventions

- Exported fields in structs use PascalCase
- Ferret struct fields use dot prefix: `.x`, `.y` (parsed as struct literals)
- Test files follow Go convention: `*_test.go`
- Import paths use forward slashes (converted to OS paths internally)

### File Organization

```
internal/
  compiler/          # Entry point: Compile() function
  pipeline/          # Lex → Parse → (future) Analyze → Codegen
  context_v2/        # Module registry, phase tracking, config
  frontend/
    lexer/           # Regex-based tokenizer (no separate scanner)
    parser/          # Recursive descent, error recovery
    ast/             # AST node definitions
  diagnostics/       # Error collection & rendering
  source/            # Position/Location tracking
  types/             # Built-in types (i8, i32, str, etc.)
  utils/             # Helpers (fs, strings, numeric, stack)
```

## Language Features (for test cases)

- **Type inference:** `let x := 42` infers `i32`
- **Optional types:** `i32?` with elvis operator `value ?: default`
- **Error types:** `Result ! Error` with `catch` blocks
- **Structs:** `type Point struct { .x: f64, .y: f64 };`
- **Enums:** `type Color enum { Red, Green, Blue };`
- **Methods:** `fn (p: Point) distance() -> f64 { ... }`

## Common Pitfalls

1. **Don't modify `ctx.Modules` without locking** (`ctx.mu.Lock()`)
2. **Don't assume phase progression** (always check with `GetModulePhase()`)
3. **Import paths ≠ file paths** (use `ImportPathToFilePath()` for resolution)
4. **Include 3+ lines context** when modifying parser error messages (helps locate code)
5. **WASM build uses different entry point** (`main_wasm.go` vs `main.go`)

## Key Files to Reference

- `internal/context_v2/context.go`: Architecture comments (lines 1-20)
- `internal/pipeline/pipeline.go`: Concurrency patterns, 3-state machine
- `internal/diagnostics/emitter.go`: Error rendering with colors
- `internal/frontend/parser/parser.go`: Recursive descent patterns
- `test_project/`: Example `.fer` files for testing imports/features
