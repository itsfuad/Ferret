# Ferret Compiler Roadmap

This document tracks planned improvements and enhancements to the Ferret compiler. Tasks are organized by priority and can be completed incrementally.

## Current Status

✅ Phase 1: Lexing & Parsing  
✅ Phase 2: Symbol Collection  
✅ Phase 3: Name Resolution  
✅ Phase 4: Type Checking (Core complete, needs polish)  
⏸️ Phase 5: Code Generation (Deferred until phases 1-4 are solid)

---

## Priority 1: Polish Existing Phases (Easy Wins)

### Documentation Cleanup
- [x] Update `.github/copilot-instructions.md`
  - [x] Remove outdated "bounded parallelism" and "semaphore" references
  - [x] Remove mention of non-existent `synchronize()` function
  - [x] Document actual unbounded goroutine approach
  - [x] Add current concurrency model explanation
- [x] Document lock ordering in `context_v2/context.go`
  - [x] Add comments explaining lock hierarchy (Context.mu → Module.Mu → DiagnosticBag.mu → SourceCache.mu)
  - [x] Document when to use RLock vs Lock
  - [x] Add example of proper lock usage
- [ ] Add architecture overview to main README
  - [ ] High-level pipeline explanation
  - [ ] Phase progression diagram
  - [ ] Module resolution explanation

### Parser Improvements (Low-Hanging Fruit)
- [x] Better error messages for common mistakes
  - [x] Missing semicolons (already good, can enhance)
  - [x] Mismatched brackets/braces
  - [x] Missing closing quotes
  - [x] Invalid token sequences
- [ ] Add more helpful hints to existing errors
  - [ ] Suggest fixes for common typos
  - [ ] Show expected token alternatives
  - [ ] Add examples in help text

### Type Checker Polish
- [x] Review and improve existing error messages
  - [x] Make type mismatch errors more specific
  - [x] Add context about where types come from
  - [x] Suggest possible fixes (casts, conversions)
  - [x] Changed "may lose precision" → "possible data loss"
  - [x] Only show data loss warning when target bit size < source bit size
- [x] Optional type narrowing
  - [x] Narrow T? to T in if/coalescing/comparison contexts
  - [x] Defer-based scope restoration for type narrowing
- [x] Byte type improvements
  - [x] Fixed byte literal type inference
  - [x] Byte and u8 require explicit cast (no implicit conversion)
  - [x] Added escape sequence support (\n, \t, \0, \xHH)
  - [x] Validate byte values fit in 0-255 range
- [x] Unicode support
  - [x] Full UTF-8 support in strings and comments
  - [x] Multi-byte character handling in position tracking
  - [x] Byte literals restricted to ASCII only
- [ ] Edge case handling
  - [ ] Validate all built-in type operations
  - [ ] Check array bounds in literals
  - [ ] Validate enum variant access
- [ ] Add more validation
  - [ ] Unused variables (warnings)
  - [ ] Unreachable code (already have, ensure comprehensive)
  - [ ] Dead code detection

---

## Priority 2: Testing & Robustness

### Testing Infrastructure (Start Simple)
- [x] Add more unit tests for existing code
  - [x] Parser edge cases (19 test cases added)
  - [x] Parser error recovery tests
  - [x] Valid program tests
  - [ ] Type checker scenarios
  - [ ] Symbol resolution cases
- [ ] Create test files for language features
  - [ ] One test file per feature (structs, enums, functions, etc.)
  - [ ] Test both valid and invalid code
  - [ ] Document expected behavior
- [ ] Integration tests
  - [ ] Simple multi-file projects
  - [ ] Import resolution tests
  - [ ] Error message snapshot tests
- [ ] Add test utilities
  - [ ] Helper to create test contexts
  - [ ] Helper to assert compilation success/failure
  - [ ] Helper to compare error messages

### Bug Fixes & Edge Cases
- [ ] Fix parser crash with for loops in function bodies (discovered in testing)
- [ ] Review GitHub issues for bug reports
- [ ] Test compiler with malformed input
- [ ] Test all type combinations
- [ ] Test all control flow paths
- [ ] Validate all semantic rules are enforced

---

## Priority 3: Language Features (After Polish)

### Type System Enhancements
- [ ] Generics/Parametric polymorphism
  - [ ] Design syntax and semantics
  - [ ] Implement type parameter parsing
  - [ ] Generic type checking
  - [ ] Monomorphization for code generation
- [ ] Union types
  - [ ] Syntax design
  - [ ] Type checking rules
  - [ ] Exhaustiveness checking
- [ ] Pattern matching
  - [ ] Match expressions
  - [ ] Destructuring
  - [ ] Guards
- [ ] Traits/Interfaces (if not yet complete)
  - [ ] Trait definitions
  - [ ] Implementation checking
  - [ ] Dynamic dispatch vs static dispatch

### Advanced Control Flow
- [ ] Match expressions (see above)
- [ ] Defer statements
- [ ] Error propagation operators (`?` operator)
- [ ] Async/await (future consideration)

### Module System Enhancements
- [ ] Remote module support
  - [ ] Git-based dependency resolution
  - [ ] Version management
  - [ ] Dependency caching
- [ ] Package manager integration
  - [ ] ferret.toml or similar for dependencies
  - [ ] Lock file for reproducible builds
  - [ ] Central package registry

---

## Priority 4: Code Generation (After Phases 1-4 are Solid)

### Code Generation Prerequisites
- [ ] Finalize type system semantics
- [ ] Complete all type checking validation
- [ ] Ensure control flow analysis is complete
- [ ] Document IR design decisions

### Basic Code Generation
- [ ] Choose backend (LLVM, Cranelift, QBE, or custom)
- [ ] Implement basic IR generation
  - [ ] Variables and constants
  - [ ] Functions
  - [ ] Basic types (integers, booleans)
  - [ ] Control flow (if, while, for)
- [ ] Runtime support
  - [ ] Memory allocation
  - [ ] Basic I/O
- [ ] Executable generation
  - [ ] Link with runtime
  - [ ] Produce working binary

---

## Priority 5: Optimization & Performance (Much Later)

### Developer Tools
- [ ] Language Server Protocol (LSP)
  - [ ] Completion
  - [ ] Go to definition
  - [ ] Find references
  - [ ] Rename refactoring
  - [ ] Hover information
  - [ ] Diagnostics in real-time
- [ ] Formatter (`ferret fmt`)
  - [ ] Consistent code style
  - [ ] Configurable rules
  - [ ] Integration with editors
- [ ] Documentation generator
  - [ ] Extract doc comments
  - [ ] Generate HTML/markdown docs
  - [ ] API reference generation
- [ ] Build system
  - [ ] Project configuration
  - [ ] Build profiles (debug/release)
  - [ ] Cross-compilation support

### Standard Library Expansion
- [ ] Core modules
  - [ ] Collections (List, Map, Set)
  - [ ] String manipulation
  - [ ] File I/O
  - [ ] Networking
  - [ ] Date/Time
  - [ ] JSON/serialization
- [ ] System modules
  - [ ] OS interaction
  - [ ] Process management
  - [ ] Environment variables
- [ ] Testing framework
  - [ ] Unit testing primitives
  - [ ] Test runner
  - [ ] Assertion library
  - [ ] Mock/stub support

### Playground & Education
- [ ] Web playground improvements
  - [ ] Share code snippets
  - [ ] Embedded examples in docs
  - [ ] Interactive tutorials
- [ ] Educational resources
  - [ ] Language tutorial
  - [ ] API documentation
  - [ ] Example projects
  - [ ] Best practices guide

⏸️ *Deferred optimizations - focus on correctness first*
- Dead code elimination
- Constant folding
- Inline expansion
- Loop optimizations
- Advanced backend features

---

## Priority 6: Advanced Features (Future)

### Advanced Type System
⏸️ *Deferred until basic compiler is complete*
- Generics/parametric polymorphism
- Union types
- Advanced pattern matching
- Trait/interface improvements

### Tooling & Ecosystem
⏸️ *Build after compiler is stable*
- Language Server Protocol (LSP)
- Formatter (`ferret fmt`)
- Documentation generator
- Package manager

---

## Next Steps (Start Here!)

### Week 1-2: Low-Hanging Fruit
1. [ ] Update `copilot-instructions.md` with accurate implementation details
2. [ ] Add lock ordering comments to `context_v2/context.go`
3. [ ] Write 5-10 additional test cases for existing features
4. [ ] Review and improve 3-5 error messages

### Week 3-4: Testing
1. [ ] Create comprehensive test suite for type checker
2. [ ] Add integration tests for multi-file projects
3. [ ] Test all language features systematically
4. [ ] Fix any bugs discovered

### Month 2: Edge Cases & Polish
1. [ ] Handle all parser edge cases gracefully
2. [ ] Ensure type checker catches all invalid programs
3. [ ] Improve error messages based on real usage
4. [ ] Add more helpful hints and suggestions

### Month 3+: Prepare for Code Generation
1. [ ] Finalize type system semantics
2. [ ] Complete validation rules
3. [ ] Choose code generation backend
4. [ ] Design IR representation

---

## How to Use This Roadmap

### For Contributors
1. Pick a task from Priority 1 or 2
2. Create an issue on GitHub referencing this task
3. Submit a PR when ready
4. Mark task as complete with commit hash

### For Maintainers
- Review and prioritize tasks regularly
- Add new tasks as they're identified
- Archive completed sections
- Keep priority levels up to date

### Task Status Symbols
- [ ] Not started
- 🚧 In progress
- ✅ Completed
- 🔄 Needs revision
- ⏸️ Blocked/Deferred
- ❌ Cancelled/Won't do

---

## Completed Tasks

### Recent Improvements (December 2025)
- ✅ **Optional Type Narrowing**
  - Implemented T? → T narrowing in conditional contexts
  - Coalescing operator type narrowing (x ?? default)
  - None literal comparison narrowing
  - Defer-based scope restoration for clean implementation
- ✅ **Byte Type Enhancements**
  - Fixed byte literal type inference (ast.BYTE → types.TypeByte)
  - Enforced explicit cast requirement between byte and u8
  - Added escape sequence support (\n, \t, \r, \0, \\, \', \", \xHH)
  - Byte value validation (0-255 range)
- ✅ **Unicode Support**
  - Full UTF-8 support in string literals (CJK, emoji, etc.)
  - Full UTF-8 support in comments (single-line and multi-line)
  - Fixed Position.Advance() to handle multi-byte UTF-8 correctly
  - Byte literals remain ASCII-only (0-127) as intended
- ✅ **Improved Error Messages**
  - Changed "may lose precision" to "possible data loss"
  - Only show data loss warning when target bit size < source bit size
  - Cleaner messages for same-size type conversions (byte ↔ u8)
  - Better cast suggestions in error messages

### Phase 1-4 Implementation (v0.1.0)
- ✅ Lexer with comprehensive token support
- ✅ Recursive descent parser with error collection
- ✅ Multi-phase pipeline architecture
- ✅ Symbol table with lexical scoping
- ✅ Name resolution
- ✅ Type checking with inference
- ✅ Control flow analysis
- ✅ Rust-quality error diagnostics
- ✅ Parallel parsing with module deduplication
- ✅ Import cycle detection
- ✅ Module system (local, builtin)

---

## Notes

- This roadmap is a living document and will evolve
- Tasks can be broken down into smaller subtasks as needed
- Priority levels may shift based on user feedback and project needs
- Feel free to propose new tasks via GitHub issues

**Last Updated:** December 6, 2025  
**Current Version:** v0.1.0-alpha
