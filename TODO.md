# Ferret Compiler TODO

## Current Stage
- Front-end pipeline reaches MIR (parse/resolve/typecheck/CFG/HIR-lower/MIR).
- Large primitive runtime + MIR/QBE pointer ABI implemented (i128/u128/i256/u256/f128/f256).
- Fixed array indexing requires compile-time constants (negative const indices allowed). Non-const index -> T0028.
- Dynamic arrays still use runtime bounds checks.
- Blank identifier '_' is reserved for for-loop iterators; invalid elsewhere.

## Next Work Items
### Incremental plan (easy -> hard, dependency-ordered)
- [x] Builtin primitive coverage (foundation)
  - [x] Ensure full builtin type support in MIR + QBE (i8..i256/u8..u256, f32..f256, bool, str, byte).
  - [x] Validate const/load/store coverage for those primitives.
  - [x] Limb-based integer runtime (i128/u128/i256/u256) with 32/64-bit limb selection.
- [x] Control flow completeness (depends on large primitive support)
  - [x] MIR lowering for if/else (non-block else, else-if chains).
  - [x] Ensure for/while desugar to supported MIR (range/array for first).
- [x] Structs + enums end-to-end (depends on large primitive support)
  - [x] Solidify data layout for structs/enums and pointer/reference handling.
  - [x] MIR lowering for struct literals/field access/assignment.
  - [x] MIR lowering for enum variants (const values).
  - [x] MIR lowering for enum pattern usage (match on enum variants).
- [ ] Methods on named types + cross-module usability (depends on structs/enums)
  - [ ] Receiver lowering + name mangling across modules.
  - [ ] Cross-module symbol resolution in MIR/QBE (imports + qualified names).
- [ ] Collections and loops over them (depends on structs/enums)
  - [ ] MIR lowering for map literals.
  - [ ] MIR lowering for map iteration (MapIterInit/MapIterNext) and map for-loops.
  - [ ] MIR lowering for dynamic array literals in expressions.
- [ ] Interfaces (depends on structs/enums + methods)
  - [ ] Interface representation (vtable layout + method dispatch).
- [ ] Functions beyond named decls (hard; depends on structs/enums + methods)
  - [ ] Function literals and closure capture strategy (if supported in AST).
- [ ] Error flow (depends on large primitive support)
  - [ ] MIR lowering for result/catch paths.
- [ ] Backend targets
  - [ ] MIR -> target IR/codegen (native + wasm).
  - [ ] Decide runtime behavior for dynamic array OOB (trap vs panic).
