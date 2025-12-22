# Ferret Compiler TODO

## Current Stage
- Front-end pipeline reaches MIR (parse/resolve/typecheck/CFG/HIR-lower/MIR).
- C backend detached; MIR is final output for now.
- Fixed array indexing requires compile-time constants (negative const indices allowed). Non-const index -> T0028.
- Dynamic arrays still use runtime bounds checks.

## Next Work Items
### Incremental plan (easy -> hard, dependency-ordered)
1) Builtin primitive coverage (foundation)
   - Ensure full builtin type support in MIR + QBE (i8..i256/u8..u256, f32..f256, bool, str, byte).
   - Validate const/load/store coverage for those primitives.
2) Control flow completeness (depends on 1)
   - MIR lowering for if/else (non-block else, else-if chains).
   - Ensure for/while desugar to supported MIR (range/array for first).
3) Structs + enums end-to-end (depends on 1)
   - Solidify data layout for structs/enums and pointer/reference handling.
   - MIR lowering for struct literals/field access/assignment.
   - MIR lowering for enum variants and pattern usage.
4) Methods on named types + cross-module usability (depends on 3)
   - Receiver lowering + name mangling across modules.
   - Cross-module symbol resolution in MIR/QBE (imports + qualified names).
5) Collections and loops over them (depends on 1, 3)
   - MIR lowering for map literals.
   - MIR lowering for map iteration (MapIterInit/MapIterNext) and map for-loops.
   - MIR lowering for dynamic array literals in expressions.
6) Interfaces (depends on 3, 4)
   - Interface representation (vtable layout + method dispatch).
7) Functions beyond named decls (hard; depends on 3, 4)
   - Function literals and closure capture strategy (if supported in AST).
8) Error flow (depends on 1)
   - MIR lowering for result/catch paths.
9) Backend targets
   - MIR -> target IR/codegen (native + wasm).
   - Decide runtime behavior for dynamic array OOB (trap vs panic).
