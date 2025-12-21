# Ferret Compiler TODO

## Current Stage
- Front-end pipeline reaches MIR (parse/resolve/typecheck/CFG/HIR-lower/MIR).
- C backend detached; MIR is final output for now.
- Fixed array indexing requires compile-time constants (negative const indices allowed). Non-const index -> T0028.
- Dynamic arrays still use runtime bounds checks.

## Next Work Items
- MIR lowering for map literals (map[K]V{...} / {} as map[K]V).
- MIR lowering for map iteration (MapIterInit/MapIterNext in for/foreach).
- MIR lowering for range-based for/foreach loops.
- MIR lowering for dynamic array literals in expressions (not just var init).
- MIR lowering for result/catch paths.
- Decide runtime behavior for dynamic array OOB (trap vs panic).
- Backend: MIR -> target IR/codegen (native + wasm).
