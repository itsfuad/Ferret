# Compiler Architecture Fix Plan

## Summary
The compiler’s **core architecture is correct and modern** (semantic import paths, per-module phase tracking, dependency graph, universe scope). The main work now is to align **execution** with that design and remove scaling traps.

This plan combines **blocking fixes** (must do now) and **medium‑term friction fixes** (do soon to keep architecture extendable).

---

## Blocking Issues (Fix Now)

### Issue 1: Discovery parses every file, then Phase 2 parses again (2× work)

**What’s happening**
- `discoverModules()` calls `quickScanImports()`.
- `quickScanImports()` currently does **full lex + full parse**.
- Phase 2 (`runLexParsePhase`) lex+parses again.

**Why it’s a problem**
- Doubles frontend cost per file.
- Makes discovery expensive (hurts incremental builds).
- Undercuts the benefit of parallel lex/parse.

**What we need to do (pick one)**

**Option A (preferred): implement real “quick import scan”.**
- Scan only top‑level import section.
- Stop at first non‑import declaration.
- No full AST in discovery.

**Option B (simple now): reuse AST from discovery.**
- In discovery, after parsing:
  - `module.AST = astModule`
  - `module.Phase = PhaseParsed`
- Phase 2 skips already‑parsed modules.

**Acceptance**
- Each file is parsed **exactly once** per compilation run.

---

### Issue 2: Parallel lex/parse is not thread‑safe (race risk)

**What’s happening**
- `lexParseParallel()` spawns goroutines.
- Each goroutine merges diagnostics into shared `ctx.Diagnostics`.
- `DiagnosticBag.Add()` is not guaranteed thread‑safe.

**Why it’s a problem**
- Racy diagnostics, nondeterministic behavior, possible crashes.
- Future parallel phases will magnify this.

**What we need to do**
- Make diagnostics merging thread‑safe.

**Minimal acceptable fix**
- Add a mutex in `mergeDiagnostics()` OR inside `DiagnosticBag.Add()`.

Example (pipeline‑level lock):
```go
var diagMu sync.Mutex

func (p *Pipeline) mergeDiagnostics(diag *diagnostics.DiagnosticBag) {
    diagMu.Lock()
    defer diagMu.Unlock()
    for _, d := range diag.Diagnostics() {
        p.ctx.Diagnostics.Add(d)
    }
}
```

**Better long‑term fix**
- Put the lock inside `DiagnosticBag` so all `Add()` calls are safe.

**Acceptance**
- Parallel lex/parse produces stable diagnostics (no races).

---

### Issue 3: Errors get reported twice (structured + generic)

**What’s happening**
- Parser diagnostics are merged into context.
- The goroutine error collector also does:
  - `ctx.ReportError(err.Error(), nil)`

**Why it’s a problem**
- Duplicate errors.
- Second copy has no location.
- Output becomes noisy and confusing.

**What we need to do**
- Stop re‑emitting generic errors after diagnostics merge.

Two valid patterns:
1. Lex/parse **returns no error**, only diagnostics.
2. Collector uses returned errors only to stop pipeline, **does not call ReportError**.

**Acceptance**
- Each user‑facing error appears once, with correct location when available.

---

### Optional Safety Fix: Debug tooling nil‑guards

**What’s happening**
- `PrintModuleDetails()` assumes `module.AST` is non‑nil.

**Fix**
```go
nodes := 0
if module.AST != nil {
    nodes = len(module.AST.Nodes)
}
fmt.Printf("AST Nodes: %d\n", nodes)
```

**Acceptance**
- Debug tools never panic on failed parses.

---

## Medium‑Term Friction Points (Do Soon)

### Friction 1: Phase gating via `requiredPhase-1` is brittle

**What’s happening**
- `CanProcessPhase()` assumes strictly linear sequential phases.

**Why it becomes a problem**
- Adding/removing/reordering phases silently breaks arithmetic.
- Conditional or rerunnable phases won’t fit.

**What we need to do**
- Replace arithmetic with explicit prereq/transition map.

Example:
```go
var phasePrereq = map[ModulePhase]ModulePhase{
    PhaseLexed:       PhaseNotStarted,
    PhaseParsed:      PhaseLexed,
    PhaseCollected:   PhaseParsed,
    PhaseResolved:    PhaseCollected,
    PhaseTypeChecked: PhaseResolved,
    PhaseCodeGen:     PhaseTypeChecked,
}

func (ctx *CompilerContext) CanProcessPhase(path string, target ModulePhase) bool {
    cur := ctx.GetModulePhase(path)
    prereq, ok := phasePrereq[target]
    return ok && cur == prereq
}
```

**Acceptance**
- New phases require only updating the map.

---

### Friction 2: Import path normalization mixes semantic + filesystem layers

**What’s happening**
- Import paths are normalized using `filepath.*` helpers.

**Why it becomes a problem**
- Import paths are semantic identifiers; OS rules shouldn’t apply.
- Virtual/remote modules and multi‑root projects get tricky.

**What we need to do**
- Introduce semantic import normalizer and use it everywhere.

Example:
```go
func normalizeImportPath(p string) string {
    p = strings.TrimSpace(p)
    p = strings.ReplaceAll(p, "\\", "/")
    p = strings.Trim(p, "/")
    for strings.Contains(p, "//") {
        p = strings.ReplaceAll(p, "//", "/")
    }
    return p
}
```

**Acceptance**
- Import paths are treated purely semantically (no OS leakage).

---

### Friction 3: Source ownership is duplicated (`Module.Content` vs diagnostics cache)

**What’s happening**
- Source text lives in both `Module.Content` and `Diagnostics` cache.

**Why it becomes a problem**
- Incremental edits/in‑memory code can desync diagnostics.

**What we need to do (pick one)**

**Option A (simple):** Diagnostics reads text from modules.
- Remove source cache from diagnostics.
- Diagnostics asks `ctx.Modules[path].Content`.

**Option B (standard):** Add `SourceManager` to context.
- `SourceManager` owns `virtualPath -> string`.
- Both modules and diagnostics read from it.

**Acceptance**
- Exactly one source of truth for file text.

---

### Friction 4: Entry module naming differs between file vs in‑memory mode

**What’s happening**
- File mode derives entry module via `FilePathToImportPath`.
- In‑memory mode uses `ProjectName + "/" + moduleName`.

**Why it becomes a problem**
- Divergent semantics between CLI and playground/WASM.
- Potential import resolution and test mismatches.

**What we need to do**
- Route both through the same import‑path derivation helper.

**Acceptance**
- Entry module identity is consistent across compilation modes.

---

### Friction 5: Builtin modules map not populated in `Compile()`

**What’s happening**
- `ImportPathToFilePath()` resolves builtins using `Config.BuiltinModules`.
- `Compile()` sets `BuiltinModulesPath` but never initializes `BuiltinModules`.

**Why it becomes a problem**
- Stdlib imports may never resolve unless another step fills the map.

**What we need to do**
- Load builtins during context/pipeline init.

Two acceptable options:
1. Load builtins inside `context_v2.New()` from `BuiltinModulesPath`.
2. Pipeline calls `ctx.LoadBuiltinModules()` before discovery.

**Acceptance**
- Builtin import paths resolve automatically.

---

## Global Acceptance Criteria
After all fixes:
1. Each file is parsed **exactly once** per run.
2. Parallel lex/parse is race‑free.
3. Diagnostics are single‑source and non‑duplicated.
4. Phase transitions are explicit and safe to extend.
5. Import paths are semantic identifiers only.
6. Entry module identity matches across modes.
7. Builtins resolve out of the box.

