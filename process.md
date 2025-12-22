```d2
direction: right

cli: "ferret CLI\n(main.go)"
wasm: "WASM entry\n(Compile code string)"
opts: "compiler.Options\nflags + entry file or code"
cli -> opts
wasm -> opts

ctx: {
  label: "CompilerContext"
  entry: "Entry module"
  modules: "Modules\n(import path -> Module)"
  depgraph: "DepGraph"
  topo: "Topological order"
  diagnostics: "Diagnostics"
  universe: "Universe scope\n(builtins)"
  config: "Config\n(project + runtime + backend)"

  entry -> modules
  config -> modules
  modules -> depgraph -> topo
}

opts -> ctx.config
opts -> ctx.entry

parse: {
  label: "Phase 1: Lex + Parse (parallel)"
  discovery: "Module discovery\nImportPath -> FilePath\n(native:// builtins)"
  read: "Read file content\n(or in-memory code)"
  lexer: "Lexer"
  parser: "Parser -> AST"
  store: "Store AST + ModuleScope"
  imports: "Top-level imports\nAdd dep edges"
  spawn: "Spawn goroutines\nprocess imports"

  discovery -> read -> lexer -> parser -> store -> imports -> spawn
  spawn -> discovery: "goroutine"
}

ctx.entry -> parse.discovery
parse.store -> ctx.modules
parse.imports -> ctx.depgraph
parse.parser -> ctx.diagnostics: "syntax errors"
parse.read -> ctx.diagnostics: "IO errors"
parse.imports -> ctx.diagnostics: "import errors"

order: "Compute topological order"
ctx.depgraph -> order -> ctx.topo

phases: {
  label: "Phase 2..8 (per module, topo order)"
  collect: "Collector\nsymbols + scopes"
  resolve: "Resolver\nname binding + imports"
  typecheck: "Typechecker\nassign types + compatibility"
  hirgen: "HIR generation\n(typed AST -> HIR)"
  cfg: "HIR CFG analysis\nconst-eval + dead-code"
  hirlower: "HIR lowering\ncanonical HIR"
  mirgen: "MIR generation\n(HIR -> MIR)"

  collect -> resolve -> typecheck -> hirgen -> cfg -> hirlower -> mirgen
}

order -> phases.collect

module_state: {
  label: "Per-module state (ctx.Modules)"
  ast: "AST"
  scopes: "Symbol tables\n(ModuleScope + ExprTypes)"
  hir_src: "HIR (source)"
  hir_low: "HIR (lowered)"
  mir: "MIR"
}

ctx.modules -> module_state
ctx.universe -> module_state.scopes: "parent scope"
parse.store -> module_state.ast
phases.collect -> module_state.scopes
phases.typecheck -> module_state.scopes
phases.hirgen -> module_state.hir_src
phases.cfg -> module_state.hir_src
phases.hirlower -> module_state.hir_low
phases.mirgen -> module_state.mir

phases.resolve -> ctx.diagnostics
phases.typecheck -> ctx.diagnostics
phases.cfg -> ctx.diagnostics
phases.hirlower -> ctx.diagnostics
phases.mirgen -> ctx.diagnostics

codegen: {
  label: "Phase 9: Codegen + Link"
  select: "Backend select\nnone | c | qbe"
  skip: "Skip codegen\n(-t, backend=none, or errors)"
  cgen: "C backend\n(AST + symbols -> .h/.c)"
  qbe: "QBE backend\n(MIR -> .ssa -> .s)"
  temp: "Temp dir\n<output>/gen"
  link_step: "Link\nclang/gcc + runtime"
  output: "Binary / exe"

  select -> skip: {style.opacity: 0.6}
  select -> cgen
  select -> qbe
  cgen -> temp -> link_step -> output
  qbe -> temp -> link_step -> output
}

phases.mirgen -> codegen.select
ctx.config -> codegen.select
ctx.diagnostics -> codegen.skip: "HasErrors"
module_state.ast -> codegen.cgen
module_state.scopes -> codegen.cgen
module_state.mir -> codegen.qbe

runtime: "runtime/\n(io.c, interface.c, map.c, bigint.c, array.c, string_builder.c)"
runtime -> codegen.link_step

result: "CompilerResult\n(binary + diagnostics)"
codegen.output -> result
codegen.skip -> result
ctx.diagnostics -> result
result -> cli
result -> wasm
```
