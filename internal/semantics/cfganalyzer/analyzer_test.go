package cfganalyzer

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
	"compiler/internal/semantics/collector"
	"compiler/internal/semantics/table"
	"compiler/internal/semantics/typechecker"
	"testing"
)

func TestIfConstantCondition_UsesBlockScope(t *testing.T) {
	t.Parallel()

	cfg := &context_v2.Config{
		ProjectName: "test",
		ProjectRoot: t.TempDir(),
		Extension:   ".fer",
	}
	ctx := context_v2.New(cfg, false)

	mod := &context_v2.Module{
		FilePath:       "test.fer",
		Content:        `fn main() { let a := 10; if a == 10 { let x := 1; } else { let y := 2; } }`,
		ModuleScope:    table.NewSymbolTable(ctx.Universe),
		ImportPath:     "test",
		Imports:        make(map[string]*context_v2.Import),
		ImportAliasMap: make(map[string]string),
	}
	mod.CurrentScope = mod.ModuleScope
	ctx.AddModule(mod.ImportPath, mod)

	lex := lexer.New(mod.FilePath, mod.Content, ctx.Diagnostics)
	tokens := lex.Tokenize(false)
	mod.AST = parser.Parse(tokens, mod.FilePath, ctx.Diagnostics)

	collector.CollectModule(ctx, mod)
	typechecker.CheckModule(ctx, mod)

	AnalyzeModule(ctx, mod)

	diags := ctx.Diagnostics.Diagnostics()
	for _, d := range diags {
		if d.Severity == diagnostics.Warning && d.Code == diagnostics.WarnConstantConditionTrue {
			return
		}
	}

	t.Fatalf("expected warning %s, got %d diagnostics", diagnostics.WarnConstantConditionTrue, len(diags))
}
