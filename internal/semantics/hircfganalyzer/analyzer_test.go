package hircfganalyzer_test

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
	"compiler/internal/hirgen"
	"compiler/internal/semantics/collector"
	"compiler/internal/semantics/hircfganalyzer"
	"compiler/internal/semantics/table"
	"compiler/internal/semantics/typechecker"
	"strings"
	"testing"
)

func analyzeHIR(t *testing.T, src string) *context_v2.CompilerContext {
	t.Helper()

	cfg := &context_v2.Config{
		ProjectName: "test",
		ProjectRoot: t.TempDir(),
		Extension:   ".fer",
	}
	ctx := context_v2.New(cfg, false)
	mod := &context_v2.Module{
		FilePath:       "test.fer",
		Content:        src,
		ModuleScope:    table.NewSymbolTable(ctx.Universe),
		ImportPath:     "test",
		Imports:        make(map[string]*context_v2.Import),
		ImportAliasMap: make(map[string]string),
	}
	mod.CurrentScope = mod.ModuleScope

	lex := lexer.New(mod.FilePath, src, ctx.Diagnostics)
	tokens := lex.Tokenize(false)
	mod.AST = parser.Parse(tokens, mod.FilePath, ctx.Diagnostics)

	ctx.AddModule(mod.ImportPath, mod)

	collector.CollectModule(ctx, mod)
	typechecker.TypeCheckTopLevelSignatures(ctx, mod)
	typechecker.CheckModule(ctx, mod)

	hirMod := hirgen.New(ctx, mod).GenerateModule()
	hircfganalyzer.AnalyzeModule(ctx, mod, hirMod)

	return ctx
}

func TestHIRUnreachableAfterReturn(t *testing.T) {
	src := `fn test() -> i32 {
		return 42;
		let x := 10;
	}`

	ctx := analyzeHIR(t, src)
	diags := ctx.Diagnostics.Diagnostics()
	found := false
	for _, diag := range diags {
		if strings.Contains(diag.Message, "unreachable code") {
			found = true
			break
		}
	}

	if !found {
		t.Fatalf("expected unreachable code error, got %d diagnostics", len(diags))
	}
}

func TestHIRMissingReturnInBranch(t *testing.T) {
	src := `fn test(x: i32) -> i32 {
		if x > 0 {
			return 1;
		}
	}`

	ctx := analyzeHIR(t, src)
	diags := ctx.Diagnostics.Diagnostics()
	found := false
	for _, diag := range diags {
		if strings.Contains(diag.Message, "missing return") ||
			strings.Contains(diag.Message, "not all code paths") {
			found = true
			break
		}
	}

	if !found {
		t.Fatalf("expected missing return error, got %d diagnostics", len(diags))
	}
}

func TestHIRAllPathsReturn(t *testing.T) {
	src := `fn test(x: i32) -> i32 {
		if x > 0 {
			return 1;
		} else {
			return -1;
		}
	}`

	ctx := analyzeHIR(t, src)
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "missing return") ||
			strings.Contains(diag.Message, "not all code paths") {
			t.Fatalf("unexpected missing return error: %s", diag.Message)
		}
	}
}

func TestHIRVoidFunctionNoReturn(t *testing.T) {
	src := `fn test() {
		let x := 42;
	}`

	ctx := analyzeHIR(t, src)
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "missing return") ||
			strings.Contains(diag.Message, "not all code paths") {
			t.Fatalf("void function should not require return: %s", diag.Message)
		}
	}
}
