package collector

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
	"compiler/internal/semantics/table"
	"strings"
	"testing"
)

// Helper to parse and collect for import alias conflict tests
func parseAndCollectForImportTest(t *testing.T, src string) (*context_v2.Module, *context_v2.CompilerContext) {
	t.Helper()

	ctx := &context_v2.CompilerContext{
		Diagnostics: diagnostics.NewDiagnosticBag("test.fer"),
		Universe:    table.NewSymbolTable(nil),
	}

	mod := &context_v2.Module{
		FilePath:       "test.fer",
		Content:        src,
		ModuleScope:    table.NewSymbolTable(ctx.Universe),
		ImportPath:     "test",
		Imports:        make(map[string]*context_v2.Import),
		ImportAliasMap: make(map[string]string),
	}
	mod.CurrentScope = mod.ModuleScope

	diag := diagnostics.NewDiagnosticBag(mod.FilePath)
	lex := lexer.New(mod.FilePath, src, diag)
	tokens := lex.Tokenize(false)

	astMod := parser.Parse(tokens, mod.FilePath, diag)
	mod.AST = astMod

	CollectModule(ctx, mod)

	return mod, ctx
}

func TestImportAliasConflictWithVariable(t *testing.T) {
	src := `
		import "std/math";
		let math := 42;
	`

	_, ctx := parseAndCollectForImportTest(t, src)

	// Should have one error about import alias conflict
	diags := ctx.Diagnostics.Diagnostics()
	found := false
	for _, d := range diags {
		if d.Severity == diagnostics.Error &&
			strings.Contains(d.Message, "'math' is already used as an import alias") &&
			d.Code == diagnostics.ErrRedeclaredSymbol {
			found = true
			break
		}
	}

	if !found {
		t.Error("Expected error about import alias conflict with variable declaration")
		for _, d := range diags {
			t.Logf("Got: %s [%s]", d.Message, d.Code)
		}
	}
}

func TestImportAliasConflictWithConstant(t *testing.T) {
	src := `
		import "std/math";
		const math := 100;
	`

	_, ctx := parseAndCollectForImportTest(t, src)

	// Should have one error about import alias conflict
	diags := ctx.Diagnostics.Diagnostics()
	found := false
	for _, d := range diags {
		if d.Severity == diagnostics.Error &&
			strings.Contains(d.Message, "'math' is already used as an import alias") &&
			d.Code == diagnostics.ErrRedeclaredSymbol {
			found = true
			break
		}
	}

	if !found {
		t.Error("Expected error about import alias conflict with constant declaration")
		for _, d := range diags {
			t.Logf("Got: %s [%s]", d.Message, d.Code)
		}
	}
}

func TestImportAliasConflictWithFunction(t *testing.T) {
	src := `
		import "std/math";
		fn math() {}
	`

	_, ctx := parseAndCollectForImportTest(t, src)

	// Should have one error about import alias conflict
	diags := ctx.Diagnostics.Diagnostics()
	found := false
	for _, d := range diags {
		if d.Severity == diagnostics.Error &&
			strings.Contains(d.Message, "'math' is already used as an import alias") &&
			d.Code == diagnostics.ErrRedeclaredSymbol {
			found = true
			break
		}
	}

	if !found {
		t.Error("Expected error about import alias conflict with function declaration")
		for _, d := range diags {
			t.Logf("Got: %s [%s]", d.Message, d.Code)
		}
	}
}

func TestImportAliasConflictWithType(t *testing.T) {
	src := `
		import "std/math";
		type math struct { .x: i32; }
	`

	_, ctx := parseAndCollectForImportTest(t, src)

	// Should have one error about import alias conflict
	diags := ctx.Diagnostics.Diagnostics()
	found := false
	for _, d := range diags {
		if d.Severity == diagnostics.Error &&
			strings.Contains(d.Message, "'math' is already used as an import alias") &&
			d.Code == diagnostics.ErrRedeclaredSymbol {
			found = true
			break
		}
	}

	if !found {
		t.Error("Expected error about import alias conflict with type declaration")
		for _, d := range diags {
			t.Logf("Got: %s [%s]", d.Message, d.Code)
		}
	}
}

func TestNoConflictWithDifferentName(t *testing.T) {
	src := `
		import "std/math";
		let x := 42;
		const y := 100;
		fn foo() {}
		type Bar struct { .x: i32; }
	`

	_, ctx := parseAndCollectForImportTest(t, src)

	// Should have no errors about import alias conflicts
	diags := ctx.Diagnostics.Diagnostics()
	for _, d := range diags {
		if d.Severity == diagnostics.Error &&
			strings.Contains(d.Message, "already used as an import alias") {
			t.Errorf("Unexpected error about import alias conflict: %s", d.Message)
		}
	}
}

func TestImportAliasConflictWithCustomAlias(t *testing.T) {
	src := `
		import "std/math" as m;
		let m := 42;
	`

	_, ctx := parseAndCollectForImportTest(t, src)

	// Should have one error about import alias conflict with 'm'
	diags := ctx.Diagnostics.Diagnostics()
	found := false
	for _, d := range diags {
		if d.Severity == diagnostics.Error &&
			strings.Contains(d.Message, "'m' is already used as an import alias") &&
			d.Code == diagnostics.ErrRedeclaredSymbol {
			found = true
			break
		}
	}

	if !found {
		t.Error("Expected error about import alias conflict with custom alias 'm'")
		for _, d := range diags {
			t.Logf("Got: %s [%s]", d.Message, d.Code)
		}
	}
}
