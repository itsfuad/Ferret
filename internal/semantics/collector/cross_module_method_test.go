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

// TestCrossModuleMethodRestriction verifies that methods cannot be defined on types from other modules
func TestCrossModuleMethodRestriction(t *testing.T) {
	// Module A defines a type
	moduleAContent := `
type Point struct {
	.x: i32,
	.y: i32
};
`

	// Module B tries to define a method on Point from module A
	moduleBContent := `
import "test/module_a";

// This should be rejected - defining method on Point from module_a
fn (p: module_a::Point) distance() -> f64 {
	let dx := p.x * p.x;
	let dy := p.y * p.y;
	return 0.0;
}
`

	// Create context
	config := &context_v2.Config{
		ProjectName: "test",
		ProjectRoot: "/tmp/test",
		Extension:   ".fer",
	}
	ctx := context_v2.New(config, false)

	// Parse and collect module A
	modA := &context_v2.Module{
		FilePath:       "test/module_a.fer",
		Content:        moduleAContent,
		ModuleScope:    table.NewSymbolTable(ctx.Universe),
		ImportPath:     "test/module_a",
		Imports:        make(map[string]*context_v2.Import),
		ImportAliasMap: make(map[string]string),
	}
	modA.CurrentScope = modA.ModuleScope

	diagA := diagnostics.NewDiagnosticBag(modA.FilePath)
	lexerA := lexer.New(modA.FilePath, moduleAContent, diagA)
	tokensA := lexerA.Tokenize(false)
	astA := parser.Parse(tokensA, modA.FilePath, diagA)
	modA.AST = astA

	ctx.AddModule("test/module_a", modA)
	CollectModule(ctx, modA)

	// Parse and collect module B
	modB := &context_v2.Module{
		FilePath:       "test/module_b.fer",
		Content:        moduleBContent,
		ModuleScope:    table.NewSymbolTable(ctx.Universe),
		ImportPath:     "test/module_b",
		Imports:        make(map[string]*context_v2.Import),
		ImportAliasMap: make(map[string]string),
	}
	modB.CurrentScope = modB.ModuleScope

	diagB := diagnostics.NewDiagnosticBag(modB.FilePath)
	lexerB := lexer.New(modB.FilePath, moduleBContent, diagB)
	tokensB := lexerB.Tokenize(false)
	astB := parser.Parse(tokensB, modB.FilePath, diagB)
	modB.AST = astB

	ctx.AddModule("test/module_b", modB)
	CollectModule(ctx, modB)

	// Check for errors
	diags := ctx.Diagnostics.Diagnostics()

	// Should have the cross-module method error
	foundCrossModuleError := false
	for _, d := range diags {
		if d.Severity == diagnostics.Error && strings.Contains(d.Message, "cannot define methods on types from other modules") {
			foundCrossModuleError = true

			// Verify diagnostic structure
			if d.Code != diagnostics.ErrInvalidMethodReceiver {
				t.Errorf("Expected code %s, got %s", diagnostics.ErrInvalidMethodReceiver, d.Code)
			}

			if len(d.Labels) == 0 {
				t.Error("Expected diagnostic to have labels")
			}

			if len(d.Help) == 0 {
				t.Error("Expected diagnostic to have help text")
			}

			if !strings.Contains(d.Help, "same module") {
				t.Errorf("Expected help text to mention 'same module', got: %s", d.Help)
			}

			if len(d.Notes) == 0 {
				t.Error("Expected diagnostic to have notes")
			}
			break
		}
	}

	if !foundCrossModuleError {
		t.Error("Expected cross-module method error, but didn't find it")
		t.Log("Got errors:")
		for _, d := range diags {
			if d.Severity == diagnostics.Error {
				t.Logf("  - %s", d.Message)
			}
		}
	}

	// Verify that the method body was still parsed (AST should contain the method node)
	if astB == nil || len(astB.Nodes) == 0 {
		t.Error("Expected method to be parsed, but AST is empty")
	}
}

// TestLocalMethodAllowed verifies that methods on local types are still allowed
func TestLocalMethodAllowed(t *testing.T) {
	moduleContent := `
type Point struct {
	.x: i32,
	.y: i32
};

// This should work - defining method on local type
fn (p: Point) distance() -> f64 {
	return 0.0;
}
`

	// Create context
	config := &context_v2.Config{
		ProjectName: "test",
		ProjectRoot: "/tmp/test",
		Extension:   ".fer",
	}
	ctx := context_v2.New(config, false)

	mod := &context_v2.Module{
		FilePath:       "test/module.fer",
		Content:        moduleContent,
		ModuleScope:    table.NewSymbolTable(ctx.Universe),
		ImportPath:     "test/module",
		Imports:        make(map[string]*context_v2.Import),
		ImportAliasMap: make(map[string]string),
	}
	mod.CurrentScope = mod.ModuleScope

	diag := diagnostics.NewDiagnosticBag(mod.FilePath)
	lex := lexer.New(mod.FilePath, moduleContent, diag)
	tokens := lex.Tokenize(false)
	ast := parser.Parse(tokens, mod.FilePath, diag)
	mod.AST = ast

	ctx.AddModule("test/module", mod)
	CollectModule(ctx, mod)

	// Should have no errors about cross-module methods
	diags := ctx.Diagnostics.Diagnostics()
	for _, d := range diags {
		if d.Severity == diagnostics.Error && strings.Contains(d.Message, "cannot define methods on types from other modules") {
			t.Errorf("Expected no cross-module error for local method, but got: %s", d.Message)
		}
	}
}
