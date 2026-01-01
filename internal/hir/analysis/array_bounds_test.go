package analysis_test

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
	"compiler/internal/hir/analysis"
	"compiler/internal/hir/gen"
	"compiler/internal/semantics/collector"
	"compiler/internal/semantics/table"
	"compiler/internal/semantics/typechecker"
	"strings"
	"testing"
)

// Helper to create a test context
func createArrayTestContext() *context_v2.CompilerContext {
	config := &context_v2.Config{
		ProjectName: "test",
		ProjectRoot: "/tmp/test",
		Extension:   ".fer",
	}
	return context_v2.New(config, false)
}

// Helper to parse, collect, type-check, and analyze HIR
func parseCollectAndCheck(t *testing.T, src string) (*context_v2.Module, *context_v2.CompilerContext) {
	t.Helper()

	ctx := createArrayTestContext()
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

	ctx.AddModule("test", mod)

	// Run collector
	collector.CollectModule(ctx, mod)

	// Run signature/type checking
	typechecker.TypeCheckTopLevelSignatures(ctx, mod)

	// Run type checker
	typechecker.CheckModule(ctx, mod)

	// Run HIR generation + CFG analysis (const eval now happens on HIR)
	hirMod := gen.New(ctx, mod).GenerateModule()
	analysis.AnalyzeModule(ctx, mod, hirMod)

	return mod, ctx
}

func TestFixedArrayBoundsChecking(t *testing.T) {
	tests := []struct {
		name          string
		src           string
		expectError   bool
		errorContains string
	}{
		{
			name: "Valid array access within bounds",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let x := arr[0];
				let y := arr[4];
			}`,
			expectError: false,
		},
		{
			name: "Out of bounds positive index",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let x := arr[5];
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "Out of bounds large positive index",
			src: `fn test() {
				let arr: [3]i32 = [1, 2, 3];
				let x := arr[10];
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "Valid negative index",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let x := arr[-1];
				let y := arr[-5];
			}`,
			expectError: false,
		},
		{
			name: "Out of bounds negative index",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let x := arr[-6];
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "Runtime index - error",
			src: `fn test(i: i32) {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let x := arr[i];
			}`,
			expectError:   true,
			errorContains: "compile-time constant",
		},
		{
			name: "Dynamic array with known length - bounds checking",
			src: `fn test() {
				let arr: []i32 = [1, 2, 3];
				let x := arr[10];
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "Dynamic array with unknown length - no bounds checking",
			src: `fn test(arr: []i32) {
				let x := arr[10];
				let y := arr[100];
			}`,
			expectError: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, ctx := parseCollectAndCheck(t, tt.src)

			diags := ctx.Diagnostics.Diagnostics()
			hasError := false
			foundExpectedError := false

			for _, d := range diags {
				if d.Severity == diagnostics.Error {
					hasError = true
					if tt.errorContains != "" && strings.Contains(d.Message, tt.errorContains) {
						foundExpectedError = true
					}
					t.Logf("Error: %s", d.Message)
				}
			}

			if tt.expectError && !hasError {
				t.Errorf("Expected error but got none")
			}

			if !tt.expectError && hasError {
				t.Errorf("Expected no error but got:")
				for _, d := range diags {
					if d.Severity == diagnostics.Error {
						t.Logf("  - %s", d.Message)
					}
				}
			}

			if tt.expectError && tt.errorContains != "" && !foundExpectedError {
				t.Errorf("Expected error containing %q, but didn't find it", tt.errorContains)
				t.Log("Got errors:")
				for _, d := range diags {
					if d.Severity == diagnostics.Error {
						t.Logf("  - %s", d.Message)
					}
				}
			}
		})
	}
}

func TestArrayBoundsErrorCode(t *testing.T) {
	src := `fn test() {
		let arr: [3]i32 = [1, 2, 3];
		let x := arr[5];
	}`

	_, ctx := parseCollectAndCheck(t, src)

	diags := ctx.Diagnostics.Diagnostics()
	found := false

	for _, d := range diags {
		if d.Severity == diagnostics.Error && strings.Contains(d.Message, "array index out of bounds") {
			found = true
			if d.Code != diagnostics.ErrArrayOutOfBounds {
				t.Errorf("Expected error code %s, got %s", diagnostics.ErrArrayOutOfBounds, d.Code)
			}
			break
		}
	}

	if !found {
		t.Error("Did not find array bounds error")
	}
}
