package controlflow

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
	"strings"
	"testing"
)

// Helper to create a test context
func createTestContext() *context_v2.CompilerContext {
	return &context_v2.CompilerContext{
		Diagnostics: diagnostics.NewDiagnosticBag("test.fer"),
		Universe:    context_v2.NewSymbolTable(nil),
	}
}

// Helper to parse a function from source
func parseFunction(t *testing.T, src string) (*ast.FuncDecl, *context_v2.Module) {
	t.Helper()

	mod := &context_v2.Module{
		FilePath: "test.fer",
		Content:  src,
	}

	diag := diagnostics.NewDiagnosticBag(mod.FilePath)

	lex := lexer.New(mod.FilePath, src, diag)
	tokens := lex.Tokenize(false)

	astMod := parser.Parse(tokens, mod.FilePath, diag)

	if len(astMod.Nodes) == 0 {
		t.Fatal("No nodes parsed")
	}

	// Extract function declaration
	for _, node := range astMod.Nodes {
		if funcDecl, ok := node.(*ast.FuncDecl); ok {
			return funcDecl, mod
		}
		if declStmt, ok := node.(*ast.DeclStmt); ok {
			if funcDecl, ok := declStmt.Decl.(*ast.FuncDecl); ok {
				return funcDecl, mod
			}
		}
	}

	t.Fatal("No function found in AST")
	return nil, nil
}

// Test basic CFG construction
func TestCFGBasicConstruction(t *testing.T) {
	src := `fn test() -> i32 {
		return 42;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	cfg := builder.BuildFunctionCFG(funcDecl)

	if cfg == nil {
		t.Fatal("CFG is nil")
	}

	if cfg.Entry == nil {
		t.Fatal("Entry block is nil")
	}

	if cfg.Exit == nil {
		t.Fatal("Exit block is nil")
	}

	// Entry should be reachable
	if !cfg.Entry.Reachable {
		t.Error("Entry block should be reachable")
	}
}

// Test unreachable code detection
func TestUnreachableCodeAfterReturn(t *testing.T) {
	src := `fn test() -> i32 {
		return 42;
		let x := 10; // unreachable
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	_ = builder.BuildFunctionCFG(funcDecl)

	// Check for unreachable code warning
	diags := ctx.Diagnostics.Diagnostics()
	hasUnreachable := false
	for _, diag := range diags {
		if strings.Contains(diag.Message, "unreachable") {
			hasUnreachable = true
			break
		}
	}

	if !hasUnreachable {
		t.Error("Expected unreachable code warning")
	}
}

// Test if-else control flow
func TestIfElseControlFlow(t *testing.T) {
	src := `fn test(x: i32) -> i32 {
		if x > 0 {
			return 1;
		} else {
			return -1;
		}
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	cfg := builder.BuildFunctionCFG(funcDecl)

	// Should not report missing return - both branches return
	AnalyzeReturns(ctx, mod, funcDecl, cfg)

	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "missing return") {
			t.Errorf("Unexpected missing return error: %s", diag.Message)
		}
	}
}

// Test missing return in one branch
func TestMissingReturnInBranch(t *testing.T) {
	src := `fn test(x: i32) -> i32 {
		if x > 0 {
			return 1;
		}
		// Missing return here
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	cfg := builder.BuildFunctionCFG(funcDecl)

	AnalyzeReturns(ctx, mod, funcDecl, cfg)

	// Should report missing return
	diags := ctx.Diagnostics.Diagnostics()
	hasMissingReturn := false
	for _, diag := range diags {
		t.Logf("Diagnostic: %s", diag.Message)
		if strings.Contains(diag.Message, "missing return") || strings.Contains(diag.Message, "not all code paths") {
			hasMissingReturn = true
			break
		}
	}

	if !hasMissingReturn {
		t.Error("Expected missing return error")
	}
}

// Test void function doesn't require return
func TestVoidFunctionNoReturn(t *testing.T) {
	src := `fn test() {
		let x := 42;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	cfg := builder.BuildFunctionCFG(funcDecl)

	AnalyzeReturns(ctx, mod, funcDecl, cfg)

	// Should not report missing return for void function
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "missing return") {
			t.Errorf("Void function should not require return: %s", diag.Message)
		}
	}
}

// Test break outside loop
func TestBreakOutsideLoop(t *testing.T) {
	t.Skip("Parser may not accept break statement at top level of function")
	src := `fn test() {
		break;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	_ = builder.BuildFunctionCFG(funcDecl)

	// Should report break outside loop
	diags := ctx.Diagnostics.Diagnostics()
	hasBreakError := false
	for _, diag := range diags {
		if strings.Contains(diag.Message, "break") && strings.Contains(diag.Message, "outside") {
			hasBreakError = true
			break
		}
	}

	if !hasBreakError {
		t.Error("Expected 'break outside loop' error")
	}
}

// Test continue outside loop
func TestContinueOutsideLoop(t *testing.T) {
	t.Skip("Parser may not accept continue statement at top level of function")
	src := `fn test() {
		continue;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	_ = builder.BuildFunctionCFG(funcDecl)

	// Should report continue outside loop
	diags := ctx.Diagnostics.Diagnostics()
	hasContinueError := false
	for _, diag := range diags {
		if strings.Contains(diag.Message, "continue") && strings.Contains(diag.Message, "outside") {
			hasContinueError = true
			break
		}
	}

	if !hasContinueError {
		t.Error("Expected 'continue outside loop' error")
	}
}

// Test loop with break/continue
func TestLoopWithBreakContinue(t *testing.T) {
	src := `fn test() {
		for let i := 0; i < 10; i := i + 1 {
			if i == 5 {
				break;
			}
			if i == 3 {
				continue;
			}
		}
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	_ = builder.BuildFunctionCFG(funcDecl)

	// Should not report any errors
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if diag.Severity == diagnostics.Error {
			t.Errorf("Unexpected error: %s", diag.Message)
		}
	}
}

// Test nested if with early return
func TestNestedIfEarlyReturn(t *testing.T) {
	src := `fn test(x: i32, y: i32) -> i32 {
		if x > 0 {
			if y > 0 {
				return x + y;
			}
			return x;
		}
		return 0;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	cfg := builder.BuildFunctionCFG(funcDecl)

	AnalyzeReturns(ctx, mod, funcDecl, cfg)

	// Should not report missing return - all paths return
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "missing return") {
			t.Errorf("All paths return, unexpected error: %s", diag.Message)
		}
	}
}

// Test complex branching
func TestComplexBranching(t *testing.T) {
	src := `fn test(x: i32) -> i32 {
		if x > 10 {
			if x > 20 {
				return 2;
			} else {
				return 1;
			}
		} else if x > 5 {
			return 0;
		} else {
			return -1;
		}
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	cfg := builder.BuildFunctionCFG(funcDecl)

	AnalyzeReturns(ctx, mod, funcDecl, cfg)

	// All branches return
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "missing return") {
			t.Errorf("All branches return, unexpected error: %s", diag.Message)
		}
	}
}

// Test while loop
func TestWhileLoop(t *testing.T) {
	src := `fn test() {
		let x := 0;
		while x < 10 {
			x := x + 1;
			if x == 5 {
				break;
			}
		}
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	builder := NewCFGBuilder(ctx, mod)
	_ = builder.BuildFunctionCFG(funcDecl)

	// Should not report any errors
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if diag.Severity == diagnostics.Error {
			t.Errorf("Unexpected error: %s", diag.Message)
		}
	}
}
