package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
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

	lex := lexer.New(mod.FilePath, src)
	tokens := lex.Tokenize(false)
	if len(lex.Errors) > 0 {
		t.Fatalf("Lexer errors: %v", lex.Errors)
	}

	diag := diagnostics.NewDiagnosticBag(mod.FilePath)
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
