package cfganalyzer

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/controlflow"
)

// AnalyzeModule performs control flow analysis on a module.
// This runs after type checking and validates:
// - All code paths return appropriate values
// - Loops have escape paths (no infinite loops)
// - Unreachable code detection
func AnalyzeModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	if mod.AST == nil {
		return
	}

	// Analyze control flow for all function and method declarations
	for _, node := range mod.AST.Nodes {
		switch decl := node.(type) {
		case *ast.FuncDecl:
			analyzeFuncDecl(ctx, mod, decl)
		case *ast.MethodDecl:
			analyzeMethodDecl(ctx, mod, decl)
		}
	}
}

// analyzeFuncDecl performs CFG analysis on a function declaration
func analyzeFuncDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.FuncDecl) {
	if decl.Body == nil {
		return
	}

	// Build control flow graph for the function
	cfgBuilder := controlflow.NewCFGBuilder(ctx, mod)
	cfg := cfgBuilder.BuildFunctionCFG(decl)

	// Analyze return paths
	controlflow.AnalyzeReturns(ctx, mod, decl, cfg)
}

// analyzeMethodDecl performs CFG analysis on a method declaration
func analyzeMethodDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.MethodDecl) {
	if decl.Body == nil {
		return
	}

	// Convert method to function for CFG analysis
	// (BuildFunctionCFG expects a FuncDecl)
	tempFuncDecl := &ast.FuncDecl{
		Name:     decl.Name,
		Type:     decl.Type,
		Body:     decl.Body,
		Scope:    decl.Scope,
		Location: decl.Location,
	}

	// Build control flow graph for the method
	cfgBuilder := controlflow.NewCFGBuilder(ctx, mod)
	cfg := cfgBuilder.BuildFunctionCFG(tempFuncDecl)

	// Analyze return paths
	controlflow.AnalyzeReturns(ctx, mod, tempFuncDecl, cfg)
}
