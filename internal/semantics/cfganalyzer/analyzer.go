package cfganalyzer

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/consteval"
	"compiler/internal/semantics/controlflow"
	"compiler/internal/semantics/table"
)

// AnalyzeModule performs control flow analysis on a module.
// This runs after type checking and validates:
// - All code paths return appropriate values
// - Loops have escape paths (no infinite loops)
// - Unreachable code detection
// - Dead code detection (constant conditions)
func AnalyzeModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	if mod.AST == nil {
		return
	}

	// First, check for constant conditions (dead code detection)
	checkConstantConditions(ctx, mod)

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

// checkConstantConditions checks for constant conditions in if statements and while loops
// This detects dead code (branches that never execute)
func checkConstantConditions(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	if mod.AST == nil {
		return
	}

	for _, node := range mod.AST.Nodes {
		checkNodeForConstantConditions(ctx, mod, node)
	}
}

// checkNodeForConstantConditions recursively checks nodes for constant conditions
func checkNodeForConstantConditions(ctx *context_v2.CompilerContext, mod *context_v2.Module, node ast.Node) {
	if node == nil {
		return
	}

	switch n := node.(type) {
	case *ast.FuncDecl:
		if n.Scope != nil {
			if scope, ok := n.Scope.(*table.SymbolTable); ok {
				defer mod.EnterScope(scope)()
			}
		}
		if n.Body != nil {
			checkBlockForConstantConditions(ctx, mod, n.Body)
		}
	case *ast.MethodDecl:
		if n.Scope != nil {
			if scope, ok := n.Scope.(*table.SymbolTable); ok {
				defer mod.EnterScope(scope)()
			}
		}
		if n.Body != nil {
			checkBlockForConstantConditions(ctx, mod, n.Body)
		}
	case *ast.Block:
		checkBlockForConstantConditions(ctx, mod, n)
	}
}

// checkBlockForConstantConditions checks a block for constant conditions
func checkBlockForConstantConditions(ctx *context_v2.CompilerContext, mod *context_v2.Module, block *ast.Block) {
	if block == nil {
		return
	}

	if block.Scope != nil {
		if scope, ok := block.Scope.(*table.SymbolTable); ok {
			defer mod.EnterScope(scope)()
		}
	}

	for _, node := range block.Nodes {
		switch n := node.(type) {
		case *ast.IfStmt:
			checkIfStmtConstantCondition(ctx, mod, n)
			// Recursively check nested blocks
			if n.Body != nil {
				checkBlockForConstantConditions(ctx, mod, n.Body)
			}
			if n.Else != nil {
				if elseBlock, ok := n.Else.(*ast.Block); ok {
					checkBlockForConstantConditions(ctx, mod, elseBlock)
				} else if elseIf, ok := n.Else.(*ast.IfStmt); ok {
					checkIfStmtConstantCondition(ctx, mod, elseIf)
					if elseIf.Body != nil {
						checkBlockForConstantConditions(ctx, mod, elseIf.Body)
					}
					if elseIf.Else != nil {
						if elseBlock, ok := elseIf.Else.(*ast.Block); ok {
							checkBlockForConstantConditions(ctx, mod, elseBlock)
						}
					}
				}
			}
		case *ast.WhileStmt:
			checkWhileStmtConstantCondition(ctx, mod, n)
			if n.Body != nil {
				checkBlockForConstantConditions(ctx, mod, n.Body)
			}
		case *ast.ForStmt:
			if n.Body != nil {
				checkBlockForConstantConditions(ctx, mod, n.Body)
			}
		case *ast.Block:
			checkBlockForConstantConditions(ctx, mod, n)
		}
	}
}

// checkIfStmtConstantCondition checks if an if statement has a constant condition
func checkIfStmtConstantCondition(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.IfStmt) {
	if stmt.Cond == nil {
		return
	}

	// Evaluate condition as constant
	if constVal := consteval.EvaluateExpr(ctx, mod, stmt.Cond); constVal != nil {
		if boolVal, ok := constVal.AsBool(); ok {
			if boolVal {
				// Condition is always true - else branch is dead code
				if stmt.Else != nil {
					ctx.Diagnostics.Add(
						diagnostics.NewWarning("condition is always true").
							WithCode(diagnostics.WarnConstantConditionTrue).
							WithPrimaryLabel(stmt.Cond.Loc(), "this condition is always true").
							WithSecondaryLabel(stmt.Else.Loc(), "this branch will never execute").
							WithHelp("remove the if statement or the unreachable else branch"),
					)
				}
			} else {
				// Condition is always false - then branch is dead code
				ctx.Diagnostics.Add(
					diagnostics.NewWarning("condition is always false").
						WithCode(diagnostics.WarnConstantConditionFalse).
						WithPrimaryLabel(stmt.Cond.Loc(), "this condition is always false").
						WithSecondaryLabel(stmt.Body.Loc(), "this branch will never execute").
						WithHelp("remove the if statement or fix the condition"),
				)
			}
		}
	}
}

// checkWhileStmtConstantCondition checks if a while statement has a constant condition
func checkWhileStmtConstantCondition(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.WhileStmt) {
	if stmt.Cond == nil {
		return
	}

	// Evaluate condition as constant
	if constVal := consteval.EvaluateExpr(ctx, mod, stmt.Cond); constVal != nil {
		if boolVal, ok := constVal.AsBool(); ok {
			// Only warn if condition is always false - loop never executes
			// Always-true conditions are often intentional (infinite loops with break)
			if !boolVal {
				ctx.Diagnostics.Add(
					diagnostics.NewWarning("condition is always false").
						WithCode(diagnostics.WarnConstantConditionFalse).
						WithPrimaryLabel(stmt.Cond.Loc(), "this condition is always false").
						WithSecondaryLabel(stmt.Body.Loc(), "this loop will never execute").
						WithHelp("remove the while loop or fix the condition"),
				)
			}
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
