package resolver

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/table"

	"fmt"
)

// ResolveModule resolves imports and binds names for a module.
// It normalizes import paths, detects cycles, and connects modules.
func ResolveModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {

	if mod.AST == nil {
		return
	}

	// Walk the AST and check identifier references
	for _, node := range mod.AST.Nodes {
		resolveNode(ctx, mod, node)
	}

}

// resolveNode recursively binds names in an AST node
func resolveNode(ctx *context_v2.CompilerContext, mod *context_v2.Module, node ast.Node) {
	if node == nil {
		return
	}

	switch n := node.(type) {
	case *ast.VarDecl:
		// Check initializer expressions
		for _, decl := range n.Decls {
			if decl.Value != nil {
				resolveExpr(ctx, mod, decl.Value)
			}
		}
	case *ast.ConstDecl:
		// Check initializer expressions
		for _, decl := range n.Decls {
			if decl.Value != nil {
				resolveExpr(ctx, mod, decl.Value)
			}
		}

	case *ast.FuncDecl:
		// 1) Validate parameters (variadic rules, duplicates, missing types)
		validateFunctionParams(ctx, mod, n)

		sym, ok := mod.Scope.Lookup(n.Name.Name)
		if !ok {
			fmt.Printf("function %s not declared", n.Name.Name)
			return
		}

		funcScope := sym.Scope

		// switch to function scope
		oldScope := mod.Scope
		mod.Scope = funcScope

		for _, param := range n.Type.Params {
			psym := table.Symbol{
				Name: param.Name.Name,
				Kind: table.SymbolParameter,
				Decl: &param,
			}

			err := mod.Scope.Declare(param.Name.Name, &psym)

			if err != nil {
				fmt.Printf("param %s alreary declared", param.Name.Name)
			}
		}

		// 3) Bind names in function body
		if n.Body != nil {
			resolveBlock(ctx, mod, n.Body)
			mod.Scope = oldScope // restore
		}

	case *ast.AssignStmt:
		resolveExpr(ctx, mod, n.Lhs)
		resolveExpr(ctx, mod, n.Rhs)

	case *ast.ReturnStmt:
		if n.Result != nil {
			resolveExpr(ctx, mod, n.Result)
		}

	case *ast.IfStmt:
		resolveExpr(ctx, mod, n.Cond)
		resolveBlock(ctx, mod, n.Body)
		if n.Else != nil {
			resolveNode(ctx, mod, n.Else)
		}

	case *ast.ForStmt:
		if n.Init != nil {
			resolveNode(ctx, mod, n.Init)
		}
		if n.Cond != nil {
			resolveExpr(ctx, mod, n.Cond)
		}
		if n.Post != nil {
			resolveNode(ctx, mod, n.Post)
		}
		resolveBlock(ctx, mod, n.Body)

	case *ast.Block:
		resolveBlock(ctx, mod, n)

	case *ast.DeclStmt:
		resolveNode(ctx, mod, n.Decl)

	case *ast.ExprStmt:
		resolveExpr(ctx, mod, n.X)
	}
}

// resolveBlock binds names in a block
func resolveBlock(ctx *context_v2.CompilerContext, mod *context_v2.Module, block *ast.Block) {
	if block == nil {
		return
	}
	for _, node := range block.Nodes {
		resolveNode(ctx, mod, node)
	}
}

// resolveExpr checks identifier references in expressions
func resolveExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *ast.IdentifierExpr:
		// Check if the identifier is declared
		name := e.Name
		if _, ok := mod.Scope.Lookup(name); !ok {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("symbol '%s' not found", name)).
					WithPrimaryLabel(mod.FilePath, &e.Location, "").
					WithNote("Make sure it's imported or defined"),
			)
		}

	case *ast.BinaryExpr:
		resolveExpr(ctx, mod, e.X)
		resolveExpr(ctx, mod, e.Y)

	case *ast.UnaryExpr:
		resolveExpr(ctx, mod, e.X)

	case *ast.CallExpr:
		resolveExpr(ctx, mod, e.Fun)
		for _, arg := range e.Args {
			resolveExpr(ctx, mod, arg)
		}

	case *ast.IndexExpr:
		resolveExpr(ctx, mod, e.X)
		resolveExpr(ctx, mod, e.Index)

	case *ast.SelectorExpr:
		resolveExpr(ctx, mod, e.X)
		// Sel is the field name, not checked here

	case *ast.ScopeResolutionExpr:
		resolveExpr(ctx, mod, e.X)

	case *ast.CompositeLit:
		for _, elem := range e.Elts {
			if kv, ok := elem.(*ast.KeyValueExpr); ok {
				if kv.Value != nil {
					resolveExpr(ctx, mod, kv.Value)
				}
			} else {
				resolveExpr(ctx, mod, elem)
			}
		}

	case *ast.CastExpr:
		resolveExpr(ctx, mod, e.X)

	case *ast.ElvisExpr:
		resolveExpr(ctx, mod, e.Cond)
		resolveExpr(ctx, mod, e.Default)

	// Literals don't need binding
	case *ast.BasicLit:
		// No binding needed
	}
}

// validateFunctionParams validates function parameters including variadic rules
func validateFunctionParams(ctx *context_v2.CompilerContext, mod *context_v2.Module, funcDecl *ast.FuncDecl) {
	if funcDecl.Type == nil || funcDecl.Type.Params == nil {
		return
	}

	params := funcDecl.Type.Params
	variadicCount := 0
	variadicIndex := -1

	// Check for variadic parameters and their position
	for i, param := range params {
		if param.IsVariadic {
			variadicCount++
			variadicIndex = i
		}
	}

	// Rule: Maximum one variadic parameter
	if variadicCount > 1 {
		for _, param := range params {
			if param.IsVariadic {
				ctx.Diagnostics.Add(
					diagnostics.NewError("multiple variadic parameters").
						WithPrimaryLabel(mod.FilePath, param.Loc(), "variadic parameter here").
						WithHelp("a function can have at most one variadic parameter"),
				)
			}
		}
		return
	}

	// Rule: Variadic parameter must be last
	if variadicCount == 1 && variadicIndex != len(params)-1 {
		ctx.Diagnostics.Add(
			diagnostics.NewError("variadic parameter must be last").
				WithPrimaryLabel(mod.FilePath, params[variadicIndex].Loc(), "variadic parameter not at end").
				WithHelp(fmt.Sprintf("move this parameter to position %d (last)", len(params))),
		)
	}

	// Check for duplicate parameter names
	paramNames := make(map[string]*ast.Field)
	for _, param := range params {
		if param.Name != nil {
			name := param.Name.Name
			if existing, ok := paramNames[name]; ok {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("duplicate parameter name '%s'", name)).
						WithPrimaryLabel(mod.FilePath, param.Loc(), fmt.Sprintf("'%s' already declared", name)).
						WithSecondaryLabel(mod.FilePath, existing.Loc(), "previous declaration here"),
				)
			} else {
				paramNames[name] = &param
			}
		}
	}

	// Check for missing parameter types
	for _, param := range params {
		if param.Type == nil {
			paramName := "<unnamed>"
			if param.Name != nil {
				paramName = param.Name.Name
			}
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("parameter '%s' missing type annotation", paramName)).
					WithPrimaryLabel(mod.FilePath, param.Loc(), "type annotation required").
					WithHelp("add type annotation (e.g., 'name: i32')"),
			)
		}
	}
}
