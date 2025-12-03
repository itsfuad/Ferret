package resolver

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/table"

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
		// switch to function scope
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		// Bind names in function body
		if n.Body != nil {
			resolveBlock(ctx, mod, n.Body)
		}

	case *ast.AssignStmt:
		resolveExpr(ctx, mod, n.Lhs)
		resolveExpr(ctx, mod, n.Rhs)

	case *ast.ReturnStmt:
		if n.Result != nil {
			resolveExpr(ctx, mod, n.Result)
		}

	case *ast.IfStmt:
		// Enter if scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		resolveExpr(ctx, mod, n.Cond)
		resolveBlock(ctx, mod, n.Body)
		if n.Else != nil {
			resolveNode(ctx, mod, n.Else)
		}

	case *ast.ForStmt:
		// Enter for loop scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

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

	case *ast.WhileStmt:
		// Enter while loop scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		resolveExpr(ctx, mod, n.Cond)
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

	// Enter block scope if it exists
	if block.Scope != nil {
		defer mod.EnterScope(block.Scope.(*table.SymbolTable))()
	}

	for _, node := range block.Nodes {
		resolveNode(ctx, mod, node)
	}
}

// resolveExpr checks identifier references in expressions// resolveExpr checks identifier references in expressions
func resolveExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *ast.IdentifierExpr:
		// Check if the identifier is declared
		name := e.Name
		if _, ok := mod.CurrentScope.Lookup(name); !ok {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("symbol '%s' not found", name)).
					WithPrimaryLabel(&e.Location, "").
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
		resolveStaticAccess(ctx, mod, e)

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

	case *ast.FuncLit:
		// Function literals have their own scope for parameters and body
		if e.Scope != nil {
			defer mod.EnterScope(e.Scope.(*table.SymbolTable))()
		}

		// Resolve function body
		if e.Body != nil {
			resolveNode(ctx, mod, e.Body)
		}

	// Literals don't need binding
	case *ast.BasicLit:
		// No binding needed
	}
}

func resolveStaticAccess(ctx *context_v2.CompilerContext, mod *context_v2.Module, e *ast.ScopeResolutionExpr) {
	// Handle module::symbol resolution
	// X should be an identifier representing the module name/alias
	if ident, ok := e.X.(*ast.IdentifierExpr); ok {
		moduleAlias := ident.Name
		symbolName := e.Selector.Name

		// Look up the import path from the alias/name
		importPath, ok := mod.ImportAliasMap[moduleAlias]
		if !ok {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("module '%s' not imported", moduleAlias)).
					WithPrimaryLabel(ident.Loc(), fmt.Sprintf("'%s' is not an imported module", moduleAlias)).
					WithNote("Make sure to import this module first"),
			)
			return
		}

		// Get the imported module from context
		importedMod, exists := ctx.GetModule(importPath)

		if !exists {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("imported module '%s' not found", importPath)).
					WithPrimaryLabel(ident.Loc(), fmt.Sprintf("module '%s' not loaded", moduleAlias)).
					WithNote("This is likely a compiler bug"),
			)
			return
		}

		// Look up symbol in the imported module's module scope (not CurrentScope)
		// Use ModuleScope to access module-level symbols
		sym, ok := importedMod.ModuleScope.GetSymbol(symbolName)
		if !ok {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("symbol '%s' not found in module '%s'", symbolName, importPath)).
					WithPrimaryLabel(e.Selector.Loc(), fmt.Sprintf("'%s' not found", symbolName)),
			)
			return
		}

		// Check if symbol is exported
		if !sym.Exported {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("symbol '%s' is not exported from module '%s'", symbolName, importPath)).
					WithPrimaryLabel(e.Selector.Loc(), fmt.Sprintf("'%s' is private", symbolName)).
					WithSecondaryLabel(sym.Decl.Loc(), "declared here").
					WithNote("only symbols starting with uppercase letters are exported"),
			)
			return
		}

		mod.Imports[importPath].IsUsed = true

	} else {
		// For now, just resolve the left side (could be enum::variant later)
		resolveExpr(ctx, mod, e.X)
	}
}
