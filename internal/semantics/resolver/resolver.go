package resolver

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"fmt"
)

// ResolveModule resolves imports and binds names for a module.
// It normalizes import paths, detects cycles, and connects modules.
func ResolveModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	// Resolve each import
	for _, imp := range mod.Imports {
		resolveImport(ctx, mod, imp)
	}

	// Bind identifiers to symbols (for now, we'll do a basic pass)
	// More sophisticated name binding can be added later
	bindNames(ctx, mod)
}

// resolveImport resolves a single import statement
func resolveImport(ctx *context_v2.CompilerContext, mod *context_v2.Module, imp *context_v2.Import) {
	// Normalize the import path
	resolvedPath := ctx.NormalizeImportPath(imp.Path)
	imp.ResolvedPath = resolvedPath

	// Add dependency to graph for cycle detection
	err := ctx.AddDependency(mod.ImportPath, resolvedPath)
	if err != nil {
		ctx.Diagnostics.Add(
			diagnostics.NewError(err.Error()).
				WithPrimaryLabel(mod.FilePath, imp.Location, fmt.Sprintf("imports '%s'", imp.Path)),
		)
		return
	}

	// Get the imported module (it should be parsed by now)
	_, exists := ctx.GetModule(resolvedPath)
	if !exists {
		// Module hasn't been parsed yet - this is handled by the pipeline
		// which ensures dependencies are parsed before resolution
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("cannot find module '%s'", imp.Path)).
				WithPrimaryLabel(mod.FilePath, imp.Location, "imported here").
				WithHelp(fmt.Sprintf("ensure module '%s' exists and is accessible", resolvedPath)),
		)
		return
	}

	// Check if the imported module has been collected
	// (symbols should be available)
	phase := ctx.GetModulePhase(resolvedPath)
	if phase < context_v2.PhaseCollected {
		// This shouldn't happen if the pipeline is correct
		ctx.ReportError(fmt.Sprintf("internal error: trying to resolve import '%s' before it's collected", resolvedPath), nil)
		return
	}

	// Import is now resolved - the module can be looked up via ctx.GetModule(imp.ResolvedPath)
	// The imported module's symbol table is accessible via that module's Symbols field
}

// bindNames creates a mapping from identifier expressions to symbols
// This is a basic implementation - more sophisticated binding can be added later
func bindNames(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	// For now, we'll just verify that all used identifiers are declared
	// More sophisticated binding (e.g., creating a Bindings map) can be added later

	if mod.AST == nil {
		return
	}

	// Walk the AST and check identifier references
	for _, node := range mod.AST.Nodes {
		bindNamesInNode(ctx, mod, node)
	}
}

// bindNamesInNode recursively binds names in an AST node
func bindNamesInNode(ctx *context_v2.CompilerContext, mod *context_v2.Module, node ast.Node) {
	if node == nil {
		return
	}

	switch n := node.(type) {
	case *ast.VarDecl:
		// Check initializer expressions
		for _, decl := range n.Decls {
			if decl.Value != nil {
				bindNamesInExpr(ctx, mod, decl.Value)
			}
		}
	case *ast.ConstDecl:
		// Check initializer expressions
		for _, decl := range n.Decls {
			if decl.Value != nil {
				bindNamesInExpr(ctx, mod, decl.Value)
			}
		}
	case *ast.FuncDecl:
		// Bind names in function body
		if n.Body != nil {
			bindNamesInBlock(ctx, mod, n.Body)
		}
	case *ast.AssignStmt:
		bindNamesInExpr(ctx, mod, n.Lhs)
		bindNamesInExpr(ctx, mod, n.Rhs)
	case *ast.ReturnStmt:
		if n.Result != nil {
			bindNamesInExpr(ctx, mod, n.Result)
		}
	case *ast.IfStmt:
		bindNamesInExpr(ctx, mod, n.Cond)
		bindNamesInBlock(ctx, mod, n.Body)
		if n.Else != nil {
			bindNamesInNode(ctx, mod, n.Else)
		}
	case *ast.ForStmt:
		if n.Init != nil {
			bindNamesInNode(ctx, mod, n.Init)
		}
		if n.Cond != nil {
			bindNamesInExpr(ctx, mod, n.Cond)
		}
		if n.Post != nil {
			bindNamesInNode(ctx, mod, n.Post)
		}
		bindNamesInBlock(ctx, mod, n.Body)
	case *ast.Block:
		bindNamesInBlock(ctx, mod, n)
	case *ast.DeclStmt:
		bindNamesInNode(ctx, mod, n.Decl)
	case *ast.ExprStmt:
		bindNamesInExpr(ctx, mod, n.X)
	}
}

// bindNamesInBlock binds names in a block
func bindNamesInBlock(ctx *context_v2.CompilerContext, mod *context_v2.Module, block *ast.Block) {
	if block == nil {
		return
	}
	for _, node := range block.Nodes {
		bindNamesInNode(ctx, mod, node)
	}
}

// bindNamesInExpr checks identifier references in expressions
func bindNamesInExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *ast.IdentifierExpr:
		// Check if the identifier is declared
		name := e.Name
		if _, ok := mod.Symbols.Lookup(name); !ok {
			// Not found in module scope - this might be an error or it might be
			// from an imported module. For now, we'll defer this check to the type checker.
			// A more sophisticated implementation would check imported modules here.
		}
	case *ast.BinaryExpr:
		bindNamesInExpr(ctx, mod, e.X)
		bindNamesInExpr(ctx, mod, e.Y)
	case *ast.UnaryExpr:
		bindNamesInExpr(ctx, mod, e.X)
	case *ast.CallExpr:
		bindNamesInExpr(ctx, mod, e.Fun)
		for _, arg := range e.Args {
			bindNamesInExpr(ctx, mod, arg)
		}
	case *ast.IndexExpr:
		bindNamesInExpr(ctx, mod, e.X)
		bindNamesInExpr(ctx, mod, e.Index)
	case *ast.SelectorExpr:
		bindNamesInExpr(ctx, mod, e.X)
		// Sel is the field name, not checked here
	case *ast.CompositeLit:
		for _, elem := range e.Elts {
			if kv, ok := elem.(*ast.KeyValueExpr); ok {
				if kv.Value != nil {
					bindNamesInExpr(ctx, mod, kv.Value)
				}
			} else {
				bindNamesInExpr(ctx, mod, elem)
			}
		}
	case *ast.CastExpr:
		bindNamesInExpr(ctx, mod, e.X)
	case *ast.ElvisExpr:
		bindNamesInExpr(ctx, mod, e.Cond)
		bindNamesInExpr(ctx, mod, e.Default)
	// Literals don't need binding
	case *ast.BasicLit:
		// No binding needed
	}
}
