package collector

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/table"
	"compiler/internal/types"
	"fmt"
)

// CollectModule builds the symbol table for a module by traversing its AST.
// It declares names for let/const/fn/type declarations and fills Module.Symbols.
// No type checking is performed - all symbols get TYPE_UNKNOWN initially.
func CollectModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	// Initialize module symbol table if not exist
	if mod.Scope == nil {
		mod.Scope = table.NewSymbolTable(ctx.Universe)
	}

	// Initialize imports list if not exist
	if mod.Imports == nil {
		mod.Imports = make([]*context_v2.Import, 0)
	}

	// Traverse the AST and collect declarations
	if mod.AST != nil {
		for _, node := range mod.AST.Nodes {
			collectNode(ctx, mod, node)
		}
	}
}

// collectNode processes a single AST node and declares symbols
func collectNode(ctx *context_v2.CompilerContext, mod *context_v2.Module, node ast.Node) {
	switch n := node.(type) {
	case *ast.VarDecl:
		collectVarDecl(ctx, mod, n, table.SymbolVariable)
	case *ast.ConstDecl:
		collectVarDecl(ctx, mod, n, table.SymbolConstant)
	case *ast.FuncDecl:
		collectFuncDecl(ctx, mod, n)
	case *ast.TypeDecl:
		collectTypeDecl(ctx, mod, n)
	case *ast.ImportStmt:
		collectImport(mod, n)
	case *ast.DeclStmt:
		// DeclStmt wraps a declaration, recurse into it
		collectNode(ctx, mod, n.Decl)
	// Other statements don't declare module-level symbols
	default:
		// Skip non-declaration nodes
	}
}

// collectVarDecl handles variable and constant declarations
func collectVarDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl interface{}, kind table.SymbolKind) {
	var declItems []ast.DeclItem

	switch d := decl.(type) {
	case *ast.VarDecl:
		declItems = d.Decls
	case *ast.ConstDecl:
		declItems = d.Decls
	default:
		return
	}

	for _, item := range declItems {
		name := item.Name.Name

		// Check for duplicate declaration
		if existing, ok := mod.Scope.Lookup(name); ok {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("redeclaration of '%s'", name)).
					WithPrimaryLabel(mod.FilePath, &item.Name.Location, fmt.Sprintf("'%s' already declared", name)).
					WithSecondaryLabel(mod.FilePath, existing.Decl.Loc(), "previous declaration here"),
			)
			continue
		}

		// Create symbol with TYPE_UNKNOWN (will be filled during type checking)
		sym := &table.Symbol{
			Name:     name,
			Kind:     kind,
			Type:     types.TypeUnknown,
			Exported: isExported(name),
			Decl:     item.Name,
		}

		mod.Scope.Declare(name, sym)
	}
}

// collectFuncDecl handles function declarations
func collectFuncDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.FuncDecl) {

	if decl.Name == nil {
		return
	}

	name := decl.Name.Name

	// Check for duplicate declaration
	if existing, ok := mod.Scope.Lookup(name); ok {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("redeclaration of '%s'", name)).
				WithPrimaryLabel(mod.FilePath, decl.Loc(), fmt.Sprintf("'%s' already declared", name)).
				WithSecondaryLabel(mod.FilePath, existing.Decl.Loc(), "previous declaration here"),
		)
		return
	}

	// Create symbol (type will be filled during type checking)
	sym := &table.Symbol{
		Name:     name,
		Kind:     table.SymbolFunction,
		Type:     types.NewFunction([]types.ParamType{}, types.TypeVoid), // Placeholder, will be filled during type checking
		Exported: isExported(name),
		Decl:     decl,
		Scope:    table.NewSymbolTable(mod.Scope),
	}

	mod.Scope.Declare(name, sym)
}

// collectTypeDecl handles type declarations
func collectTypeDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.TypeDecl) {
	name := decl.Name.Name

	// Check for duplicate declaration
	if existing, ok := mod.Scope.Lookup(name); ok {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("redeclaration of '%s'", name)).
				WithPrimaryLabel(mod.FilePath, decl.Loc(), fmt.Sprintf("'%s' already declared", name)).
				WithSecondaryLabel(mod.FilePath, existing.Decl.Loc(), "previous declaration here"),
		)
		return
	}

	// Create symbol
	sym := &table.Symbol{
		Name:     name,
		Kind:     table.SymbolType,
		Type:     types.TypeUnknown, // The actual type will be resolved later
		Exported: isExported(name),
		Decl:     decl,
	}

	mod.Scope.Declare(name, sym)
}

// collectImport handles import statements
func collectImport(mod *context_v2.Module, stmt *ast.ImportStmt) {
	// BasicLit has a Value field containing the string value
	path := stmt.Path.Value
	alias := ""
	if stmt.Alias != nil {
		alias = stmt.Alias.Name
	}

	// Create import entry
	imp := &context_v2.Import{
		Path:     path,
		Alias:    alias,
		Location: stmt.Loc(),
	}

	mod.Imports = append(mod.Imports, imp)
}

// isExported checks if a name is exported (starts with uppercase letter in Ferret)
func isExported(name string) bool {
	if len(name) == 0 {
		return false
	}
	// In Ferret, exported names start with uppercase letters
	firstChar := rune(name[0])
	return firstChar >= 'A' && firstChar <= 'Z'
}
