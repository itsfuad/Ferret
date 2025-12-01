package collector

import (
	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/table"
	"compiler/internal/types"
	"fmt"
	"strings"
)

// CollectModule builds the symbol table for a module by traversing its AST.
// It declares names for let/const/fn/type declarations and fills Module.Symbols.
// No type checking is performed - all symbols get TYPE_UNKNOWN initially.
func CollectModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	// Initialize module symbol table if not exist
	if mod.CurrentScope == nil {
		mod.CurrentScope = table.NewSymbolTable(ctx.Universe)
	}

	// Initialize imports list if not exist
	if mod.Imports == nil {
		mod.Imports = make([]*context_v2.Import, 0)
	}

	// Initialize import alias map
	if mod.ImportAliasMap == nil {
		mod.ImportAliasMap = make(map[string]string)
	}

	// Traverse the AST and collect declarations
	if mod.AST != nil {
		for _, node := range mod.AST.Nodes {
			collectNode(ctx, mod, node)
		}
	}

	// Build import alias map after collecting all imports
	buildImportAliasMap(ctx, mod)
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
		collectNode(ctx, mod, n.Decl)
	case *ast.Block:
		collectBlock(ctx, mod, n)
	case *ast.IfStmt:
		collectIfStmt(ctx, mod, n)
	case *ast.ForStmt:
		collectForStmt(ctx, mod, n)
	case *ast.WhileStmt:
		collectWhileStmt(ctx, mod, n)
	// Other statements don't declare module-level symbols
	default:
		// Skip non-declaration nodes
	}
}

func collectBlock(ctx *context_v2.CompilerContext, mod *context_v2.Module, block *ast.Block) {
	if ctx.Debug {
		colors.BROWN.Println("Entering new scope")
	}

	// Create new block scope with current scope as parent
	blockScope := table.NewSymbolTable(mod.CurrentScope)
	block.Scope = blockScope

	// Enter new block scope
	oldScope := mod.CurrentScope
	mod.CurrentScope = blockScope

	defer func() {
		mod.CurrentScope = oldScope
		if ctx.Debug {
			colors.BROWN.Println("Exiting new scope...")
		}
	}()

	for _, n := range block.Nodes {
		collectNode(ctx, mod, n)
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
		if existing, ok := mod.CurrentScope.GetSymbol(name); ok {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("redeclaration of '%s'", name)).
					WithCode("C-REDEC").
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

		mod.CurrentScope.Declare(name, sym)

		// Collect any function literals in the initializer expression
		if item.Value != nil {
			collectExpr(ctx, mod, item.Value)
		}
	}
}

// collectFuncDecl handles function declarations
func collectFuncDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.FuncDecl) {

	if decl.Name == nil {
		return
	}

	name := decl.Name.Name

	// Check for duplicate declaration
	if existing, ok := mod.CurrentScope.GetSymbol(name); ok {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("redeclaration of '%s'", name)).
				WithCode("C-REDEC-FUNC").
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
	}

	mod.CurrentScope.Declare(name, sym)

	// Collect function scope and body using shared helper
	collectFunctionScope(ctx, mod, decl.Type, decl.Body, &decl.Scope)
}

// validateParams validates function parameters including variadic rules, duplicates, and type annotations
// This is shared by both named functions and function literals
func validateParams(ctx *context_v2.CompilerContext, mod *context_v2.Module, params []ast.Field) {
	if len(params) == 0 {
		return
	}

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

// collectTypeDecl handles type declarations
func collectTypeDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.TypeDecl) {
	name := decl.Name.Name

	// Check for duplicate declaration
	if existing, ok := mod.CurrentScope.GetSymbol(name); ok {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("redeclaration of '%s'", name)).
				WithCode("C-REDEC-TYPE").
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

	mod.CurrentScope.Declare(name, sym)
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

// collectIfStmt handles if statements and their scopes
func collectIfStmt(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.IfStmt) {
	// Create scope for if statement
	ifScope := table.NewSymbolTable(mod.CurrentScope)
	stmt.Scope = ifScope

	oldScope := mod.CurrentScope
	mod.CurrentScope = ifScope
	defer func() {
		mod.CurrentScope = oldScope
	}()

	// Collect symbols in the if body
	if stmt.Body != nil {
		collectNode(ctx, mod, stmt.Body)
	}

	// Restore scope before processing else
	mod.CurrentScope = oldScope

	// Process else branch (which may be another IfStmt or Block)
	if stmt.Else != nil {
		collectNode(ctx, mod, stmt.Else)
	}
}

// collectForStmt handles for loops and their scopes
func collectForStmt(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.ForStmt) {
	// Create scope for for loop
	forScope := table.NewSymbolTable(mod.CurrentScope)
	stmt.Scope = forScope

	oldScope := mod.CurrentScope
	mod.CurrentScope = forScope
	defer func() {
		mod.CurrentScope = oldScope
	}()

	// Collect symbols in the for body
	if stmt.Body != nil {
		collectNode(ctx, mod, stmt.Body)
	}
}

// collectWhileStmt handles while loops and their scopes
func collectWhileStmt(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.WhileStmt) {
	// Create scope for while loop
	whileScope := table.NewSymbolTable(mod.CurrentScope)
	stmt.Scope = whileScope

	oldScope := mod.CurrentScope
	mod.CurrentScope = whileScope
	defer func() {
		mod.CurrentScope = oldScope
	}()

	// Collect symbols in the while body
	if stmt.Body != nil {
		collectNode(ctx, mod, stmt.Body)
	}
}

// collectExpr recursively walks expressions to find and collect function literals
func collectExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression) {
	if expr == nil {
		return
	}

	switch e := expr.(type) {
	case *ast.FuncLit:
		collectFuncLit(ctx, mod, e)

	case *ast.BinaryExpr:
		collectExpr(ctx, mod, e.X)
		collectExpr(ctx, mod, e.Y)

	case *ast.UnaryExpr:
		collectExpr(ctx, mod, e.X)

	case *ast.CallExpr:
		collectExpr(ctx, mod, e.Fun)
		for _, arg := range e.Args {
			collectExpr(ctx, mod, arg)
		}

	case *ast.IndexExpr:
		collectExpr(ctx, mod, e.X)
		collectExpr(ctx, mod, e.Index)

	case *ast.SelectorExpr:
		collectExpr(ctx, mod, e.X)

	case *ast.ScopeResolutionExpr:
		collectExpr(ctx, mod, e.X)

	case *ast.CompositeLit:
		for _, elem := range e.Elts {
			if kv, ok := elem.(*ast.KeyValueExpr); ok {
				if kv.Value != nil {
					collectExpr(ctx, mod, kv.Value)
				}
			} else {
				collectExpr(ctx, mod, elem)
			}
		}

	case *ast.CastExpr:
		collectExpr(ctx, mod, e.X)

	case *ast.ElvisExpr:
		collectExpr(ctx, mod, e.Cond)
		collectExpr(ctx, mod, e.Default)

	// Literals and identifiers don't need collection
	default:
		// Skip
	}
}

// collectFuncLit handles function literal collection
// Function literals are expressions that create closures with their own scope
func collectFuncLit(ctx *context_v2.CompilerContext, mod *context_v2.Module, lit *ast.FuncLit) {
	if ctx.Debug {
		colors.BROWN.Println("Entering function literal scope")
	}

	// Collect function scope and body using shared helper
	collectFunctionScope(ctx, mod, lit.Type, lit.Body, &lit.Scope)

	if ctx.Debug {
		colors.BROWN.Println("Exiting function literal scope")
	}
}

// collectFunctionScope creates a new scope for a function (named or literal),
// validates and declares parameters, and collects the function body.
// This is shared by both collectFuncDecl and collectFuncLit.
func collectFunctionScope(ctx *context_v2.CompilerContext, mod *context_v2.Module, funcType *ast.FuncType, body *ast.Block, scopePtr *ast.SymbolTable) {
	// Create new scope with current scope as parent
	// For function literals, this enables closure (capturing parent variables)
	newScope := table.NewSymbolTable(mod.CurrentScope)
	*scopePtr = newScope

	// Enter function scope
	oldScope := mod.CurrentScope
	mod.CurrentScope = newScope

	defer func() {
		mod.CurrentScope = oldScope
	}()

	// Validate and declare parameters
	if funcType != nil && funcType.Params != nil {
		validateParams(ctx, mod, funcType.Params)

		// Declare parameters in function scope
		for _, param := range funcType.Params {
			psym := &table.Symbol{
				Name: param.Name.Name,
				Kind: table.SymbolParameter,
				Decl: &param,
				Type: types.TypeUnknown, // Will be filled during type checking
			}

			err := mod.CurrentScope.Declare(param.Name.Name, psym)
			if err != nil {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("duplicate parameter '%s'", param.Name.Name)).
						WithPrimaryLabel(mod.FilePath, param.Loc(), "parameter already declared"),
				)
			}
		}
	}

	// Collect local variables and nested declarations in function body
	if body != nil {
		collectNode(ctx, mod, body)
	}
}

// buildImportAliasMap creates a mapping from alias/module-name to import path
// This enables module::symbol resolution in the resolver phase
func buildImportAliasMap(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	for _, imp := range mod.Imports {
		// If there's an alias, use it
		if imp.Alias != "" {
			mod.ImportAliasMap[imp.Alias] = imp.Path
		} else {
			// Extract the last component of the import path as the default name
			// e.g., "test_project/utils" -> "utils"
			//       "std/math" -> "math"
			parts := splitImportPath(imp.Path)
			if len(parts) > 0 {
				defaultName := parts[len(parts)-1]
				mod.ImportAliasMap[defaultName] = imp.Path
			}
		}
	}
}

// splitImportPath splits an import path by '/' to get components
func splitImportPath(path string) []string {
	// Remove quotes if present
	path = strings.Trim(path, "\"")

	// Split by forward slash
	parts := []string{}
	for _, part := range strings.Split(path, "/") {
		if part != "" {
			parts = append(parts, part)
		}
	}
	return parts
}
