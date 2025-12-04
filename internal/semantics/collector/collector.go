package collector

import (
	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/types"
	"compiler/internal/utils"
	"compiler/internal/utils/numeric"
	"fmt"
)

// CollectModule builds the symbol table for a module by traversing its AST.
// It declares names for let/const/fn/type declarations and fills Module.table.
// No type checking is performed - all symbols get TYPE_UNKNOWN initially.
func CollectModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {

	// Initialize imports list if not exist
	if mod.Imports == nil {
		mod.Imports = make(map[string]*context_v2.Import)
	}

	// Initialize import alias map
	if mod.ImportAliasMap == nil {
		mod.ImportAliasMap = make(map[string]string)
	}

	// Ensure CurrentScope starts at ModuleScope
	mod.CurrentScope = mod.ModuleScope

	// Traverse the AST and collect declarations
	if mod.AST != nil {
		for _, node := range mod.AST.Nodes {
			collectNode(ctx, mod, node)
		}
	}

	// Reset CurrentScope to ModuleScope after collection
	// (it may have been changed during nested scope traversal)
	mod.CurrentScope = mod.ModuleScope

	// Build import alias map after collecting all imports
	buildImportAliasMap(ctx, mod)
}

// collectNode processes a single AST node and declares symbols
func collectNode(ctx *context_v2.CompilerContext, mod *context_v2.Module, node ast.Node) {
	switch n := node.(type) {
	case *ast.VarDecl:
		collectVarDecl(ctx, mod, n, symbols.SymbolVariable)
	case *ast.ConstDecl:
		collectVarDecl(ctx, mod, n, symbols.SymbolConstant)
	case *ast.FuncDecl:
		collectFuncDecl(ctx, mod, n)
	case *ast.MethodDecl:
		collectMethodDecl(ctx, mod, n)
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
	case *ast.ReturnStmt:
		// Collect function literals in return expressions
		if n.Result != nil {
			collectExpr(ctx, mod, n.Result)
		}
	case *ast.AssignStmt:
		// Collect function literals in assignment expressions
		collectExpr(ctx, mod, n.Lhs)
		collectExpr(ctx, mod, n.Rhs)
	case *ast.ExprStmt:
		// Collect function literals in expression statements
		collectExpr(ctx, mod, n.X)
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

	// Enter new block scope and ensure it's restored on exit
	defer mod.EnterScope(blockScope)()

	if ctx.Debug {
		defer colors.BROWN.Println("Exiting new scope...")
	}

	for _, n := range block.Nodes {
		collectNode(ctx, mod, n)
	}
}

// collectVarDecl handles variable and constant declarations
func collectVarDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl interface{}, kind symbols.SymbolKind) {
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
					WithCode(diagnostics.ErrRedeclaredSymbol).
					WithPrimaryLabel(&item.Name.Location, fmt.Sprintf("'%s' already declared", name)).
					WithSecondaryLabel(existing.Decl.Loc(), "previous declaration here"),
			)
			continue
		}

		// Create symbol with TYPE_UNKNOWN (will be filled during type checking)
		sym := &symbols.Symbol{
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
				WithCode(diagnostics.ErrRedeclaredSymbol).
				WithPrimaryLabel(decl.Loc(), fmt.Sprintf("'%s' already declared", name)).
				WithSecondaryLabel(existing.Decl.Loc(), "previous declaration here"),
		)
		return
	}

	// Create symbol (type will be filled during type checking)
	sym := &symbols.Symbol{
		Name:     name,
		Kind:     symbols.SymbolFunction,
		Type:     types.NewFunction([]types.ParamType{}, types.TypeVoid), // Placeholder, will be filled during type checking
		Exported: isExported(name),
		Decl:     decl,
	}

	mod.CurrentScope.Declare(name, sym)

	// Collect function scope and body using shared helper
	collectFunctionScope(ctx, mod, decl.Type, decl.Body, &decl.Scope)
}

// extractReceiverTypeName extracts the type name from a receiver's TypeNode.
// Returns the type name and whether the receiver is valid (not anonymous struct).
// For anonymous structs, it generates and assigns a unique ID and reports an error.
func extractReceiverTypeName(ctx *context_v2.CompilerContext, receiverType ast.TypeNode, receiverLoc *ast.Field) (typeName string, isValid bool) {
	if receiverType == nil {
		ctx.Diagnostics.Add(
			diagnostics.NewError("method receiver must have a type").
				WithCode(diagnostics.ErrInvalidMethodReceiver).
				WithPrimaryLabel(receiverLoc.Loc(), "receiver without type"),
		)
		return utils.GenerateStructLitID(), false
	}

	// Check for anonymous struct type (struct { ... })
	if structType, ok := receiverType.(*ast.StructType); ok {
		ctx.Diagnostics.Add(
			diagnostics.NewError("cannot define methods on anonymous struct types").
				WithCode(diagnostics.ErrInvalidMethodReceiver).
				WithPrimaryLabel(structType.Loc(), "anonymous struct type").
				WithHelp("define a named type first: type MyType struct { ... }").
				WithNote("methods can only be defined on named types"),
		)
		// Generate unique ID for error recovery
		if structType.ID == "" {
			structType.ID = utils.GenerateStructLitID()
		}
		return structType.ID, false
	}

	// Handle direct identifier (most common case: fn (p: Point) ...)
	if ident, ok := receiverType.(*ast.IdentifierExpr); ok {
		return ident.Name, true
	}

	// Handle wrapped reference type (less common: fn (p: &Point) ...)
	// Note: Currently the parser doesn't generate ReferenceType for receivers,
	// but we handle it for future compatibility
	if refType, ok := receiverType.(*ast.ReferenceType); ok {
		if ident, ok := refType.Base.(*ast.IdentifierExpr); ok {
			return ident.Name, true
		}
	}

	// Other type nodes are not supported as receiver types
	return "<unsupported type>", false
}

// collectMethodDecl handles method declarations with receivers
func collectMethodDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.MethodDecl) {
	if decl.Name == nil || decl.Receiver == nil {
		return
	}

	methodName := decl.Name.Name
	receiverType := decl.Receiver.Type

	// Extract receiver type name and validate
	receiverTypeName, isValidReceiver := extractReceiverTypeName(ctx, receiverType, decl.Receiver)

	// Look up the type symbol to get its method scope
	var typeScope *table.SymbolTable
	if isValidReceiver {
		typeSym, found := mod.ModuleScope.Lookup(receiverTypeName)
		if !found {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("cannot define method on undefined type '%s'", receiverTypeName)).
					WithCode(diagnostics.ErrInvalidMethodReceiver).
					WithPrimaryLabel(decl.Receiver.Loc(), fmt.Sprintf("type '%s' not found", receiverTypeName)).
					WithHelp(fmt.Sprintf("declare the type first: type %s struct { ... }", receiverTypeName)),
			)
			isValidReceiver = false
		} else if typeSym.Scope == nil {
			// Type exists but has no scope (internal error - should never happen)
			ctx.ReportError(fmt.Sprintf("type '%s' has no scope for methods (collector bug)", receiverTypeName), decl.Receiver.Loc())
			isValidReceiver = false
		} else {
			typeScope = typeSym.Scope.(*table.SymbolTable)
		}
	}

	// Register method in the type's scope (not module scope)
	// We still create scope for invalid methods to allow analysis of method body
	if isValidReceiver && typeScope != nil {
		// Check for duplicate method
		if existing, ok := typeScope.GetSymbol(methodName); ok {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("method '%s' redeclared on type '%s'", methodName, receiverTypeName)).
					WithCode(diagnostics.ErrRedeclaredSymbol).
					WithPrimaryLabel(decl.Name.Loc(), "method redeclared here").
					WithSecondaryLabel(existing.Decl.Loc(), "previous declaration"),
			)
			// Don't register duplicate, but continue to process scope
		} else {
			// Register method symbol in the type's scope
			sym := &symbols.Symbol{
				Name:     methodName,
				Kind:     symbols.SymbolFunction,
				Type:     types.NewFunction([]types.ParamType{}, types.TypeVoid), // Filled in type checking
				Exported: isExported(methodName),
				Decl:     decl,
			}
			typeScope.Declare(methodName, sym)
		}
	}

	// Create method scope and collect parameters + body
	// This happens regardless of receiver validity to enable analysis of method body
	collectMethodScope(ctx, mod, decl)
}

// collectMethodScope creates scope for a method and collects its parameters and body.
// Receiver is added as the first parameter in the method scope.
func collectMethodScope(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.MethodDecl) {
	// Create method scope with module scope as parent
	methodScope := table.NewSymbolTable(mod.CurrentScope)
	decl.Scope = methodScope

	// Enter method scope
	defer mod.EnterScope(methodScope)()

	// Declare receiver as first parameter
	if decl.Receiver != nil && decl.Receiver.Name != nil {
		receiverSym := &symbols.Symbol{
			Name: decl.Receiver.Name.Name,
			Kind: symbols.SymbolParameter,
			Decl: decl.Receiver,
			Type: types.TypeUnknown, // Filled during type checking
		}
		methodScope.Declare(decl.Receiver.Name.Name, receiverSym)
	}

	// Validate and declare method parameters
	if decl.Type != nil && decl.Type.Params != nil {
		validateParams(ctx, mod, decl.Type.Params)

		for _, param := range decl.Type.Params {
			if param.Name == nil {
				continue // Skip invalid params
			}

			paramSym := &symbols.Symbol{
				Name: param.Name.Name,
				Kind: symbols.SymbolParameter,
				Decl: &param,
				Type: types.TypeUnknown, // Filled during type checking
			}

			if err := methodScope.Declare(param.Name.Name, paramSym); err != nil {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("duplicate parameter '%s'", param.Name.Name)).
						WithCode(diagnostics.ErrRedeclaredSymbol).
						WithPrimaryLabel(param.Loc(), "already declared in this scope"),
				)
			}
		}
	}

	// Collect local variables and nested scopes in method body
	if decl.Body != nil {
		for _, node := range decl.Body.Nodes {
			collectNode(ctx, mod, node)
		}
	}
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
						WithPrimaryLabel(param.Loc(), "variadic parameter here").
						WithNote("a function can have at most one variadic parameter"),
				)
			}
		}
		return
	}

	// Rule: Variadic parameter must be last
	if variadicCount == 1 && variadicIndex != len(params)-1 {
		ctx.Diagnostics.Add(
			diagnostics.NewError("variadic parameter must be last").
				WithPrimaryLabel(params[variadicIndex].Loc(), "variadic parameter not at end").
				WithHelp(fmt.Sprintf("move this parameter to the %s position (last)", numeric.NumericToOrdinal(len(params)))),
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
						WithPrimaryLabel(param.Loc(), fmt.Sprintf("'%s' already declared", name)).
						WithSecondaryLabel(existing.Loc(), "previous declaration here"),
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
					WithPrimaryLabel(param.Loc(), "type annotation required").
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
				WithCode(diagnostics.ErrRedeclaredSymbol).
				WithPrimaryLabel(decl.Loc(), fmt.Sprintf("'%s' already declared", name)).
				WithSecondaryLabel(existing.Decl.Loc(), "previous declaration here"),
		)
		return
	}

	// Create a scope for this type to hold its methods
	typeScope := table.NewSymbolTable(nil) // Type scopes don't need a parent

	// Create symbol
	sym := &symbols.Symbol{
		Name:     name,
		Kind:     symbols.SymbolType,
		Type:     types.TypeUnknown, // The actual type will be resolved later
		Exported: isExported(name),
		Decl:     decl,
		Scope:    typeScope, // Type's scope for methods
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

	// Get or create import entry
	imp, exists := mod.Imports[path]
	if !exists {
		imp = &context_v2.Import{
			Path:     path,
			Alias:    alias,
			Location: stmt.Loc(),
		}
		mod.Imports[path] = imp
	}

	// Append alias and location
	imp.Alias = alias
	imp.Location = stmt.Loc()
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

	// Enter if scope for the body
	{
		defer mod.EnterScope(ifScope)()
		// Collect symbols in the if body
		if stmt.Body != nil {
			collectNode(ctx, mod, stmt.Body)
		}
	}

	// Process else branch (which may be another IfStmt or Block)
	// Note: else branch uses the parent scope, not the if scope
	if stmt.Else != nil {
		collectNode(ctx, mod, stmt.Else)
	}
}

// collectForStmt handles for loops and their scopes
func collectForStmt(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.ForStmt) {
	// Create scope for for loop
	forScope := table.NewSymbolTable(mod.CurrentScope)
	stmt.Scope = forScope

	// Enter for scope and ensure it's restored on exit
	defer mod.EnterScope(forScope)()

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

	// Enter while scope and ensure it's restored on exit
	defer mod.EnterScope(whileScope)()

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

	// Assign the scope to the pointer so the caller can access it
	*scopePtr = newScope

	// Enter function scope and ensure it's restored on exit
	defer mod.EnterScope(newScope)()

	// Validate and declare parameters
	if funcType != nil && funcType.Params != nil {
		validateParams(ctx, mod, funcType.Params)

		// Declare parameters in function scope
		for _, param := range funcType.Params {
			psym := &symbols.Symbol{
				Name: param.Name.Name,
				Kind: symbols.SymbolParameter,
				Decl: &param,
				Type: types.TypeUnknown, // Will be filled during type checking
			}

			err := mod.CurrentScope.Declare(param.Name.Name, psym)
			if err != nil {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("duplicate parameter '%s'", param.Name.Name)).
						WithPrimaryLabel(param.Loc(), "parameter already declared"),
				)
			}
		}
	}

	// Collect local variables and nested declarations in function body
	// Don't call collectBlock since we've already set up the scope
	if body != nil {
		for _, n := range body.Nodes {
			collectNode(ctx, mod, n)
		}
	}
}
