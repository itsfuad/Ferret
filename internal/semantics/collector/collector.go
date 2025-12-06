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
	"compiler/internal/utils/fs"
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

	// First pass: collect everything EXCEPT methods
	// Methods are collected in a separate phase after all types are known
	if mod.AST != nil {
		for _, node := range mod.AST.Nodes {
			// Skip method declarations in this phase
			if _, isMethod := node.(*ast.MethodDecl); !isMethod {
				collectNode(ctx, mod, node)
			}
		}
	}

	// Reset CurrentScope to ModuleScope after collection
	// (it may have been changed during nested scope traversal)
	mod.CurrentScope = mod.ModuleScope
}

// CollectMethodsOnly collects only method declarations from a module
// This is run in a second pass after all types are known across all modules
func CollectMethodsOnly(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	if mod.AST == nil {
		return
	}

	// Ensure CurrentScope is at ModuleScope
	mod.CurrentScope = mod.ModuleScope

	// Collect only method declarations
	methodCount := 0
	for _, node := range mod.AST.Nodes {
		if methodDecl, ok := node.(*ast.MethodDecl); ok {
			collectMethodDecl(ctx, mod, methodDecl)
			methodCount++
		}
	}

	// Reset CurrentScope
	mod.CurrentScope = mod.ModuleScope
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
	// case *ast.MethodDecl:
	// 	collectMethodDecl(ctx, mod, n)
	case *ast.TypeDecl:
		collectTypeDecl(ctx, mod, n)
	case *ast.ImportStmt:
		collectImport(ctx, mod, n)
	case *ast.DeclStmt:
		collectNode(ctx, mod, n.Decl)
	case *ast.Block:
		collectBlock(ctx, mod, n)
	case *ast.IfStmt:
		// Check if at module scope (top level)
		if mod.CurrentScope == mod.ModuleScope {
			ctx.Diagnostics.Add(
				diagnostics.NewError("if statement not allowed at module level").
					WithPrimaryLabel(n.Loc(), "remove this statement").
					WithNote("control flow statements must be inside functions"),
			)
		}
		collectIfStmt(ctx, mod, n)
	case *ast.ForStmt:
		// Check if at module scope (top level)
		if mod.CurrentScope == mod.ModuleScope {
			ctx.Diagnostics.Add(
				diagnostics.NewError("for loop not allowed at module level").
					WithPrimaryLabel(n.Loc(), "remove this statement").
					WithNote("control flow statements must be inside functions"),
			)
		}
		collectForStmt(ctx, mod, n)
	case *ast.WhileStmt:
		// Check if at module scope (top level)
		if mod.CurrentScope == mod.ModuleScope {
			ctx.Diagnostics.Add(
				diagnostics.NewError("while loop not allowed at module level").
					WithPrimaryLabel(n.Loc(), "remove this statement").
					WithNote("control flow statements must be inside functions"),
			)
		}
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

		// Skip placeholder '_' - it's not a real symbol
		if name == "_" {
			// Still collect function literals in initializer if present
			if item.Value != nil {
				collectExpr(ctx, mod, item.Value)
			}
			continue
		}

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

// extractReceiverTypeName extracts the type name and optional module from a receiver's TypeNode.
// Returns the type name, module alias (empty for local types), and whether the receiver is valid.
// For anonymous structs, it generates and assigns a unique ID and reports an error.
func extractReceiverTypeName(ctx *context_v2.CompilerContext, receiverType ast.TypeNode, receiverLoc *ast.Field) (typeName string, moduleAlias string, isValid bool) {
	if receiverType == nil {
		ctx.Diagnostics.Add(
			diagnostics.NewError("method receiver must have a type").
				WithCode(diagnostics.ErrInvalidMethodReceiver).
				WithPrimaryLabel(receiverLoc.Loc(), "receiver without type"),
		)
		return utils.GenerateStructLitID(), "", false
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

		return structType.ID, "", false
	}

	// check for anonymous interface, similar to anonymous structs. interfae { m1()}
	if interfaceType, ok := receiverType.(*ast.InterfaceType); ok {
		ctx.Diagnostics.Add(
			diagnostics.NewError("cannot define methods on anonymous interface type").
				WithCode(diagnostics.ErrInvalidMethodReceiver).
				WithPrimaryLabel(interfaceType.Loc(), "anonymous interface type").
				WithHelp("define a named type first: type Mytype interface { ... }").
				WithNote("methods can only be defined on named types"),
		)

	}

	// Handle direct identifier (most common case: fn (p: Point) ...)
	if ident, ok := receiverType.(*ast.IdentifierExpr); ok {
		return ident.Name, "", true
	}

	// Handle scope resolution (cross-module types: fn (p: utils::Point) ...)
	if scopeRes, ok := receiverType.(*ast.ScopeResolutionExpr); ok {
		// Return both module and type name separately
		if moduleIdent, ok := scopeRes.X.(*ast.IdentifierExpr); ok {
			return scopeRes.Selector.Name, moduleIdent.Name, true
		}
	}

	// Handle wrapped reference type (less common: fn (p: &Point) ...)
	// Note: Currently the parser doesn't generate ReferenceType for receivers,
	// but we handle it for future compatibility
	if refType, ok := receiverType.(*ast.ReferenceType); ok {
		if ident, ok := refType.Base.(*ast.IdentifierExpr); ok {
			return ident.Name, "", true
		}
	}

	// Other type nodes are not supported as receiver types
	return "<unsupported type>", "", false
}

// collectMethodDecl handles method declarations with receivers.
// Methods are stored in module scope with qualified name "TypeName::methodName".
// This treats methods as functions with receiver as first parameter, simplifying the architecture.
func collectMethodDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.MethodDecl) {
	if decl.Name == nil || decl.Receiver == nil {
		return
	}

	methodName := decl.Name.Name
	receiverType := decl.Receiver.Type

	// Extract receiver type name and module (if cross-module)
	typeName, moduleAlias, isValidReceiver := extractReceiverTypeName(ctx, receiverType, decl.Receiver)

	// Will hold the type symbol for attaching methods
	var typeSym *symbols.Symbol
	var found bool

	// Validate receiver type exists and is user-defined
	if isValidReceiver {
		// Check if it's a cross-module type
		if moduleAlias != "" {
			// Look up the import
			importPath, ok := mod.ImportAliasMap[moduleAlias]
			if !ok {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("module '%s' not imported", moduleAlias)).
						WithCode(diagnostics.ErrInvalidMethodReceiver).
						WithPrimaryLabel(decl.Receiver.Loc(), fmt.Sprintf("cannot use type from non-imported module '%s'", moduleAlias)).
						WithHelp(fmt.Sprintf("add: import \"%s\"", moduleAlias)),
				)
				isValidReceiver = false
			} else {
				// Get the imported module
				importedMod, exists := ctx.GetModule(importPath)
				if !exists {
					ctx.Diagnostics.Add(
						diagnostics.NewError(fmt.Sprintf("imported module '%s' not found", importPath)).
							WithCode(diagnostics.ErrInvalidMethodReceiver).
							WithPrimaryLabel(decl.Receiver.Loc(), "module not loaded"),
					)
					isValidReceiver = false
				} else {
					// Look up type in the imported module's scope
					typeSym, found = importedMod.ModuleScope.GetSymbol(typeName)
					if !found {
						ctx.Diagnostics.Add(
							diagnostics.NewError(fmt.Sprintf("type '%s' not found in module '%s'", typeName, importPath)).
								WithCode(diagnostics.ErrInvalidMethodReceiver).
								WithPrimaryLabel(decl.Receiver.Loc(), fmt.Sprintf("'%s' not found", typeName)),
						)
						isValidReceiver = false
					}
				}
			}
		} else {
			// Local type - look in current module scope
			typeSym, found = mod.ModuleScope.Lookup(typeName)
			if !found {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("cannot define method on undefined type '%s'", typeName)).
						WithCode(diagnostics.ErrInvalidMethodReceiver).
						WithPrimaryLabel(decl.Receiver.Loc(), fmt.Sprintf("type '%s' not found", typeName)).
						WithHelp(fmt.Sprintf("declare the type first: type %s struct { ... }", typeName)),
				)
				isValidReceiver = false
			}
		}

		// Validate it's actually a type (not a variable or function)
		if found && typeSym != nil && typeSym.Kind != symbols.SymbolType {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("cannot define method on non-type '%s'", typeName)).
					WithCode(diagnostics.ErrInvalidMethodReceiver).
					WithPrimaryLabel(decl.Receiver.Loc(), fmt.Sprintf("'%s' is not a type", typeName)),
			)
			isValidReceiver = false
		}

		// Check for duplicate method declaration on this type
		if found && typeSym != nil {
			if typeSym.Methods != nil {
				if _, exists := typeSym.Methods[methodName]; exists {
					ctx.Diagnostics.Add(
						diagnostics.NewError(fmt.Sprintf("method '%s' redeclared on type '%s'", methodName, typeName)).
							WithCode(diagnostics.ErrRedeclaredSymbol).
							WithPrimaryLabel(decl.Name.Loc(), "method redeclared here"),
					)
				}
			}
		}
	}

	// Create method scope and collect parameters + body
	// This happens regardless of receiver validity to enable analysis of method body
	// Reuse function scope logic since methods are just functions with receivers
	collectFunctionScope(ctx, mod, decl.Type, decl.Body, &decl.Scope)

	// Add receiver as the first parameter in the scope
	// This is done after collectFunctionScope so receiver appears before regular params
	if decl.Receiver != nil && decl.Receiver.Name != nil && decl.Scope != nil {
		methodScope := decl.Scope.(*table.SymbolTable)
		receiverSym := &symbols.Symbol{
			Name: decl.Receiver.Name.Name,
			Kind: symbols.SymbolParameter,
			Decl: decl.Receiver,
			Type: types.TypeUnknown, // Filled during type checking
		}
		// Declare receiver (may fail if param name conflicts, but that's a user error)
		if err := methodScope.Declare(decl.Receiver.Name.Name, receiverSym); err != nil {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("receiver name '%s' conflicts with parameter name", decl.Receiver.Name.Name)).
					WithCode(diagnostics.ErrRedeclaredSymbol).
					WithPrimaryLabel(decl.Receiver.Loc(), "receiver declared here"),
			)
		}
	}
}

// validateParams validates function parameters including variadic rules, duplicates, and type annotations
// This is shared by both named functions and function literals
func validateParams(ctx *context_v2.CompilerContext, _ *context_v2.Module, params []ast.Field) {
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

	// Create symbol
	// Note: Type symbols no longer need their own scope since methods are stored
	// in module scope with qualified names ("TypeName::methodName")
	sym := &symbols.Symbol{
		Name:     name,
		Kind:     symbols.SymbolType,
		Type:     types.TypeUnknown, // The actual type will be resolved later
		Exported: isExported(name),
		Decl:     decl,
	}

	mod.CurrentScope.Declare(name, sym)

	// Special handling for enums: register variants as placeholders
	// The actual values will be computed during type checking
	if enumType, ok := decl.Type.(*ast.EnumType); ok {
		for _, variant := range enumType.Variants {
			variantName := variant.Name.Name
			qualifiedName := name + "::" + variantName

			// Check for duplicate variant name
			if existing, ok := mod.ModuleScope.GetSymbol(qualifiedName); ok {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("duplicate enum variant '%s'", variantName)).
						WithCode(diagnostics.ErrRedeclaredSymbol).
						WithPrimaryLabel(variant.Name.Loc(), "variant already defined").
						WithSecondaryLabel(existing.Decl.Loc(), "previous definition here"),
				)
				continue
			}

			// Register variant as a placeholder symbol
			// Type will be filled in during type checking
			variantSym := &symbols.Symbol{
				Name:     qualifiedName,
				Kind:     symbols.SymbolConstant,
				Type:     types.TypeUnknown, // Will be set to the enum type during type checking
				Exported: isExported(variantName),
				Decl:     variant.Name,
			}
			mod.ModuleScope.Declare(qualifiedName, variantSym)
		}
	}
}

// collectImport handles import statements
func collectImport(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.ImportStmt) {
	// BasicLit has a Value field containing the string value
	path := stmt.Path.Value
	alias := ""
	if stmt.Alias != nil && stmt.Alias.Name != "" {
		alias = stmt.Alias.Name
	} else {
		alias = fs.LastPart(path)
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

	if oldImpPath, exists := mod.ImportAliasMap[alias]; exists {
		oldImp := mod.Imports[oldImpPath]
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("duplicate import alias '%s'", alias)).
				WithCode(diagnostics.ErrRedeclaredSymbol).
				WithPrimaryLabel(imp.Location, fmt.Sprintf("'%s' already used for another import", alias)).
				WithSecondaryLabel(oldImp.Location, "previous import here").
				WithHelp(fmt.Sprintf("use a different alias: import \"%s\" as %s_alt", imp.Path, alias)),
		)
	} else {
		mod.ImportAliasMap[alias] = imp.Path
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

	// Collect iterator (VarDecl or IdentifierExpr)
	// If it's a VarDecl, it will be declared in the for scope
	// If it's an IdentifierExpr, it references an existing variable
	if stmt.Iterator != nil {
		collectNode(ctx, mod, stmt.Iterator)
	}

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
