package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/tokens"
	"compiler/internal/types"
)

// inferExprType determines the type of an expression based on its structure and value.
// This is pure type inference without any compatibility checking.
// Returns TYPE_UNTYPED for literals that haven't been contextualized yet.
func inferExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression) types.SemType {
	if expr == nil {
		return types.TypeUnknown
	}

	switch e := expr.(type) {
	case *ast.BasicLit:
		return inferLiteralType(e)

	case *ast.IdentifierExpr:
		return inferIdentifierType(ctx, mod, e)

	case *ast.BinaryExpr:
		return inferBinaryExprType(ctx, mod, e)

	case *ast.UnaryExpr:
		return inferUnaryExprType(ctx, mod, e)

	case *ast.CallExpr:
		return inferCallExprType(ctx, mod, e)

	case *ast.IndexExpr:
		return inferIndexExprType(e)

	case *ast.SelectorExpr:
		return inferSelectorExprType(ctx, mod, e)

	case *ast.ScopeResolutionExpr:
		return inferScopeResolutionExprType(ctx, mod, e)

	case *ast.CastExpr:
		return inferCastExprType(ctx, mod, e)

	case *ast.ParenExpr:
		return inferExprType(ctx, mod, e.X)

	case *ast.CompositeLit:
		return inferCompositeLitType(ctx, mod, e)

	case *ast.FuncLit:
		return inferFuncLitType(ctx, mod, e)

	default:
		return types.TypeUnknown
	}
}

// inferLiteralType determines the type of a literal
func inferLiteralType(lit *ast.BasicLit) types.SemType {
	switch lit.Kind {
	case ast.INT:
		// Integer literals are UNTYPED until contextualized
		return types.TypeUntypedInt
	case ast.FLOAT:
		// Float literals are UNTYPED until contextualized
		return types.TypeUntypedFloat
	case ast.STRING:
		return types.TypeString
	case ast.BOOL:
		return types.TypeBool
	default:
		return types.TypeUnknown
	}
}

// inferIdentifierType looks up an identifier's declared type
func inferIdentifierType(_ *context_v2.CompilerContext, mod *context_v2.Module, ident *ast.IdentifierExpr) types.SemType {
	sym, ok := mod.CurrentScope.Lookup(ident.Name)
	if !ok {
		return types.TypeUnknown
	}
	return sym.Type
}

// inferBinaryExprType determines the result type of a binary expression
func inferBinaryExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.BinaryExpr) types.SemType {
	lhsType := inferExprType(ctx, mod, expr.X)
	rhsType := inferExprType(ctx, mod, expr.Y)

	// Handle UNTYPED operands
	lhsUntyped := types.IsUntyped(lhsType)
	rhsUntyped := types.IsUntyped(rhsType)

	if lhsUntyped && rhsUntyped {
		// Both untyped - preserve untyped, prioritize float over int
		if types.IsUntypedFloat(lhsType) || types.IsUntypedFloat(rhsType) {
			return types.TypeUntypedFloat
		}
		return types.TypeUntypedInt
	}
	if lhsUntyped {
		return rhsType
	}
	if rhsUntyped {
		return lhsType
	}

	// Determine result type based on operator
	switch expr.Op.Kind {
	case tokens.PLUS_TOKEN, tokens.MINUS_TOKEN, tokens.MUL_TOKEN, tokens.DIV_TOKEN, tokens.MOD_TOKEN:
		// Arithmetic: result is the wider of the two types
		return widerType(lhsType, rhsType)

	case tokens.DOUBLE_EQUAL_TOKEN, tokens.NOT_EQUAL_TOKEN,
		tokens.LESS_TOKEN, tokens.LESS_EQUAL_TOKEN,
		tokens.GREATER_TOKEN, tokens.GREATER_EQUAL_TOKEN:
		// Comparison: result is always bool
		return types.TypeBool

	case tokens.AND_TOKEN, tokens.OR_TOKEN:
		// Logical: result is always bool
		return types.TypeBool

	default:
		return types.TypeUnknown
	}
}

// inferUnaryExprType determines the result type of a unary expression
func inferUnaryExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.UnaryExpr) types.SemType {
	xType := inferExprType(ctx, mod, expr.X)

	switch expr.Op.Kind {
	case tokens.MINUS_TOKEN, tokens.PLUS_TOKEN:
		// Unary +/- preserves numeric type
		return xType

	case tokens.NOT_TOKEN:
		// Logical not returns bool
		return types.TypeBool

	default:
		return xType
	}
}

// inferCallExprType determines the return type of a function call
func inferCallExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CallExpr) types.SemType {
	// Get the type of the called expression (function or method)
	funType := inferExprType(ctx, mod, expr.Fun)

	// If it's a function type, return its return type
	if funcType, ok := funType.(*types.FunctionType); ok {
		return funcType.Return
	}

	// Fallback: handle direct function name lookup (legacy path)
	if ident, ok := expr.Fun.(*ast.IdentifierExpr); ok {
		sym, exists := mod.CurrentScope.Lookup(ident.Name)
		if !exists {
			return types.TypeUnknown
		}

		// Get the function declaration
		if funcDecl, ok := sym.Decl.(*ast.FuncDecl); ok {
			if funcDecl.Type != nil && funcDecl.Type.Result != nil {
				return typeFromTypeNodeWithContext(ctx, mod, funcDecl.Type.Result)
			}
		}
	}

	return types.TypeUnknown
}

// inferIndexExprType determines the element type of an index expression
func inferIndexExprType(expr *ast.IndexExpr) types.SemType {
	// TODO: Implement array/map element type extraction
	return types.TypeUnknown
}

// inferSelectorExprType determines the type of a field access or method access
func inferSelectorExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.SelectorExpr) types.SemType {
	// Infer the type of the base expression (e.g., p in p.x)
	baseType := inferExprType(ctx, mod, expr.X)

	// If base type is unknown, return unknown
	if baseType.Equals(types.TypeUnknown) {
		return types.TypeUnknown
	}

	// Handle struct field or method access
	if structType, ok := baseType.(*types.StructType); ok {
		fieldName := expr.Sel.Name // First, check if it's a struct field
		for _, field := range structType.Fields {
			if field.Name == fieldName {
				return field.Type
			}
		}

		// If not a field, check if it's a method on the named type
		// Methods are attached to type symbols in their declaring module
		if structType.Name != "" {
			// Try to find the type symbol by looking it up by name
			// First check current module's scope
			typeSym, found := mod.ModuleScope.Lookup(structType.Name)
			if !found {
				// Not in current module, might be from an imported module
				// Search through all modules to find where this type was declared
				// This is needed because we only have the type, not the original module reference
				for _, importPath := range mod.ImportAliasMap {
					if importedMod, exists := ctx.GetModule(importPath); exists {
						if sym, ok := importedMod.ModuleScope.GetSymbol(structType.Name); ok && sym.Kind == symbols.SymbolType {
							typeSym = sym
							found = true
							break
						}
					}
				}
			}

			if found && typeSym.Methods != nil {
				if method, ok := typeSym.Methods[fieldName]; ok {
					return method.FuncType
				}
			}
		}

		// Field/method not found - error will be reported by type checker
		return types.TypeUnknown
	}

	// For non-struct types, return unknown (error will be reported by type checker)
	return types.TypeUnknown
}

// inferScopeResolutionExprType determines the type of a module::symbol access
func inferScopeResolutionExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.ScopeResolutionExpr) types.SemType {
	// Handle module::symbol resolution
	// X should be an identifier representing the module name/alias
	if ident, ok := expr.X.(*ast.IdentifierExpr); ok {
		moduleName := ident.Name
		symbolName := expr.Selector.Name

		// Look up the import path from the alias/name
		importPath, ok := mod.ImportAliasMap[moduleName]
		if !ok {
			// Module not imported - error already reported by resolver
			return types.TypeUnknown
		}

		// Get the imported module from context
		importedMod, exists := ctx.GetModule(importPath)
		if !exists {
			// Module not loaded - error already reported by resolver
			return types.TypeUnknown
		}

		// Look up the symbol in the imported module's module scope
		// Use ModuleScope.GetSymbol (not Lookup) to get module-level symbols only
		sym, ok := importedMod.ModuleScope.GetSymbol(symbolName)
		if !ok {
			// Symbol not found - error already reported by resolver
			return types.TypeUnknown
		}

		// Return the symbol's type
		return sym.Type
	}

	// For non-identifier X (e.g., enum::variant), recurse
	return inferExprType(ctx, mod, expr.X)
}

// inferCastExprType determines the target type of a cast
func inferCastExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CastExpr) types.SemType {
	return typeFromTypeNodeWithContext(ctx, mod, expr.Type)
}

// inferCompositeLitType determines the type of a composite literal
func inferCompositeLitType(ctx *context_v2.CompilerContext, mod *context_v2.Module, lit *ast.CompositeLit) types.SemType {
	if lit.Type != nil {
		return typeFromTypeNodeWithContext(ctx, mod, lit.Type)
	}

	// Infer anonymous struct type from field expressions
	// Check if all elements are key-value pairs (struct literal)
	if len(lit.Elts) > 0 {
		allKeyValue := true
		for _, elem := range lit.Elts {
			if _, ok := elem.(*ast.KeyValueExpr); !ok {
				allKeyValue = false
				break
			}
		}

		if allKeyValue {
			// Build anonymous struct type from fields
			fields := make([]types.StructField, 0, len(lit.Elts))
			for _, elem := range lit.Elts {
				kv := elem.(*ast.KeyValueExpr)
				if key, ok := kv.Key.(*ast.IdentifierExpr); ok {
					fieldName := key.Name
					fieldType := inferExprType(ctx, mod, kv.Value)
					fields = append(fields, types.StructField{
						Name: fieldName,
						Type: fieldType,
					})
				}
			}

			if len(fields) > 0 {
				// Create anonymous struct type (empty name)
				return types.NewStruct("", fields)
			}
		}
	}

	return types.TypeUnknown
}

// widerType returns the wider of two numeric types for implicit conversion
func widerType(a, b types.SemType) types.SemType {
	// If types are the same, return either
	if a.Equals(b) {
		return a
	}

	// Extract primitive names for comparison
	aName, aOk := types.GetPrimitiveName(a)
	bName, bOk := types.GetPrimitiveName(b)
	if !aOk || !bOk {
		return types.TypeUnknown
	}

	// Order of widening: i8 < i16 < i32 < i64, u8 < u16 < u32 < u64, f32 < f64
	typeRank := map[types.TYPE_NAME]int{
		types.TYPE_I8:  1,
		types.TYPE_I16: 2,
		types.TYPE_I32: 3,
		types.TYPE_I64: 4,
		types.TYPE_U8:  1,
		types.TYPE_U16: 2,
		types.TYPE_U32: 3,
		types.TYPE_U64: 4,
		types.TYPE_F32: 5,
		types.TYPE_F64: 6,
	}

	rankA, okA := typeRank[aName]
	rankB, okB := typeRank[bName]

	if !okA || !okB {
		// If either type is not numeric, return unknown
		return types.TypeUnknown
	}

	if rankA > rankB {
		return a
	}
	return b
}

// finalizeUntyped converts an UNTYPED literal to a concrete default type
func finalizeUntyped(expr ast.Expression) types.SemType {
	if lit, ok := expr.(*ast.BasicLit); ok {
		switch lit.Kind {
		case ast.INT:
			// Default integer type is i64
			return types.TypeI64
		case ast.FLOAT:
			// Default float type is f64
			return types.TypeF64
		}
	}
	return types.TypeUnknown
}

// contextualizeUntyped applies contextual typing to an UNTYPED literal
func contextualizeUntyped(lit *ast.BasicLit, expected types.SemType) types.SemType {
	switch lit.Kind {
	case ast.INT:
		// Try to fit into expected type
		if types.IsNumeric(expected) {
			if _, ok := types.GetPrimitiveName(expected); ok {
				// Use big.Int for all integer validation (simpler, more maintainable)
				if fitsInType(lit.Value, expected) {
					return expected
				}
			}
		}
		// Can't contextualize - stay untyped for better error messages
		return types.TypeUntypedInt

	case ast.FLOAT:
		// Try to fit into expected float type (check precision)
		if types.IsFloat(expected) {
			if _, ok := types.GetPrimitiveName(expected); ok {
				if fitsInType(lit.Value, expected) {
					return expected
				}
			}
		}
		// Can't contextualize - stay untyped for better error messages
		return types.TypeUntypedFloat

	default:
		return types.TypeUnknown
	}
}

// inferFuncLitType infers the type of a function literal
func inferFuncLitType(ctx *context_v2.CompilerContext, mod *context_v2.Module, lit *ast.FuncLit) types.SemType {
	if lit.Type == nil {
		return types.TypeUnknown
	}

	// Enter function literal scope for type checking the body
	if lit.Scope != nil {
		defer mod.EnterScope(lit.Scope.(*table.SymbolTable))()
	}

	// Build parameter types
	params := make([]types.ParamType, len(lit.Type.Params))
	for i, param := range lit.Type.Params {
		params[i] = types.ParamType{
			Name: param.Name.Name,
			Type: typeFromTypeNode(param.Type),
		}

		// Update symbol table with the parameter's type
		if sym, ok := mod.CurrentScope.GetSymbol(param.Name.Name); ok {
			sym.Type = params[i].Type
		}
	}

	// Get return type
	returnType := typeFromTypeNode(lit.Type.Result)

	// Set the current function return type for validating return statements
	// Save the parent function's return type and restore it after
	prevReturnType := mod.CurrentFunctionReturnType
	mod.CurrentFunctionReturnType = returnType
	defer func() { mod.CurrentFunctionReturnType = prevReturnType }()

	// Type check the function body
	if lit.Body != nil {
		checkBlock(ctx, mod, lit.Body)
	}

	return types.NewFunction(params, returnType)
}
