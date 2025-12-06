package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/tokens"
	"compiler/internal/types"
	"fmt"
	"strings"
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
		return inferIndexExprType(ctx, mod, e)

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

	case *ast.RangeExpr:
		return inferRangeExprType(ctx, mod, e)

	case *ast.ElvisExpr:
		return inferElvisExprType(ctx, mod, e)

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
	case ast.BYTE:
		// Byte literals have concrete type 'byte'
		return types.TypeByte
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
// For arrays: returns the element type T for []T or [N]T
// For maps: returns optional value type V? for map[K]V (since key might not exist)
func inferIndexExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.IndexExpr) types.SemType {
	// Infer the type of the base expression (the thing being indexed)
	baseType := inferExprType(ctx, mod, expr.X)

	// Unwrap any named types to get to the underlying structure
	baseType = types.UnwrapType(baseType)

	switch bt := baseType.(type) {
	case *types.ArrayType:
		// Array indexing: arr[i] returns element type T
		return bt.Element

	case *types.MapType:
		// Map indexing: map[key] returns optional value type V?
		// This forces handling of missing keys at compile time
		return types.NewOptional(bt.Value)

	default:
		// Not an indexable type (error will be reported by type checker)
		return types.TypeUnknown
	}
}

// inferSelectorExprType determines the type of a field access or method access
func inferSelectorExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.SelectorExpr) types.SemType {
	// Infer the type of the base expression (e.g., p in p.x)
	baseType := inferExprType(ctx, mod, expr.X)

	// If base type is unknown, return unknown
	if baseType.Equals(types.TypeUnknown) {
		return types.TypeUnknown
	}

	fieldName := expr.Sel.Name

	// If baseType is a NamedType, we might have methods attached to the type symbol
	// We need to check both fields (on the underlying struct) and methods (on the named type)
	var typeSym *symbols.Symbol
	var structType *types.StructType

	if namedType, ok := baseType.(*types.NamedType); ok {
		// It's a named type - look up the type symbol for method resolution
		typeName := namedType.Name

		// First check current module's scope
		if sym, found := mod.ModuleScope.Lookup(typeName); found {
			typeSym = sym
		} else {
			// Not in current module, search imported modules
			for _, importPath := range mod.ImportAliasMap {
				if importedMod, exists := ctx.GetModule(importPath); exists {
					if sym, ok := importedMod.ModuleScope.GetSymbol(typeName); ok && sym.Kind == symbols.SymbolType {
						typeSym = sym
						break
					}
				}
			}
		}

		// Get the underlying struct for field access
		underlying := types.UnwrapType(namedType)
		structType, _ = underlying.(*types.StructType)
	} else if st, ok := baseType.(*types.StructType); ok {
		// Anonymous struct - no methods, only fields
		structType = st
	}

	// Check for struct fields first
	if structType != nil {
		for _, field := range structType.Fields {
			if field.Name == fieldName {
				return field.Type
			}
		}
	}

	// Check for methods on named types
	if typeSym != nil && typeSym.Methods != nil {
		if method, ok := typeSym.Methods[fieldName]; ok {
			return method.FuncType
		}
	}

	// Field/method not found - error will be reported by type checker
	return types.TypeUnknown
}

// inferScopeResolutionExprType determines the type of a module::symbol or enum::variant access
func inferScopeResolutionExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.ScopeResolutionExpr) types.SemType {
	// Handle module::symbol or enum::variant resolution
	// X should be an identifier representing the module name/alias or enum type name
	if ident, ok := expr.X.(*ast.IdentifierExpr); ok {
		leftName := ident.Name
		rightName := expr.Selector.Name

		// First check if leftName is a type (enum) in the current scope
		if typeSym, ok := mod.CurrentScope.Lookup(leftName); ok && typeSym.Kind == symbols.SymbolType {
			// This is EnumType::Variant access
			// Look up the qualified variant name
			qualifiedName := leftName + "::" + rightName
			if variantSym, ok := mod.ModuleScope.GetSymbol(qualifiedName); ok {
				return variantSym.Type
			}
			// Variant not found - error already reported by resolver
			return types.TypeUnknown
		}

		// Otherwise, check if it's a module import
		// Look up the import path from the alias/name
		importPath, ok := mod.ImportAliasMap[leftName]
		if !ok {
			// Not a module import - error already reported by resolver
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
		sym, ok := importedMod.ModuleScope.GetSymbol(rightName)
		if !ok {
			// Symbol not found - error already reported by resolver
			return types.TypeUnknown
		}

		// Return the symbol's type
		return sym.Type
	}

	// For non-identifier X (e.g., anonymous enum{...}::variant)
	// Handle anonymous enum type validation
	if enumType, ok := expr.X.(*ast.EnumType); ok {
		rightName := expr.Selector.Name

		// Check if the variant exists in the anonymous enum
		found := false
		for _, variant := range enumType.Variants {
			if variant.Name.Name == rightName {
				found = true
				break
			}
		}

		if !found {
			// Build enum string representation with truncation (like struct)
			var enumStr string
			if len(enumType.Variants) == 0 {
				enumStr = "enum {}"
			} else if len(enumType.Variants) <= 3 {
				// Show all variants if 3 or fewer
				parts := make([]string, len(enumType.Variants))
				for i, v := range enumType.Variants {
					parts[i] = v.Name.Name
				}
				enumStr = fmt.Sprintf("enum { %s }", strings.Join(parts, ", "))
			} else {
				// Truncate: first 2, ..., last 1
				enumStr = fmt.Sprintf("enum { %s, %s, ..., %s }",
					enumType.Variants[0].Name.Name,
					enumType.Variants[1].Name.Name,
					enumType.Variants[len(enumType.Variants)-1].Name.Name)
			}

			// Build list of available variants
			variantNames := make([]string, len(enumType.Variants))
			for i, v := range enumType.Variants {
				variantNames[i] = v.Name.Name
			}

			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("variant '%s' not found in %s", rightName, enumStr)).
					WithPrimaryLabel(expr.Selector.Loc(), fmt.Sprintf("'%s' is not a valid variant", rightName)).
					WithHelp(fmt.Sprintf("available variants: %s", strings.Join(variantNames, ", "))),
			)
			return types.TypeUnknown
		}

		// Valid variant - return unknown for anonymous enums (they don't have a semantic type)
		// This is acceptable since anonymous enums are primarily for compile-time constants
		return types.TypeUnknown
	}

	// For other expression types, recurse
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

	// Infer type from elements
	// Distinguish between struct literals (.field = value) and map literals (key => value)
	if len(lit.Elts) > 0 {
		allKeyValue := true
		hasStructSyntax := false // .field = value
		hasMapSyntax := false    // key => value

		for _, elem := range lit.Elts {
			kv, ok := elem.(*ast.KeyValueExpr)
			if !ok {
				allKeyValue = false
				break
			}

			// Check if key is an identifier (struct field or map key)
			if _, ok := kv.Key.(*ast.IdentifierExpr); ok {
				// Check if it looks like struct field syntax (.field)
				// Parser doesn't store dot prefix, so we infer from context
				// For now, treat plain identifiers as struct fields
				// Map keys should be arbitrary expressions
				hasStructSyntax = true
			} else {
				// Non-identifier key (expression) means map syntax
				hasMapSyntax = true
			}
		}

		if allKeyValue {
			// Determine if this is a struct or map literal
			// Struct: all keys are identifiers (field names)
			// Map: keys are arbitrary expressions
			if hasStructSyntax && !hasMapSyntax {
				// Struct literal: { .x: 10, .y: 20 }
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
			} else if hasMapSyntax || len(lit.Elts) > 0 {
				// Map literal: { "key" => value, "key2" => value2 }
				// Infer key and value types from first element
				// All keys must have compatible types, all values must have compatible types
				var keyType, valueType types.SemType = types.TypeUnknown, types.TypeUnknown

				for _, elem := range lit.Elts {
					kv := elem.(*ast.KeyValueExpr)
					elemKeyType := inferExprType(ctx, mod, kv.Key)
					elemValueType := inferExprType(ctx, mod, kv.Value)

					// Use first element's types as baseline
					if keyType.Equals(types.TypeUnknown) {
						keyType = elemKeyType
					}
					if valueType.Equals(types.TypeUnknown) {
						valueType = elemValueType
					}

					// TODO: Validate all keys/values have compatible types
					// For now, just use the first element's types
				}

				if !keyType.Equals(types.TypeUnknown) && !valueType.Equals(types.TypeUnknown) {
					return types.NewMap(keyType, valueType)
				}
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
			// Use centralized resolution to pick minimum required type
			return resolveUntypedInt(lit.Value)
		case ast.FLOAT:
			// Use centralized default float type
			return types.NewPrimitive(types.DEFAULT_FLOAT_TYPE)
		}
	}
	return types.TypeUnknown
}

// resolveUntypedInt picks the minimum integer type that can hold the given value.
// This is the central system for untyped integer resolution:
// - Defaults to i32 (DEFAULT_INT_TYPE) if value fits
// - Upgrades to i64, i128, i256 if value exceeds default bitsize
// - For signed values, uses signed type hierarchy
func resolveUntypedInt(valueStr string) types.SemType {
	// Default: try to fit in i32 (DEFAULT_INT_TYPE)
	defaultType := types.NewPrimitive(types.DEFAULT_INT_TYPE)
	if fitsInType(valueStr, defaultType) {
		return defaultType
	}

	// Value doesn't fit in default - try progressively larger signed types
	signedTypes := []types.TYPE_NAME{
		types.TYPE_I64,
		types.TYPE_I128,
		types.TYPE_I256,
	}

	for _, typeName := range signedTypes {
		t := types.NewPrimitive(typeName)
		if fitsInType(valueStr, t) {
			return t
		}
	}

	// Value is too large for any supported type - return i256 anyway
	// (error will be reported elsewhere)
	return types.NewPrimitive(types.TYPE_I256)
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

// inferRangeExprType infers the type of a range expression (start..end[:incr])
// Returns an array type with element type inferred from start/end
func inferRangeExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.RangeExpr) types.SemType {
	// Infer types from start and end expressions
	startType := inferExprType(ctx, mod, expr.Start)
	endType := inferExprType(ctx, mod, expr.End)

	// If both are untyped int literals, resolve to minimum required type
	// by checking both values and taking the larger type
	if types.IsUntypedInt(startType) && types.IsUntypedInt(endType) {
		var resolvedType types.SemType = types.NewPrimitive(types.DEFAULT_INT_TYPE) // default to i32
		maxBitSize := types.GetNumberBitSize(types.DEFAULT_INT_TYPE)

		// Check if start literal needs a larger type
		if startLit, ok := expr.Start.(*ast.BasicLit); ok && startLit.Kind == ast.INT {
			startResolved := resolveUntypedInt(startLit.Value)
			if name, ok := types.GetPrimitiveName(startResolved); ok {
				bitSize := types.GetNumberBitSize(name)
				if bitSize > maxBitSize {
					maxBitSize = bitSize
					resolvedType = startResolved
				}
			}
		}

		// Check if end literal needs a larger type
		if endLit, ok := expr.End.(*ast.BasicLit); ok && endLit.Kind == ast.INT {
			endResolved := resolveUntypedInt(endLit.Value)
			if name, ok := types.GetPrimitiveName(endResolved); ok {
				bitSize := types.GetNumberBitSize(name)
				if bitSize > maxBitSize {
					resolvedType = endResolved
				}
			}
		}

		// Return array type with resolved element type
		return &types.ArrayType{Element: resolvedType}
	}

	// If start is untyped, use centralized resolution
	if types.IsUntypedInt(startType) {
		if startLit, ok := expr.Start.(*ast.BasicLit); ok && startLit.Kind == ast.INT {
			startType = resolveUntypedInt(startLit.Value)
		} else {
			startType = types.NewPrimitive(types.DEFAULT_INT_TYPE)
		}
	}

	// Return array type with element type from range
	return &types.ArrayType{Element: startType}
}

// inferElvisExprType determines the result type of an elvis expression (a ?: b)
// The elvis operator unwraps optional types:
// - If 'a' has type T?, the result is T (non-optional)
// - The fallback 'b' should have type T to match
// - Result type is always the non-optional inner type
func inferElvisExprType(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.ElvisExpr) types.SemType {
	condType := inferExprType(ctx, mod, expr.Cond)

	// Check if condition is an optional type
	if optType, ok := condType.(*types.OptionalType); ok {
		// Elvis operator unwraps the optional: T? ?: T → T
		return optType.Inner
	}

	// If condition is not optional, elvis is redundant but still valid
	// Result type is the condition type itself
	return condType
}
