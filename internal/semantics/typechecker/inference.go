package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/semantics/table"
	"compiler/internal/types"
	"strconv"
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
		return inferSelectorExprType(e)

	case *ast.ScopeResolutionExpr:
		return inferScopeResolutionExprType(ctx, mod, e)

	case *ast.CastExpr:
		return inferCastExprType(e)

	case *ast.ParenExpr:
		return inferExprType(ctx, mod, e.X)

	case *ast.CompositeLit:
		return inferCompositeLitType(e)

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
		return types.TypeUntyped

	case ast.FLOAT:
		// Float literals are UNTYPED until contextualized
		return types.TypeUntyped

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
	if lhsType.Equals(types.TypeUntyped) && rhsType.Equals(types.TypeUntyped) {
		return types.TypeUntyped
	}
	if lhsType.Equals(types.TypeUntyped) {
		return rhsType
	}
	if rhsType.Equals(types.TypeUntyped) {
		return lhsType
	}

	// Determine result type based on operator
	switch expr.Op.Kind {
	case lexer.PLUS_TOKEN, lexer.MINUS_TOKEN, lexer.MUL_TOKEN, lexer.DIV_TOKEN, lexer.MOD_TOKEN:
		// Arithmetic: result is the wider of the two types
		return widerType(lhsType, rhsType)

	case lexer.DOUBLE_EQUAL_TOKEN, lexer.NOT_EQUAL_TOKEN,
		lexer.LESS_TOKEN, lexer.LESS_EQUAL_TOKEN,
		lexer.GREATER_TOKEN, lexer.GREATER_EQUAL_TOKEN:
		// Comparison: result is always bool
		return types.TypeBool

	case lexer.AND_TOKEN, lexer.OR_TOKEN:
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
	case lexer.MINUS_TOKEN, lexer.PLUS_TOKEN:
		// Unary +/- preserves numeric type
		return xType

	case lexer.NOT_TOKEN:
		// Logical not returns bool
		return types.TypeBool

	default:
		return xType
	}
}

// inferCallExprType determines the return type of a function call
func inferCallExprType(_ *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CallExpr) types.SemType {
	// Look up the function
	if ident, ok := expr.Fun.(*ast.IdentifierExpr); ok {
		sym, exists := mod.CurrentScope.Lookup(ident.Name)
		if !exists {
			return types.TypeUnknown
		}

		// Get the function declaration
		if funcDecl, ok := sym.Decl.(*ast.FuncDecl); ok {
			if funcDecl.Type != nil && funcDecl.Type.Result != nil {
				return typeFromTypeNode(funcDecl.Type.Result)
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

// inferSelectorExprType determines the type of a field access
func inferSelectorExprType(expr *ast.SelectorExpr) types.SemType {
	// TODO: Implement struct field type lookup
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
func inferCastExprType(expr *ast.CastExpr) types.SemType {
	return typeFromTypeNode(expr.Type)
}

// inferCompositeLitType determines the type of a composite literal
func inferCompositeLitType(lit *ast.CompositeLit) types.SemType {
	if lit.Type != nil {
		return typeFromTypeNode(lit.Type)
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
				value, err := strconv.ParseInt(lit.Value, 0, 64)
				if err == nil && fitsInType(value, expected) {
					return expected
				}
			}
		}
		// Can't fit or no context - return default
		return types.TypeI64

	case ast.FLOAT:
		if expected.Equals(types.TypeF32) || expected.Equals(types.TypeF64) {
			return expected
		}
		return types.TypeF64

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
		oldScope := mod.CurrentScope
		mod.CurrentScope = lit.Scope.(*table.SymbolTable)
		defer func() {
			mod.CurrentScope = oldScope
		}()
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

	// Type check the function body
	if lit.Body != nil {
		checkBlock(ctx, mod, lit.Body)
	}

	return types.NewFunction(params, returnType)
}
