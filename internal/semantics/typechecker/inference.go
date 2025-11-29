package typechecker

import (
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/types"
	"fmt"
	"strconv"
)

// inferExprType determines the type of an expression based on its structure and value.
// This is pure type inference without any compatibility checking.
// Returns TYPE_UNTYPED for literals that haven't been contextualized yet.
func (tc *typeChecker) inferExprType(expr ast.Expression) types.SemType {
	if expr == nil {
		return types.TypeUnknown
	}

	switch e := expr.(type) {
	case *ast.BasicLit:
		return tc.inferLiteralType(e)

	case *ast.IdentifierExpr:
		return tc.inferIdentifierType(e)

	case *ast.BinaryExpr:
		return tc.inferBinaryExprType(e)

	case *ast.UnaryExpr:
		return tc.inferUnaryExprType(e)

	case *ast.CallExpr:
		return tc.inferCallExprType(e)

	case *ast.IndexExpr:
		return tc.inferIndexExprType(e)

	case *ast.SelectorExpr:
		return tc.inferSelectorExprType(e)

	case *ast.CastExpr:
		return tc.inferCastExprType(e)

	case *ast.ParenExpr:
		return tc.inferExprType(e.X)

	case *ast.CompositeLit:
		return tc.inferCompositeLitType(e)

	default:
		return types.TypeUnknown
	}
}

// inferLiteralType determines the type of a literal
func (tc *typeChecker) inferLiteralType(lit *ast.BasicLit) types.SemType {
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
func (tc *typeChecker) inferIdentifierType(ident *ast.IdentifierExpr) types.SemType {
	sym, ok := tc.currentScope.Lookup(ident.Name)
	if !ok {
		// Report error with good context
		tc.ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("undefined: %s", ident.Name)).
				WithPrimaryLabel(tc.mod.FilePath, ident.Loc(), "not declared in this scope"),
		)
		return types.TypeUnknown
	}
	return sym.Type
}

// inferBinaryExprType determines the result type of a binary expression
func (tc *typeChecker) inferBinaryExprType(expr *ast.BinaryExpr) types.SemType {
	lhsType := tc.getOrInferType(expr.X)
	rhsType := tc.getOrInferType(expr.Y)

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
func (tc *typeChecker) inferUnaryExprType(expr *ast.UnaryExpr) types.SemType {
	xType := tc.getOrInferType(expr.X)

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
func (tc *typeChecker) inferCallExprType(expr *ast.CallExpr) types.SemType {
	// Look up the function
	if ident, ok := expr.Fun.(*ast.IdentifierExpr); ok {
		sym, exists := tc.currentScope.Lookup(ident.Name)
		if !exists {
			return types.TypeUnknown
		}

		// Get the function declaration
		if funcDecl, ok := sym.Decl.(*ast.FuncDecl); ok {
			if funcDecl.Type != nil && funcDecl.Type.Result != nil {
				return tc.typeFromTypeNode(funcDecl.Type.Result)
			}
		}
	}

	return types.TypeUnknown
}

// inferIndexExprType determines the element type of an index expression
func (tc *typeChecker) inferIndexExprType(expr *ast.IndexExpr) types.SemType {
	// TODO: Implement array/map element type extraction
	return types.TypeUnknown
}

// inferSelectorExprType determines the type of a field access
func (tc *typeChecker) inferSelectorExprType(expr *ast.SelectorExpr) types.SemType {
	// TODO: Implement struct field type lookup
	return types.TypeUnknown
}

// inferCastExprType determines the target type of a cast
func (tc *typeChecker) inferCastExprType(expr *ast.CastExpr) types.SemType {
	return tc.typeFromTypeNode(expr.Type)
}

// inferCompositeLitType determines the type of a composite literal
func (tc *typeChecker) inferCompositeLitType(lit *ast.CompositeLit) types.SemType {
	if lit.Type != nil {
		return tc.typeFromTypeNode(lit.Type)
	}
	return types.TypeUnknown
}

// getOrInferType gets the cached type or infers it
func (tc *typeChecker) getOrInferType(expr ast.Expression) types.SemType {
	if t, ok := tc.exprTypes[expr]; ok {
		return t
	}
	return tc.inferExprType(expr)
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
func (tc *typeChecker) finalizeUntyped(expr ast.Expression) types.SemType {
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
func (tc *typeChecker) contextualizeUntyped(lit *ast.BasicLit, expected types.SemType) types.SemType {
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
