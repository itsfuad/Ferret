package narrowing

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/types"
)

// analyzeEqualityCondition handles 'x == none' or 'x != none' narrowing for optionals.
func analyzeEqualityCondition(mod *context_v2.Module, binExpr *ast.BinaryExpr, parent *NarrowingContext, isEqual bool) (*NarrowingContext, *NarrowingContext) {
	thenNarrowing := NewNarrowingContext(parent)
	elseNarrowing := NewNarrowingContext(parent)

	varName, isNoneCheck := isNoneComparison(binExpr)
	if !isNoneCheck || varName == "" {
		return thenNarrowing, elseNarrowing
	}

	// Check if there's already a parent narrowing for this variable
	parentNarrowedType, hasParentNarrowing := parent.GetNarrowedType(varName)
	if hasParentNarrowing {
		if parentNarrowedType.Equals(types.TypeNone) {
			if isEqual {
				thenNarrowing.Narrow(varName, types.TypeNone)
			} else {
				elseNarrowing.Narrow(varName, types.TypeNone)
			}
		} else {
			if isEqual {
				elseNarrowing.Narrow(varName, parentNarrowedType)
			} else {
				thenNarrowing.Narrow(varName, parentNarrowedType)
			}
		}
		return thenNarrowing, elseNarrowing
	}

	// Check if the variable is an optional type
	optType := getOptionalVarType(mod, varName)
	if optType == nil {
		return thenNarrowing, elseNarrowing
	}

	// x == none: then gets none, else gets inner type
	// x != none: then gets inner type, else gets none
	if isEqual {
		thenNarrowing.Narrow(varName, types.TypeNone)
		elseNarrowing.Narrow(varName, optType.Inner)
	} else {
		thenNarrowing.Narrow(varName, optType.Inner)
		elseNarrowing.Narrow(varName, types.TypeNone)
	}

	return thenNarrowing, elseNarrowing
}

// isNoneComparison checks if a binary expression is comparing a variable to "none".
// Returns the variable name and true if it's a none comparison.
func isNoneComparison(binExpr *ast.BinaryExpr) (string, bool) {
	// Check left side: varName == none / varName != none
	if ident, ok := binExpr.X.(*ast.IdentifierExpr); ok {
		if isNoneLiteral(binExpr.Y) {
			return ident.Name, true
		}
	}

	// Check right side: none == varName / none != varName
	if ident, ok := binExpr.Y.(*ast.IdentifierExpr); ok {
		if isNoneLiteral(binExpr.X) {
			return ident.Name, true
		}
	}

	return "", false
}

// isNoneLiteral checks if an expression is the "none" literal.
func isNoneLiteral(expr ast.Expression) bool {
	if ident, ok := expr.(*ast.IdentifierExpr); ok {
		return ident.Name == "none"
	}
	return false
}

// getOptionalVarType gets the optional type of a variable if it exists.
func getOptionalVarType(mod *context_v2.Module, varName string) *types.OptionalType {
	sym, ok := mod.CurrentScope.Lookup(varName)
	if !ok {
		return nil
	}

	if optType, ok := sym.Type.(*types.OptionalType); ok {
		return optType
	}

	return nil
}
