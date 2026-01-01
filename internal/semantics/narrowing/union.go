package narrowing

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/types"
)

// analyzeIsCondition handles 'x is Type' narrowing for unions and interfaces.
func analyzeIsCondition(mod *context_v2.Module, binExpr *ast.BinaryExpr, parent *NarrowingContext) (*NarrowingContext, *NarrowingContext) {
	thenNarrowing := NewNarrowingContext(parent)
	elseNarrowing := NewNarrowingContext(parent)

	ident, ok := binExpr.X.(*ast.IdentifierExpr)
	if !ok {
		return thenNarrowing, elseNarrowing
	}
	varName := ident.Name

	// Try union narrowing first
	if unionType := getUnionVarType(mod, varName); unionType != nil {
		return analyzeUnionIsCondition(varName, unionType, binExpr, thenNarrowing, elseNarrowing, mod)
	}

	// Try interface narrowing
	if interfaceType := getInterfaceVarType(mod, varName); interfaceType != nil && isEmptyInterface(interfaceType) {
		return analyzeInterfaceIsCondition(varName, binExpr, thenNarrowing, elseNarrowing, mod)
	}

	return thenNarrowing, elseNarrowing
}

// analyzeUnionIsCondition handles 'x is Type' for union types.
func analyzeUnionIsCondition(varName string, unionType *types.UnionType, binExpr *ast.BinaryExpr, thenNarrowing, elseNarrowing *NarrowingContext, mod *context_v2.Module) (*NarrowingContext, *NarrowingContext) {
	targetType := getTargetTypeFromRHS(mod, binExpr.Y)
	if targetType == nil {
		return thenNarrowing, elseNarrowing
	}

	// Find which variant matches the target type
	for i, variant := range unionType.Variants {
		if targetType.Equals(variant) {
			// Then branch: narrow to the matched variant
			thenNarrowing.Narrow(varName, variant)

			// Else branch: narrow to remaining variants
			otherVariants := excludeVariant(unionType.Variants, i)
			if len(otherVariants) == 1 {
				elseNarrowing.Narrow(varName, otherVariants[0])
			} else if len(otherVariants) > 1 {
				elseNarrowing.Narrow(varName, types.NewUnion(otherVariants))
			}
			break
		}
	}

	return thenNarrowing, elseNarrowing
}

// analyzeInterfaceIsCondition handles 'x is Type' for interface{} types.
func analyzeInterfaceIsCondition(varName string, binExpr *ast.BinaryExpr, thenNarrowing, elseNarrowing *NarrowingContext, mod *context_v2.Module) (*NarrowingContext, *NarrowingContext) {
	targetType := getTargetTypeFromRHS(mod, binExpr.Y)
	if targetType == nil {
		return thenNarrowing, elseNarrowing
	}

	// Then branch: narrow to the target type
	thenNarrowing.Narrow(varName, targetType)
	// Else branch: can't narrow (still interface{})

	return thenNarrowing, elseNarrowing
}

// getTargetTypeFromRHS extracts the target type from the RHS of an 'is' expression.
func getTargetTypeFromRHS(mod *context_v2.Module, rhs ast.Expression) types.SemType {
	// Handle TypeExpr (parser format: x is []interface{})
	if typeExpr, ok := rhs.(*ast.TypeExpr); ok {
		return getTypeFromTypeNode(mod, typeExpr.Type)
	}

	// Handle IdentifierExpr (old format: x is str)
	if rhsIdent, ok := rhs.(*ast.IdentifierExpr); ok {
		return getPrimitiveTypeByName(rhsIdent.Name)
	}

	return nil
}

// excludeVariant returns the variants excluding the one at index i.
func excludeVariant(variants []types.SemType, i int) []types.SemType {
	result := make([]types.SemType, 0, len(variants)-1)
	for j, v := range variants {
		if j != i {
			result = append(result, v)
		}
	}
	return result
}

// getUnionVarType gets the union type of a variable if it exists.
func getUnionVarType(mod *context_v2.Module, varName string) *types.UnionType {
	sym, ok := mod.CurrentScope.Lookup(varName)
	if !ok {
		return nil
	}

	unwrapped := types.UnwrapType(sym.Type)
	if unionType, ok := unwrapped.(*types.UnionType); ok {
		return unionType
	}

	return nil
}

// getInterfaceVarType gets the interface type of a variable if it exists.
func getInterfaceVarType(mod *context_v2.Module, varName string) *types.InterfaceType {
	sym, ok := mod.CurrentScope.Lookup(varName)
	if !ok {
		return nil
	}

	unwrapped := types.UnwrapType(sym.Type)
	if interfaceType, ok := unwrapped.(*types.InterfaceType); ok {
		return interfaceType
	}

	return nil
}

// isEmptyInterface checks if an interface type has no methods (is interface{}).
func isEmptyInterface(interfaceType *types.InterfaceType) bool {
	return interfaceType != nil && len(interfaceType.Methods) == 0
}
