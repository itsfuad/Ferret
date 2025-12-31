package narrowing

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/tokens"
	"compiler/internal/types"
)

// OptionalNarrowingAnalyzer implements narrowing for optional types
type OptionalNarrowingAnalyzer struct{}

// NewOptionalNarrowingAnalyzer creates a new optional narrowing analyzer
func NewOptionalNarrowingAnalyzer() *OptionalNarrowingAnalyzer {
	return &OptionalNarrowingAnalyzer{}
}

// AnalyzeCondition recursively checks if a condition narrows optional or union types
func (a *OptionalNarrowingAnalyzer) AnalyzeCondition(ctx *context_v2.CompilerContext, mod *context_v2.Module, condition ast.Expression, parent *NarrowingContext) (*NarrowingContext, *NarrowingContext) {
	return analyzeConditionRecursive(ctx, mod, condition, parent)
}

// analyzeConditionRecursive recursively analyzes the condition
func analyzeConditionRecursive(ctx *context_v2.CompilerContext, mod *context_v2.Module, condition ast.Expression, parent *NarrowingContext) (*NarrowingContext, *NarrowingContext) {
	switch cond := condition.(type) {
	case *ast.BinaryExpr:
		switch cond.Op.Kind {
		case tokens.IS_TOKEN:
			return analyzeIsCondition(mod, cond, parent)
		case tokens.DOUBLE_EQUAL_TOKEN:
			return analyzeEqualityCondition(mod, cond, parent, true)
		case tokens.NOT_EQUAL_TOKEN:
			return analyzeEqualityCondition(mod, cond, parent, false)
		case tokens.OR_TOKEN:
			leftThen, leftElse := analyzeConditionRecursive(ctx, mod, cond.X, parent)
			rightThen, rightElse := analyzeConditionRecursive(ctx, mod, cond.Y, parent)
			thenNarrowing := mergeNarrowings(leftThen, rightThen)
			elseNarrowing := intersectNarrowings(leftElse, rightElse)
			return thenNarrowing, elseNarrowing
		case tokens.AND_TOKEN:
			leftThen, leftElse := analyzeConditionRecursive(ctx, mod, cond.X, parent)
			rightThen, rightElse := analyzeConditionRecursive(ctx, mod, cond.Y, parent)
			thenNarrowing := intersectNarrowings(leftThen, rightThen)
			elseNarrowing := mergeNarrowings(leftElse, rightElse)
			return thenNarrowing, elseNarrowing
		default:
			return parent, parent
		}
	case *ast.UnaryExpr:
		if cond.Op.Kind == tokens.NOT_TOKEN {
			thenNarrowing, elseNarrowing := analyzeConditionRecursive(ctx, mod, cond.X, parent)
			// Swap for negation
			return elseNarrowing, thenNarrowing
		}
		return parent, parent
	default:
		return parent, parent
	}
}

// analyzeIsCondition handles 'a is T' narrowing
func analyzeIsCondition(mod *context_v2.Module, binExpr *ast.BinaryExpr, parent *NarrowingContext) (*NarrowingContext, *NarrowingContext) {
	thenNarrowing := NewNarrowingContext(parent)
	elseNarrowing := NewNarrowingContext(parent)

	if ident, ok := binExpr.X.(*ast.IdentifierExpr); ok {
		varName := ident.Name
		if unionType := getUnionVarType(mod, varName); unionType != nil {
			if rhsIdent, ok := binExpr.Y.(*ast.IdentifierExpr); ok {
				variantName := rhsIdent.Name
				for i, variant := range unionType.Variants {
					if name, ok := types.GetPrimitiveName(variant); ok && string(name) == variantName {
						thenNarrowing.Narrow(varName, variant)
						otherVariants := excludeVariant(unionType.Variants, i)
						if len(otherVariants) == 1 {
							elseNarrowing.Narrow(varName, otherVariants[0])
						} else if len(otherVariants) > 1 {
							elseNarrowing.Narrow(varName, types.NewUnion(otherVariants))
						}
						break
					}
				}
			}
		}
	}
	return thenNarrowing, elseNarrowing
}

// analyzeEqualityCondition handles 'a == none' or 'a != none' narrowing
func analyzeEqualityCondition(mod *context_v2.Module, binExpr *ast.BinaryExpr, parent *NarrowingContext, isEqual bool) (*NarrowingContext, *NarrowingContext) {
	thenNarrowing := NewNarrowingContext(parent)
	elseNarrowing := NewNarrowingContext(parent)

	varName, isNoneCheck := isNoneComparison(binExpr)
	if isNoneCheck && varName != "" {
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
		} else {
			if optType := getOptionalVarType(mod, varName); optType != nil {
				if isEqual {
					thenNarrowing.Narrow(varName, types.TypeNone)
					elseNarrowing.Narrow(varName, optType.Inner)
				} else {
					thenNarrowing.Narrow(varName, optType.Inner)
					elseNarrowing.Narrow(varName, types.TypeNone)
				}
			}
		}
	}
	return thenNarrowing, elseNarrowing
}

// excludeVariant returns the variants excluding the one at index i
func excludeVariant(variants []types.SemType, i int) []types.SemType {
	result := make([]types.SemType, 0, len(variants)-1)
	for j, v := range variants {
		if j != i {
			result = append(result, v)
		}
	}
	return result
}

// mergeNarrowings merges two narrowing contexts (for OR)
func mergeNarrowings(a, b *NarrowingContext) *NarrowingContext {
	merged := NewNarrowingContext(nil)
	for varName := range a.NarrowedTypes {
		if aType, ok := a.GetNarrowedType(varName); ok {
			if bType, ok := b.GetNarrowedType(varName); ok {
				merged.Narrow(varName, unionTypes(aType, bType))
			} else {
				merged.Narrow(varName, aType)
			}
		}
	}
	for varName := range b.NarrowedTypes {
		if _, ok := a.GetNarrowedType(varName); !ok {
			if bType, ok := b.GetNarrowedType(varName); ok {
				merged.Narrow(varName, bType)
			}
		}
	}
	return merged
}

// intersectNarrowings intersects two narrowing contexts (for OR else branches)
func intersectNarrowings(a, b *NarrowingContext) *NarrowingContext {
	intersected := NewNarrowingContext(nil)
	for varName := range a.NarrowedTypes {
		if aType, ok := a.GetNarrowedType(varName); ok {
			if bType, ok := b.GetNarrowedType(varName); ok {
				if intersectedType := intersectTypes(aType, bType); intersectedType != nil {
					intersected.Narrow(varName, intersectedType)
				}
			}
		}
	}
	return intersected
}

// unionTypes returns the union of two types
func unionTypes(a, b types.SemType) types.SemType {
	if a.Equals(b) {
		return a
	}
	// Simple case: if both are primitives or unions, combine
	variants := []types.SemType{a}
	if !containsType(variants, b) {
		variants = append(variants, b)
	}
	if len(variants) == 1 {
		return variants[0]
	}
	return types.NewUnion(variants)
}

// intersectTypes returns the intersection of two types
func intersectTypes(a, b types.SemType) types.SemType {
	if a.Equals(b) {
		return a
	}
	// For unions, intersection would be complex; for now, return nil if no overlap
	return nil
}

// containsType checks if a slice contains a type
func containsType(types []types.SemType, t types.SemType) bool {
	for _, typ := range types {
		if typ.Equals(t) {
			return true
		}
	}
	return false
}

// isNoneComparison checks if a binary expression is comparing a variable to "none"
// Returns the variable name and true if it's a none comparison
func isNoneComparison(binExpr *ast.BinaryExpr) (string, bool) {
	// Check left side: varName != none
	if ident, ok := binExpr.X.(*ast.IdentifierExpr); ok {
		if isNoneLiteral(binExpr.Y) {
			return ident.Name, true
		}
	}

	// Check right side: none != varName
	if ident, ok := binExpr.Y.(*ast.IdentifierExpr); ok {
		if isNoneLiteral(binExpr.X) {
			return ident.Name, true
		}
	}

	return "", false
}

// isNoneLiteral checks if an expression is the "none" literal
func isNoneLiteral(expr ast.Expression) bool {
	if ident, ok := expr.(*ast.IdentifierExpr); ok {
		return ident.Name == "none"
	}
	return false
}

// getOptionalVarType gets the optional type of a variable if it exists
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

// getUnionVarType gets the union type of a variable if it exists
func getUnionVarType(mod *context_v2.Module, varName string) *types.UnionType {
	sym, ok := mod.CurrentScope.Lookup(varName)
	if !ok {
		return nil
	}

	if unionType, ok := sym.Type.(*types.UnionType); ok {
		return unionType
	}

	return nil
}
