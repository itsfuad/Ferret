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

// AnalyzeCondition checks if a condition narrows optional or union types
func (a *OptionalNarrowingAnalyzer) AnalyzeCondition(ctx *context_v2.CompilerContext, mod *context_v2.Module, condition ast.Expression, parent *NarrowingContext) (*NarrowingContext, *NarrowingContext) {
	thenNarrowing := NewNarrowingContext(parent)
	elseNarrowing := NewNarrowingContext(parent)

	// Check for patterns like: a != none, a == none, none != a, none == a
	if binExpr, ok := condition.(*ast.BinaryExpr); ok {
		switch binExpr.Op.Kind {
		case tokens.NOT_EQUAL_TOKEN:
			// a != none → a is non-optional in then branch, none in else branch
			varName, isNoneCheck := isNoneComparison(binExpr)
			if isNoneCheck && varName != "" {
				// Check if variable has been narrowed in parent context
				parentNarrowedType, hasParentNarrowing := parent.GetNarrowedType(varName)

				if hasParentNarrowing {
					// If already narrowed to none, we can't further narrow
					if parentNarrowedType.Equals(types.TypeNone) {
						// Variable is already none, so a != none is always false
						// then branch is unreachable, else branch stays none
						elseNarrowing.Narrow(varName, types.TypeNone) // Explicitly set to ensure it's applied
					} else {
						// Variable was narrowed to T (non-optional), so a != none is always true
						// then branch gets T (already from parent), else branch is unreachable
						thenNarrowing.Narrow(varName, parentNarrowedType) // Explicitly set to ensure it's applied
					}
				} else {
					// Not narrowed in parent, check if it's optional in symbol table
					if optType := getOptionalVarType(mod, varName); optType != nil {
						thenNarrowing.Narrow(varName, optType.Inner)  // Unwrap to T
						elseNarrowing.Narrow(varName, types.TypeNone) // Narrow to none in else branch
					}
				}
			}

		case tokens.DOUBLE_EQUAL_TOKEN:
			// a == none → a is none in then branch, non-optional in else branch
			varName, isNoneCheck := isNoneComparison(binExpr)
			if isNoneCheck && varName != "" {
				// Check if variable has been narrowed in parent context
				parentNarrowedType, hasParentNarrowing := parent.GetNarrowedType(varName)

				if hasParentNarrowing {
					// If already narrowed to none, we can't further narrow
					if parentNarrowedType.Equals(types.TypeNone) {
						// Variable is already none, so a == none is always true
						// then branch gets none (already from parent), else branch is unreachable
						thenNarrowing.Narrow(varName, types.TypeNone) // Explicitly set to ensure it's applied
					} else {
						// Variable was narrowed to T (non-optional), so a == none is always false
						// then branch is unreachable, else branch gets T (already from parent)
						elseNarrowing.Narrow(varName, parentNarrowedType) // Explicitly set to ensure it's applied
					}
				} else {
					// Not narrowed in parent, check if it's optional in symbol table
					if optType := getOptionalVarType(mod, varName); optType != nil {
						thenNarrowing.Narrow(varName, types.TypeNone) // Narrow to none in then branch
						elseNarrowing.Narrow(varName, optType.Inner)  // Unwrap to T in else
					}
				}
			}

		case tokens.IS_TOKEN:
			// a is T → a is narrowed to T in then branch, to union without T in else branch
			if ident, ok := binExpr.X.(*ast.IdentifierExpr); ok {
				varName := ident.Name
				// Check if it's a union
				if unionType := getUnionVarType(mod, varName); unionType != nil {
					// Assume rhs is ident of the variant type
					if rhsIdent, ok := binExpr.Y.(*ast.IdentifierExpr); ok {
						variantName := rhsIdent.Name
						// Find the matching variant
						for i, variant := range unionType.Variants {
							if name, ok := types.GetPrimitiveName(variant); ok && string(name) == variantName {
								// Narrow then to this variant
								thenNarrowing.Narrow(varName, variant)
								// Narrow else to union without this variant
								otherVariants := make([]types.SemType, 0, len(unionType.Variants)-1)
								for j, v := range unionType.Variants {
									if j != i {
										otherVariants = append(otherVariants, v)
									}
								}
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
		}
	}

	return thenNarrowing, elseNarrowing
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
