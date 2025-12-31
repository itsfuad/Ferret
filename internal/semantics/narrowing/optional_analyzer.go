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

		// First check for union types
		if unionType := getUnionVarType(mod, varName); unionType != nil {
			// Get the target type from RHS
			var targetType types.SemType

			// Handle TypeExpr (new parser format: iter is []interface{})
			if typeExpr, ok := binExpr.Y.(*ast.TypeExpr); ok {
				targetType = getTypeFromTypeNode(mod, typeExpr.Type)
			} else if rhsIdent, ok := binExpr.Y.(*ast.IdentifierExpr); ok {
				// Handle IdentifierExpr (old format: iter is str)
				variantName := rhsIdent.Name
				// Try to match by primitive type name
				for _, variant := range unionType.Variants {
					if name, ok := types.GetPrimitiveName(variant); ok && string(name) == variantName {
						targetType = variant
						break
					}
				}
			}

			// If we found a target type, check if it matches a variant
			if targetType != nil {
				for i, variant := range unionType.Variants {
					if targetType.Equals(variant) {
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
			return thenNarrowing, elseNarrowing
		}

		// Then check for interface{} types
		if interfaceType := getInterfaceVarType(mod, varName); interfaceType != nil && isEmptyInterface(interfaceType) {
			// Get the target type from RHS
			var targetType types.SemType

			// Handle TypeExpr (new parser format: val is i32)
			if typeExpr, ok := binExpr.Y.(*ast.TypeExpr); ok {
				targetType = getTypeFromTypeNode(mod, typeExpr.Type)
			} else if rhsIdent, ok := binExpr.Y.(*ast.IdentifierExpr); ok {
				// Handle IdentifierExpr (old format: val is str)
				targetType = getPrimitiveTypeByName(rhsIdent.Name)
			}

			// In the 'then' branch, narrow to the target type
			if targetType != nil {
				thenNarrowing.Narrow(varName, targetType)
			}
			// In the 'else' branch, we can't narrow (still interface{})
			return thenNarrowing, elseNarrowing
		}
	}
	return thenNarrowing, elseNarrowing
}

// getTypeFromTypeNode converts an AST type node to a semantic type
func getTypeFromTypeNode(mod *context_v2.Module, typeNode ast.TypeNode) types.SemType {
	if typeNode == nil {
		return nil
	}

	// This is a simplified version - in production, you'd use the full TypeFromTypeNodeWithContext
	// For now, handle the common cases
	switch t := typeNode.(type) {
	case *ast.IdentifierExpr:
		// Type name like "str", "i32", etc.
		typeName := types.TYPE_NAME(t.Name)
		switch typeName {
		case types.TYPE_I8:
			return types.TypeI8
		case types.TYPE_I16:
			return types.TypeI16
		case types.TYPE_I32:
			return types.TypeI32
		case types.TYPE_I64:
			return types.TypeI64
		case types.TYPE_U8:
			return types.TypeU8
		case types.TYPE_U16:
			return types.TypeU16
		case types.TYPE_U32:
			return types.TypeU32
		case types.TYPE_U64:
			return types.TypeU64
		case types.TYPE_F32:
			return types.TypeF32
		case types.TYPE_F64:
			return types.TypeF64
		case types.TYPE_BOOL:
			return types.TypeBool
		case types.TYPE_STRING:
			return types.TypeString
		case types.TYPE_BYTE:
			return types.TypeByte
		}
		// Could be a named type - look it up in the module
		if sym, ok := mod.CurrentScope.Lookup(t.Name); ok {
			return sym.Type
		}
	case *ast.ArrayType:
		// Array type like []T
		elemType := getTypeFromTypeNode(mod, t.ElType)
		if elemType != nil {
			length := -1 // dynamic array
			if t.Len != nil {
				// Fixed-size array - would need to evaluate the length expression
				// For now, assume dynamic
			}
			return types.NewArray(elemType, length)
		}
	case *ast.MapType:
		// Map type like map[K]V
		keyType := getTypeFromTypeNode(mod, t.Key)
		valueType := getTypeFromTypeNode(mod, t.Value)
		if keyType != nil && valueType != nil {
			return types.NewMap(keyType, valueType)
		}
	case *ast.InterfaceType:
		// Interface type
		if len(t.Methods) == 0 {
			// Empty interface
			return types.NewInterface(nil)
		}
	}

	return nil
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

	unwrapped := types.UnwrapType(sym.Type)
	if unionType, ok := unwrapped.(*types.UnionType); ok {
		return unionType
	}

	return nil
}

// getInterfaceVarType gets the interface type of a variable if it exists
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

// isEmptyInterface checks if an interface type has no methods (is interface{})
func isEmptyInterface(interfaceType *types.InterfaceType) bool {
	return interfaceType != nil && len(interfaceType.Methods) == 0
}

// getPrimitiveTypeByName returns a primitive type by its name string
func getPrimitiveTypeByName(name string) types.SemType {
	typeName := types.TYPE_NAME(name)
	switch typeName {
	case types.TYPE_I8:
		return types.TypeI8
	case types.TYPE_I16:
		return types.TypeI16
	case types.TYPE_I32:
		return types.TypeI32
	case types.TYPE_I64:
		return types.TypeI64
	case types.TYPE_U8:
		return types.TypeU8
	case types.TYPE_U16:
		return types.TypeU16
	case types.TYPE_U32:
		return types.TypeU32
	case types.TYPE_U64:
		return types.TypeU64
	case types.TYPE_F32:
		return types.TypeF32
	case types.TYPE_F64:
		return types.TypeF64
	case types.TYPE_STRING:
		return types.TypeString
	case types.TYPE_BOOL:
		return types.TypeBool
	case types.TYPE_BYTE:
		return types.TypeByte
	default:
		return nil
	}
}
