package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/tokens"
	"compiler/internal/types"
)

// NarrowingContext tracks type narrowing information for variables within a scope.
// Used for optional type narrowing (T? → T) based on null checks.
type NarrowingContext struct {
	// narrowedTypes maps variable names to their narrowed types
	// Example: after "if a != none", narrowedTypes["a"] = T (unwrapped from T?)
	narrowedTypes map[string]types.SemType

	// parent allows nested scopes (for nested if statements, loops, etc.)
	parent *NarrowingContext
}

// NewNarrowingContext creates a new narrowing context
func NewNarrowingContext(parent *NarrowingContext) *NarrowingContext {
	return &NarrowingContext{
		narrowedTypes: make(map[string]types.SemType),
		parent:        parent,
	}
}

// Narrow records that a variable has been narrowed to a specific type
func (nc *NarrowingContext) Narrow(varName string, narrowedType types.SemType) {
	if nc == nil {
		return
	}
	nc.narrowedTypes[varName] = narrowedType
}

// GetNarrowedType returns the narrowed type for a variable, if any
func (nc *NarrowingContext) GetNarrowedType(varName string) (types.SemType, bool) {
	if nc == nil {
		return nil, false
	}

	// Check current scope
	if typ, ok := nc.narrowedTypes[varName]; ok {
		return typ, true
	}

	// Check parent scopes
	if nc.parent != nil {
		return nc.parent.GetNarrowedType(varName)
	}

	return nil, false
}

// analyzeConditionForNarrowing checks if a condition narrows optional types
// Returns two contexts: one for the "then" branch, one for the "else" branch
func analyzeConditionForNarrowing(_ *context_v2.CompilerContext, mod *context_v2.Module,
	condition ast.Expression, parentNarrowing *NarrowingContext) (*NarrowingContext, *NarrowingContext) {

	thenNarrowing := NewNarrowingContext(parentNarrowing)
	elseNarrowing := NewNarrowingContext(parentNarrowing)

	// Check for patterns like: a != none, a == none, none != a, none == a
	if binExpr, ok := condition.(*ast.BinaryExpr); ok {
		switch binExpr.Op.Kind {
		case tokens.NOT_EQUAL_TOKEN:
			// a != none → a is non-optional in then branch, none in else branch
			varName, isNoneCheck := isNoneComparison(binExpr)
			if isNoneCheck && varName != "" {
				if optType := getOptionalVarType(mod, varName); optType != nil {
					thenNarrowing.Narrow(varName, optType.Inner) // Unwrap to T
					// In else branch, a is definitely none (stays T?)
				}
			}

		case tokens.DOUBLE_EQUAL_TOKEN:
			// a == none → a is none in then branch, non-optional in else branch
			varName, isNoneCheck := isNoneComparison(binExpr)
			if isNoneCheck && varName != "" {
				if optType := getOptionalVarType(mod, varName); optType != nil {
					// In then branch, a is definitely none (stays T?)
					elseNarrowing.Narrow(varName, optType.Inner) // Unwrap to T in else
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