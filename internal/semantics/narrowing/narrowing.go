package narrowing

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/types"
)

// NarrowingContext tracks type narrowing information for variables within a scope.
// Used for type narrowing based on conditions.
type NarrowingContext struct {
	// NarrowedTypes maps variable names to their narrowed types
	NarrowedTypes map[string]types.SemType

	// Parent allows nested scopes (for nested if statements, loops, etc.)
	Parent *NarrowingContext
}

// NewNarrowingContext creates a new narrowing context
func NewNarrowingContext(parent *NarrowingContext) *NarrowingContext {
	return &NarrowingContext{
		NarrowedTypes: make(map[string]types.SemType),
		Parent:        parent,
	}
}

// Narrow records that a variable has been narrowed to a specific type
func (nc *NarrowingContext) Narrow(varName string, narrowedType types.SemType) {
	if nc == nil {
		return
	}
	nc.NarrowedTypes[varName] = narrowedType
}

// GetNarrowedType returns the narrowed type for a variable, if any
func (nc *NarrowingContext) GetNarrowedType(varName string) (types.SemType, bool) {
	if nc == nil {
		return nil, false
	}

	// Check current scope
	if typ, ok := nc.NarrowedTypes[varName]; ok {
		return typ, true
	}

	// Check parent scopes
	if nc.Parent != nil {
		return nc.Parent.GetNarrowedType(varName)
	}

	return nil, false
}

// NarrowingAnalyzer defines the interface for type-specific narrowing analysis
type NarrowingAnalyzer interface {
	// AnalyzeCondition checks if a condition narrows types and returns then/else contexts
	AnalyzeCondition(ctx *context_v2.CompilerContext, mod *context_v2.Module, condition ast.Expression, parent *NarrowingContext) (*NarrowingContext, *NarrowingContext)
}
