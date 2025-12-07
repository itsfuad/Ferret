package typechecker

import (
	"compiler/internal/frontend/ast"
	"compiler/internal/types"
	"testing"
)

func TestContextualizeUntypedWithLargeIntegers(t *testing.T) {
	// Create a basic literal
	lit := &ast.BasicLit{
		Kind:  ast.INT,
		Value: "100",
	}

	tests := []struct {
		expected types.SemType
		name     string
	}{
		{types.TypeI128, "i128"},
		{types.TypeU128, "u128"},
		{types.TypeI256, "i256"},
		{types.TypeU256, "u256"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := resolveUntyped(lit, tt.expected)
			if !result.Equals(tt.expected) {
				t.Errorf("resolveUntyped(100, %s) = %s, want %s", tt.name, result, tt.expected)
			}
		})
	}
}
