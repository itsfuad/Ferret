package typechecker

import (
	"compiler/internal/frontend/ast"
	"compiler/internal/source"
	"compiler/internal/types"
	"testing"
)

func TestTypeFromTypeNodeLargeIntegers(t *testing.T) {
	tests := []struct {
		typeName string
		expected types.SemType
	}{
		{"i128", types.TypeI128},
		{"u128", types.TypeU128},
		{"i256", types.TypeI256},
		{"u256", types.TypeU256},
	}

	for _, tt := range tests {
		t.Run(tt.typeName, func(t *testing.T) {
			// Create an AST IdentifierExpr representing the type name
			typeNode := &ast.IdentifierExpr{
				Name:     tt.typeName,
				Location: source.Location{},
			}

			result := typeFromTypeNode(typeNode)
			if !result.Equals(tt.expected) {
				t.Errorf("typeFromTypeNode(%s) = %s, want %s", tt.typeName, result, tt.expected)
			}

			// Verify it's numeric
			if !types.IsNumeric(result) {
				t.Errorf("typeFromTypeNode(%s) should be numeric", tt.typeName)
			}
		})
	}
}
