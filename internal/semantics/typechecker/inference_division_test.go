package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/tokens"
	"compiler/internal/types"
	"testing"
)

func TestDivisionInference(t *testing.T) {
	tests := []struct {
		name     string
		lhs      types.SemType
		rhs      types.SemType
		expected types.SemType
	}{
		{
			name:     "i256_over_i256",
			lhs:      types.TypeI256,
			rhs:      types.TypeI256,
			expected: types.TypeI256,
		},
		{
			name:     "u256_over_u256",
			lhs:      types.TypeU256,
			rhs:      types.TypeU256,
			expected: types.TypeU256,
		},
		{
			name:     "f256_over_f256",
			lhs:      types.TypeF256,
			rhs:      types.TypeF256,
			expected: types.TypeF256,
		},
		{
			name:     "f128_over_f256",
			lhs:      types.TypeF128,
			rhs:      types.TypeF256,
			expected: types.TypeF256,
		},
		{
			name:     "i32_over_i32",
			lhs:      types.TypeI32,
			rhs:      types.TypeI32,
			expected: types.TypeF32,
		},
		{
			name:     "i64_over_i64",
			lhs:      types.TypeI64,
			rhs:      types.TypeI64,
			expected: types.TypeF64,
		},
		{
			name:     "i128_over_i64",
			lhs:      types.TypeI128,
			rhs:      types.TypeI64,
			expected: types.TypeI128,
		},
		{
			name:     "f256_over_i64",
			lhs:      types.TypeF256,
			rhs:      types.TypeI64,
			expected: types.TypeF256,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mod := moduleWithBinaryOperands(tt.lhs, tt.rhs)
			expr := &ast.BinaryExpr{
				X:  &ast.IdentifierExpr{Name: "a"},
				Op: tokens.Token{Kind: tokens.DIV_TOKEN, Value: "/"},
				Y:  &ast.IdentifierExpr{Name: "b"},
			}
			got := inferBinaryExprType(nil, mod, expr)
			if !got.Equals(tt.expected) {
				t.Fatalf("expected %s, got %s", tt.expected.String(), got.String())
			}
		})
	}
}

func moduleWithBinaryOperands(lhs, rhs types.SemType) *context_v2.Module {
	scope := table.NewSymbolTable(nil)
	_ = scope.Declare("a", &symbols.Symbol{
		Name: "a",
		Kind: symbols.SymbolVariable,
		Type: lhs,
	})
	_ = scope.Declare("b", &symbols.Symbol{
		Name: "b",
		Kind: symbols.SymbolVariable,
		Type: rhs,
	})
	return &context_v2.Module{CurrentScope: scope}
}
