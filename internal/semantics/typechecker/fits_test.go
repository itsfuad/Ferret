package typechecker

import (
	"compiler/internal/types"
	"testing"
)

func TestFitsInTypeBigIntLargeIntegers(t *testing.T) {
	tests := []struct {
		value    string
		typ      types.SemType
		expected bool
	}{
		{"100", types.TypeI128, true},
		{"200", types.TypeU128, true},
		{"300", types.TypeI256, true},
		{"400", types.TypeU256, true},
		{"-100", types.TypeI128, true},
		{"-100", types.TypeU128, false}, // negative doesn't fit in unsigned
		{"127", types.TypeI8, true},
		{"128", types.TypeI8, false},
		{"255", types.TypeU8, true},
		{"256", types.TypeU8, false},
		// Test large values
		{"170141183460469231731687303715884105727", types.TypeI128, true},  // max i128
		{"170141183460469231731687303715884105728", types.TypeI128, false}, // overflow i128
		{"340282366920938463463374607431768211455", types.TypeU128, true},  // max u128
		{"340282366920938463463374607431768211456", types.TypeU128, false}, // overflow u128
	}

	for _, tt := range tests {
		result := fitsInType(tt.value, tt.typ)
		if result != tt.expected {
			t.Errorf("fitsInTypeBigInt(%s, %s) = %v, want %v", tt.value, tt.typ, result, tt.expected)
		}
	}
}
