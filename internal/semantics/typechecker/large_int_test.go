package typechecker

import (
	"compiler/internal/types"
	"testing"
)

func TestUntypedIntCompatibilityWithLargeIntegers(t *testing.T) {
	// Test that untyped integers are assignable to large integer types
	untypedInt := types.TypeUntypedInt

	tests := []struct {
		target types.SemType
		name   string
	}{
		{types.TypeI128, "i128"},
		{types.TypeU128, "u128"},
		{types.TypeI256, "i256"},
		{types.TypeU256, "u256"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			compat := checkTypeCompatibility(untypedInt, tt.target)
			if compat != Assignable {
				t.Errorf("Untyped int should be assignable to %s, got %v", tt.name, compat)
			}
		})
	}
}
