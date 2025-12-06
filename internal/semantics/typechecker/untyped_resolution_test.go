package typechecker

import (
	"compiler/internal/types"
	"testing"
)

func TestResolveUntypedInt(t *testing.T) {
	tests := []struct {
		name      string
		value     string
		wantType  types.TYPE_NAME
		wantError bool
	}{
		{
			name:     "small value fits in i32",
			value:    "100",
			wantType: types.TYPE_I32,
		},
		{
			name:     "i32 max",
			value:    "2147483647",
			wantType: types.TYPE_I32,
		},
		{
			name:     "i32 min",
			value:    "-2147483648",
			wantType: types.TYPE_I32,
		},
		{
			name:     "exceeds i32 max - needs i64",
			value:    "2147483648",
			wantType: types.TYPE_I64,
		},
		{
			name:     "large value needs i64",
			value:    "3000000000",
			wantType: types.TYPE_I64,
		},
		{
			name:     "i64 max",
			value:    "9223372036854775807",
			wantType: types.TYPE_I64,
		},
		{
			name:     "exceeds i64 - needs i128",
			value:    "9223372036854775808",
			wantType: types.TYPE_I128,
		},
		{
			name:     "very large value needs i128",
			value:    "20000000000000000000",
			wantType: types.TYPE_I128,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := resolveUntypedInt(tt.value)

			// Extract primitive type name
			name, ok := types.GetPrimitiveName(result)
			if !ok {
				t.Fatalf("resolveUntypedInt(%q) did not return a primitive type", tt.value)
			}

			if name != tt.wantType {
				t.Errorf("resolveUntypedInt(%q) = %s, want %s", tt.value, name, tt.wantType)
			}
		})
	}
}

func TestResolveUntypedIntDefaultsToI32(t *testing.T) {
	// Verify that DEFAULT_INT_TYPE is i32
	if types.DEFAULT_INT_TYPE != types.TYPE_I32 {
		t.Fatalf("Expected DEFAULT_INT_TYPE to be TYPE_I32, got %s", types.DEFAULT_INT_TYPE)
	}

	// Small values should default to i32
	result := resolveUntypedInt("42")
	name, ok := types.GetPrimitiveName(result)
	if !ok {
		t.Fatal("resolveUntypedInt did not return a primitive type")
	}

	if name != types.TYPE_I32 {
		t.Errorf("Small value should default to i32, got %s", name)
	}
}
