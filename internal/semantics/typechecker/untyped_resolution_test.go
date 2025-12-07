package typechecker

import (
	"compiler/internal/frontend/ast"
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
			name:     "small value fits in i16",
			value:    "100",
			wantType: types.TYPE_I16, // 100 fits in i16 (minimum type)
		},
		{
			name:     "i32 max",
			value:    "2147483647",
			wantType: types.TYPE_I64, // i32 max needs i64 for "u32 or i64"
		},
		{
			name:     "i32 min",
			value:    "-2147483648",
			wantType: types.TYPE_I32, // Negative values use signed types
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
			wantType: types.TYPE_I128, // i64 max needs i128 for "u64 or i128"
		},
		{
			name:     "exceeds i64 - needs i128",
			value:    "9223372036854775808",
			wantType: types.TYPE_I128,
		},
		{
			name:     "very large value needs i128",
			value:    "20000000000000000000",
			wantType: types.TYPE_I256, // Exceeds i128, needs i256
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lit := &ast.BasicLit{
				Kind:  ast.INT,
				Value: tt.value,
			}
			result := resolveUntyped(lit, types.TypeUnknown)

			// Extract primitive type name
			name, ok := types.GetPrimitiveName(result)
			if !ok {
				t.Fatalf("resolveUntyped(%q) did not return a primitive type", tt.value)
			}

			if name != tt.wantType {
				t.Errorf("resolveUntyped(%q) = %s, want %s", tt.value, name, tt.wantType)
			}
		})
	}
}

func TestResolveUntypedIntDefaultsToI32(t *testing.T) {
	// Verify that DEFAULT_INT_TYPE is i32
	if types.DEFAULT_INT_TYPE != types.TYPE_I32 {
		t.Fatalf("Expected DEFAULT_INT_TYPE to be TYPE_I32, got %s", types.DEFAULT_INT_TYPE)
	}

	// Small values should use minimum type (i16 for 42)
	lit := &ast.BasicLit{
		Kind:  ast.INT,
		Value: "42",
	}
	result := resolveUntyped(lit, types.TypeUnknown)
	name, ok := types.GetPrimitiveName(result)
	if !ok {
		t.Fatal("resolveUntyped did not return a primitive type")
	}

	if name != types.TYPE_I16 {
		t.Errorf("Small value should use minimum type i16, got %s", name)
	}
}
