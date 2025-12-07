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
			name:     "small value uses default i32",
			value:    "100",
			wantType: types.TYPE_I32, // Uses DEFAULT_INT_TYPE (i32)
		},
		{
			name:     "i32 max fits in i32",
			value:    "2147483647",
			wantType: types.TYPE_I32, // Fits in default i32
		},
		{
			name:     "i32 min fits in i32",
			value:    "-2147483648",
			wantType: types.TYPE_I32, // Fits in default i32
		},
		{
			name:     "exceeds i32 max - promotes to i64",
			value:    "2147483648",
			wantType: types.TYPE_I64, // Doesn't fit in i32, promote to i64
		},
		{
			name:     "large value promotes to i64",
			value:    "3000000000",
			wantType: types.TYPE_I64, // Exceeds i32, uses i64
		},
		{
			name:     "i64 max fits in i64",
			value:    "9223372036854775807",
			wantType: types.TYPE_I64, // Fits in i64
		},
		{
			name:     "exceeds i64 - promotes to i128",
			value:    "9223372036854775808",
			wantType: types.TYPE_I128, // Doesn't fit in i64, promote to i128
		},
		{
			name:     "very large value promotes to i128",
			value:    "20000000000000000000",
			wantType: types.TYPE_I128, // Exceeds i64, uses i128
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lit := &ast.BasicLit{
				Kind:  ast.INT,
				Value: tt.value,
			}
			result := resolveLiteral(lit, types.TypeUnknown)

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

	// Small values should use DEFAULT_INT_TYPE (i32)
	lit := &ast.BasicLit{
		Kind:  ast.INT,
		Value: "42",
	}
	result := resolveLiteral(lit, types.TypeUnknown)
	name, ok := types.GetPrimitiveName(result)
	if !ok {
		t.Fatal("resolveUntyped did not return a primitive type")
	}

	if name != types.TYPE_I32 {
		t.Errorf("Small value should use default type i32, got %s", name)
	}
}
