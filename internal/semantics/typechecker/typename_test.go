package typechecker

import (
	"compiler/internal/types"
	"testing"
)

func TestTypeFromTypeNameLargeIntegers(t *testing.T) {
	// Test creating types from TYPE_NAME
	i128 := types.FromTypeName(types.TYPE_I128)
	u128 := types.FromTypeName(types.TYPE_U128)
	i256 := types.FromTypeName(types.TYPE_I256)
	u256 := types.FromTypeName(types.TYPE_U256)

	// Verify they're numeric
	if !types.IsNumeric(i128) {
		t.Error("i128 from FromTypeName should be numeric")
	}
	if !types.IsNumeric(u128) {
		t.Error("u128 from FromTypeName should be numeric")
	}
	if !types.IsNumeric(i256) {
		t.Error("i256 from FromTypeName should be numeric")
	}
	if !types.IsNumeric(u256) {
		t.Error("u256 from FromTypeName should be numeric")
	}

	// Verify GetPrimitiveName works
	if name, ok := types.GetPrimitiveName(i128); !ok || name != types.TYPE_I128 {
		t.Errorf("GetPrimitiveName(i128) = (%v, %v), want (TYPE_I128, true)", name, ok)
	}
}
