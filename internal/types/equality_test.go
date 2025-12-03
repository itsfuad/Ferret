package types

import "testing"

func TestLargeIntegerTypeEquality(t *testing.T) {
	// Test that types created from TYPE_NAME equal the global types
	i128FromName := FromTypeName(TYPE_I128)
	if !i128FromName.Equals(TypeI128) {
		t.Error("i128 from FromTypeName should equal TypeI128")
	}

	u128FromName := FromTypeName(TYPE_U128)
	if !u128FromName.Equals(TypeU128) {
		t.Error("u128 from FromTypeName should equal TypeU128")
	}

	i256FromName := FromTypeName(TYPE_I256)
	if !i256FromName.Equals(TypeI256) {
		t.Error("i256 from FromTypeName should equal TypeI256")
	}

	u256FromName := FromTypeName(TYPE_U256)
	if !u256FromName.Equals(TypeU256) {
		t.Error("u256 from FromTypeName should equal TypeU256")
	}
}
