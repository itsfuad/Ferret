package types

import "testing"

func TestLargeIntegerTypes(t *testing.T) {
	// Test that large integer types are properly initialized
	if TypeI128 == nil {
		t.Error("TypeI128 is nil")
	}
	if TypeU128 == nil {
		t.Error("TypeU128 is nil")
	}
	if TypeI256 == nil {
		t.Error("TypeI256 is nil")
	}
	if TypeU256 == nil {
		t.Error("TypeU256 is nil")
	}

	// Test string representation
	if TypeI128.String() != "i128" {
		t.Errorf("TypeI128.String() = %s, want i128", TypeI128.String())
	}
	if TypeU128.String() != "u128" {
		t.Errorf("TypeU128.String() = %s, want u128", TypeU128.String())
	}
	if TypeI256.String() != "i256" {
		t.Errorf("TypeI256.String() = %s, want i256", TypeI256.String())
	}
	if TypeU256.String() != "u256" {
		t.Errorf("TypeU256.String() = %s, want u256", TypeU256.String())
	}

	// Test size
	if TypeI128.Size() != 16 {
		t.Errorf("TypeI128.Size() = %d, want 16", TypeI128.Size())
	}
	if TypeU128.Size() != 16 {
		t.Errorf("TypeU128.Size() = %d, want 16", TypeU128.Size())
	}
	if TypeI256.Size() != 32 {
		t.Errorf("TypeI256.Size() = %d, want 32", TypeI256.Size())
	}
	if TypeU256.Size() != 32 {
		t.Errorf("TypeU256.Size() = %d, want 32", TypeU256.Size())
	}

	// Test IsNumeric
	if !IsNumeric(TypeI128) {
		t.Error("TypeI128 should be numeric")
	}
	if !IsNumeric(TypeU128) {
		t.Error("TypeU128 should be numeric")
	}
	if !IsNumeric(TypeI256) {
		t.Error("TypeI256 should be numeric")
	}
	if !IsNumeric(TypeU256) {
		t.Error("TypeU256 should be numeric")
	}

	// Test IsInteger
	if !IsInteger(TypeI128) {
		t.Error("TypeI128 should be integer")
	}
	if !IsInteger(TypeU128) {
		t.Error("TypeU128 should be integer")
	}
	if !IsInteger(TypeI256) {
		t.Error("TypeI256 should be integer")
	}
	if !IsInteger(TypeU256) {
		t.Error("TypeU256 should be integer")
	}
}
