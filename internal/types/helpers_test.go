package types

import (
	"testing"

	"compiler/internal/utils/numeric"
)

func TestIsInteger(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"123", true},
		{"-123", true},
		{"0", true},
		{"123.45", false},
		{"abc", false},
		{"", false},
	}

	for _, test := range tests {
		result := numeric.IsDecimal(test.input)
		if result != test.expected {
			t.Errorf("IsInteger(%q) = %v, expected %v", test.input, result, test.expected)
		}
	}
}

func TestIsFloat(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"123", false},
		{"123.0", true},
		{"-123", false},
		{"0", false},
		{"123.45", true},
		{"-123.45", true},
		{"0.0", true},
		{"abc", false},
		{"", false},
	}

	for _, test := range tests {
		result := numeric.IsFloat(test.input)
		if result != test.expected {
			t.Errorf("IsFloat(%q) = %v, expected %v", test.input, result, test.expected)
		}
	}
}

func TestGetNumberBitSize(t *testing.T) {
	tests := []struct {
		kind     TYPE_NAME
		expected uint8
	}{
		{TYPE_I8, 8},
		{TYPE_U8, 8},
		{TYPE_BYTE, 8},
		{TYPE_I16, 16},
		{TYPE_U16, 16},
		{TYPE_I32, 32},
		{TYPE_U32, 32},
		{TYPE_F32, 32},
		{TYPE_I64, 64},
		{TYPE_U64, 64},
		{TYPE_F64, 64},
		{TYPE_STRING, 0}, // assuming STRING is a TYPE_NAME that doesn't match any case
	}

	for _, test := range tests {
		result := GetNumberBitSize(test.kind)
		if result != test.expected {
			t.Errorf("GetNumberBitSize(%v) = %v, expected %v", test.kind, result, test.expected)
		}
	}
}

func TestIsSigned(t *testing.T) {
	tests := []struct {
		kind     TYPE_NAME
		expected bool
	}{
		{TYPE_I8, true},
		{TYPE_I16, true},
		{TYPE_I32, true},
		{TYPE_I64, true},
		{TYPE_U8, false},
		{TYPE_U16, false},
		{TYPE_U32, false},
		{TYPE_U64, false},
		{TYPE_BYTE, false},
		{TYPE_F32, false},
		{TYPE_F64, false},
		{TYPE_STRING, false}, // assuming STRING is a TYPE_NAME that doesn't match any case
	}

	for _, test := range tests {
		result := IsSigned(test.kind)
		if result != test.expected {
			t.Errorf("IsSigned(%v) = %v, expected %v", test.kind, result, test.expected)
		}
	}
}

func TestIsUnsigned(t *testing.T) {
	tests := []struct {
		kind     TYPE_NAME
		expected bool
	}{
		{TYPE_U8, true},
		{TYPE_U16, true},
		{TYPE_U32, true},
		{TYPE_U64, true},
		{TYPE_BYTE, true},
		{TYPE_I8, false},
		{TYPE_I16, false},
		{TYPE_I32, false},
		{TYPE_I64, false},
		{TYPE_F32, false},
		{TYPE_F64, false},
		{TYPE_STRING, false}, // assuming STRING is a TYPE_NAME that doesn't match any case
	}

	for _, test := range tests {
		result := IsUnsigned(test.kind)
		if result != test.expected {
			t.Errorf("IsUnsigned(%v) = %v, expected %v", test.kind, result, test.expected)
		}
	}
}
