package numeric

import (
	"testing"
)

func TestNewNumericValue(t *testing.T) {
	tests := []struct {
		input     string
		wantInt64 bool // true if should use Int64Value, false if BigIntValue
	}{
		{"42", true},
		{"127", true},
		{"-128", true},
		{"9223372036854775807", true},  // max int64
		{"-9223372036854775808", true}, // min int64
		{"9223372036854775808", false}, // overflow int64, needs BigInt
		{"170141183460469231731687303715884105727", false}, // i128 max
		{"0xFF", true},
		{"0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", false}, // u128 max in hex
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			val, err := NewNumericValue(tt.input)
			if err != nil {
				t.Fatalf("NewNumericValue(%s) error: %v", tt.input, err)
			}

			switch v := val.(type) {
			case *Int64Value:
				if !tt.wantInt64 {
					t.Errorf("NewNumericValue(%s) = Int64Value, want BigIntValue", tt.input)
				}
			case *BigIntValue:
				if tt.wantInt64 {
					t.Errorf("NewNumericValue(%s) = BigIntValue, want Int64Value", tt.input)
				}
			default:
				t.Errorf("NewNumericValue(%s) = unexpected type %T", tt.input, v)
			}
		})
	}
}

func TestInt64ValueFitsInBitSize(t *testing.T) {
	tests := []struct {
		value   int64
		bitSize int
		signed  bool
		want    bool
	}{
		{100, 8, true, true},
		{127, 8, true, true},
		{128, 8, true, false},
		{-128, 8, true, true},
		{-129, 8, true, false},
		{255, 8, false, true},
		{256, 8, false, false},
		{-1, 8, false, false},
		{100, 128, true, true},
		{100, 256, true, true},
		{-100, 128, false, false},
	}

	for _, tt := range tests {
		val := &Int64Value{value: tt.value}
		got := val.FitsInBitSize(tt.bitSize, tt.signed)
		if got != tt.want {
			signStr := "unsigned"
			if tt.signed {
				signStr = "signed"
			}
			t.Errorf("Int64Value{%d}.FitsInBitSize(%d, %s) = %v, want %v",
				tt.value, tt.bitSize, signStr, got, tt.want)
		}
	}
}

func TestBigIntValueFitsInBitSize(t *testing.T) {
	tests := []struct {
		value   string
		bitSize int
		signed  bool
		want    bool
	}{
		{"170141183460469231731687303715884105727", 128, true, true},   // max i128
		{"170141183460469231731687303715884105728", 128, true, false},  // overflow i128
		{"340282366920938463463374607431768211455", 128, false, true},  // max u128
		{"340282366920938463463374607431768211456", 128, false, false}, // overflow u128
		{"-170141183460469231731687303715884105728", 128, true, true},  // min i128
		{"-170141183460469231731687303715884105729", 128, true, false}, // underflow i128
		{"1000", 256, true, true},
		{"-1", 128, false, false}, // negative doesn't fit unsigned
	}

	for _, tt := range tests {
		bigInt, err := StringToBigInt(tt.value)
		if err != nil {
			t.Fatalf("StringToBigInt(%s) error: %v", tt.value, err)
		}
		val := &BigIntValue{value: bigInt}
		got := val.FitsInBitSize(tt.bitSize, tt.signed)
		if got != tt.want {
			signStr := "unsigned"
			if tt.signed {
				signStr = "signed"
			}
			t.Errorf("BigIntValue{%s}.FitsInBitSize(%d, %s) = %v, want %v",
				tt.value, tt.bitSize, signStr, got, tt.want)
		}
	}
}

func TestNumericValueInterface(t *testing.T) {
	// Test that both Int64Value and BigIntValue implement NumericValue
	var _ NumericValue = &Int64Value{}
	var _ NumericValue = &BigIntValue{}
	var _ NumericValue = &Float64Value{}
}

func TestFloat64Value(t *testing.T) {
	val, err := NewFloatValue("3.14159")
	if err != nil {
		t.Fatalf("NewFloatValue error: %v", err)
	}

	if !val.FitsInBitSize(32, false) {
		t.Error("Float should fit in 32-bit")
	}

	if !val.FitsInBitSize(64, false) {
		t.Error("Float should fit in 64-bit")
	}

	if val.FitsInBitSize(8, true) {
		t.Error("Float should not fit in integer type")
	}
}
