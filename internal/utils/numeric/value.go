package numeric

import (
	"math/big"
	"strconv"
)

// NumericValue represents any numeric literal value that can be validated against type constraints.
// This abstraction allows us to optimize for common cases (int64/float64) while supporting
// arbitrary precision for large integers (128-bit, 256-bit, etc.).
type NumericValue interface {
	// FitsInBitSize checks if the value fits in the given bit size (signed or unsigned)
	FitsInBitSize(bitSize int, signed bool) bool

	// String returns the string representation of the value
	String() string

	// IsNegative returns true if the value is negative
	IsNegative() bool
}

// Int64Value represents a value that fits in int64 (fast path)
type Int64Value struct {
	value int64
}

// BigIntValue represents a value that requires arbitrary precision
type BigIntValue struct {
	value *big.Int
}

// Float64Value represents a floating-point value
type Float64Value struct {
	value float64
}

// NewNumericValue creates the appropriate NumericValue from a string.
// It automatically chooses the most efficient representation.
func NewNumericValue(s string) (NumericValue, error) {
	// Remove underscores
	s = cleanNumericString(s)

	// Try int64 first (fast path for most common case)
	if value, err := strconv.ParseInt(s, 0, 64); err == nil {
		return &Int64Value{value: value}, nil
	}

	// Fall back to big.Int for large integers
	bigValue, err := StringToBigInt(s)
	if err != nil {
		return nil, err
	}

	return &BigIntValue{value: bigValue}, nil
}

// NewFloatValue creates a NumericValue for floating-point literals
func NewFloatValue(s string) (NumericValue, error) {
	value, err := StringToFloat(s)
	if err != nil {
		return nil, err
	}
	return &Float64Value{value: value}, nil
}

// Int64Value implementation

func (v *Int64Value) FitsInBitSize(bitSize int, signed bool) bool {
	if signed {
		// Signed range: -2^(bitSize-1) to 2^(bitSize-1) - 1
		switch bitSize {
		case 8:
			return v.value >= -128 && v.value <= 127
		case 16:
			return v.value >= -32768 && v.value <= 32767
		case 32:
			return v.value >= -2147483648 && v.value <= 2147483647
		case 64:
			return true // int64 always fits
		case 128, 256:
			return true // int64 always fits in larger types
		default:
			// For arbitrary bit sizes, use big.Int
			return v.toBigInt().FitsInBitSize(bitSize, signed)
		}
	} else {
		// Unsigned range: 0 to 2^bitSize - 1
		if v.value < 0 {
			return false
		}
		switch bitSize {
		case 8:
			return v.value <= 255
		case 16:
			return v.value <= 65535
		case 32:
			return v.value <= 4294967295
		case 64:
			return true // positive int64 always fits in u64
		case 128, 256:
			return true // positive int64 always fits in larger unsigned types
		default:
			// For arbitrary bit sizes, use big.Int
			return v.toBigInt().FitsInBitSize(bitSize, signed)
		}
	}
}

func (v *Int64Value) String() string {
	return strconv.FormatInt(v.value, 10)
}

func (v *Int64Value) IsNegative() bool {
	return v.value < 0
}

func (v *Int64Value) toBigInt() *BigIntValue {
	return &BigIntValue{value: big.NewInt(v.value)}
}

// BigIntValue implementation

func (v *BigIntValue) FitsInBitSize(bitSize int, signed bool) bool {
	return FitsInBitSize(v.value, bitSize, signed)
}

func (v *BigIntValue) String() string {
	return v.value.String()
}

func (v *BigIntValue) IsNegative() bool {
	return v.value.Sign() < 0
}

// Float64Value implementation

func (v *Float64Value) FitsInBitSize(bitSize int, signed bool) bool {
	// For floats, we check if they fit in f32 or f64
	switch bitSize {
	case 32:
		// Check if the float can be represented in float32 without overflow
		// This is a simplified check; precise validation would need more careful handling
		return true // For now, allow all floats in f32 context
	case 64:
		return true // All float64 values fit in f64
	default:
		return false // Floats don't fit in integer types
	}
}

func (v *Float64Value) String() string {
	return strconv.FormatFloat(v.value, 'g', -1, 64)
}

func (v *Float64Value) IsNegative() bool {
	return v.value < 0
}

// Helper function to clean numeric strings (remove underscores)
func cleanNumericString(s string) string {
	// Already implemented in StringToBigInt, reuse that logic
	result := make([]byte, 0, len(s))
	for i := 0; i < len(s); i++ {
		if s[i] != '_' {
			result = append(result, s[i])
		}
	}
	return string(result)
}
