package consteval

import (
	"compiler/internal/types"
	"fmt"
	"math/big"
)

// ConstValue represents a compile-time known constant value
// Used for constant propagation and dead code detection
type ConstValue struct {
	Kind  ConstKind
	Type  types.SemType
	Value interface{} // Actual value: *big.Int, *big.Float, bool, string
}

// ConstKind categorizes the type of constant value
type ConstKind int

const (
	ConstUnknown ConstKind = iota // Not a constant or cannot be evaluated
	ConstInt                      // Integer constant (*big.Int)
	ConstFloat                    // Float constant (*big.Float)
	ConstBool                     // Boolean constant (bool)
	ConstString                   // String constant (string)
)

// NewIntValue creates a constant integer value
func NewIntValue(val int64, typ types.SemType) *ConstValue {
	return &ConstValue{
		Kind:  ConstInt,
		Type:  typ,
		Value: big.NewInt(val),
	}
}

// NewBigIntValue creates a constant integer value from big.Int
func NewBigIntValue(val *big.Int, typ types.SemType) *ConstValue {
	return &ConstValue{
		Kind:  ConstInt,
		Type:  typ,
		Value: val,
	}
}

// NewFloatValue creates a constant float value
func NewFloatValue(val float64, typ types.SemType) *ConstValue {
	f := big.NewFloat(val)
	return &ConstValue{
		Kind:  ConstFloat,
		Type:  typ,
		Value: f,
	}
}

// NewBigFloatValue creates a constant float value from big.Float
func NewBigFloatValue(val *big.Float, typ types.SemType) *ConstValue {
	return &ConstValue{
		Kind:  ConstFloat,
		Type:  typ,
		Value: val,
	}
}

// NewBoolValue creates a constant boolean value
func NewBoolValue(val bool) *ConstValue {
	return &ConstValue{
		Kind:  ConstBool,
		Type:  types.TypeBool,
		Value: val,
	}
}

// NewStringValue creates a constant string value
func NewStringValue(val string) *ConstValue {
	return &ConstValue{
		Kind:  ConstString,
		Type:  types.TypeString,
		Value: val,
	}
}

// IsConstant returns true if the value represents a compile-time constant
func (cv *ConstValue) IsConstant() bool {
	return cv != nil && cv.Kind != ConstUnknown
}

// AsInt returns the integer value if this is a ConstInt
// Second return value indicates success
func (cv *ConstValue) AsInt() (*big.Int, bool) {
	if cv == nil || cv.Kind != ConstInt {
		return nil, false
	}
	if val, ok := cv.Value.(*big.Int); ok {
		return val, true
	}
	return nil, false
}

// AsInt64 returns the integer value as int64 if possible
func (cv *ConstValue) AsInt64() (int64, bool) {
	val, ok := cv.AsInt()
	if !ok {
		return 0, false
	}
	if !val.IsInt64() {
		return 0, false // Value too large for int64
	}
	return val.Int64(), true
}

// AsFloat returns the float value if this is a ConstFloat
func (cv *ConstValue) AsFloat() (*big.Float, bool) {
	if cv == nil || cv.Kind != ConstFloat {
		return nil, false
	}
	if val, ok := cv.Value.(*big.Float); ok {
		return val, true
	}
	return nil, false
}

// AsFloat64 returns the float value as float64 if possible
func (cv *ConstValue) AsFloat64() (float64, bool) {
	val, ok := cv.AsFloat()
	if !ok {
		return 0, false
	}
	f64, _ := val.Float64()
	return f64, true
}

// AsBool returns the boolean value if this is a ConstBool
func (cv *ConstValue) AsBool() (bool, bool) {
	if cv == nil || cv.Kind != ConstBool {
		return false, false
	}
	if val, ok := cv.Value.(bool); ok {
		return val, true
	}
	return false, false
}

// AsString returns the string value if this is a ConstString
func (cv *ConstValue) AsString() (string, bool) {
	if cv == nil || cv.Kind != ConstString {
		return "", false
	}
	if val, ok := cv.Value.(string); ok {
		return val, true
	}
	return "", false
}

// String returns a string representation of the constant value
func (cv *ConstValue) String() string {
	if cv == nil || cv.Kind == ConstUnknown {
		return "<non-constant>"
	}

	switch cv.Kind {
	case ConstInt:
		if val, ok := cv.AsInt(); ok {
			return val.String()
		}
	case ConstFloat:
		if val, ok := cv.AsFloat(); ok {
			return val.String()
		}
	case ConstBool:
		if val, ok := cv.AsBool(); ok {
			return fmt.Sprintf("%t", val)
		}
	case ConstString:
		if val, ok := cv.AsString(); ok {
			return fmt.Sprintf("%q", val)
		}
	}
	return "<invalid>"
}

// IsTruthy returns true if the value is considered "truthy" in boolean context
// For booleans: the actual value
// For numbers: non-zero
// For strings: non-empty
func (cv *ConstValue) IsTruthy() bool {
	if cv == nil || cv.Kind == ConstUnknown {
		return false
	}

	switch cv.Kind {
	case ConstBool:
		val, _ := cv.AsBool()
		return val
	case ConstInt:
		if val, ok := cv.AsInt(); ok {
			return val.Sign() != 0
		}
	case ConstFloat:
		if val, ok := cv.AsFloat(); ok {
			return val.Sign() != 0
		}
	case ConstString:
		if val, ok := cv.AsString(); ok {
			return len(val) > 0
		}
	}
	return false
}

// Equals compares two constant values for equality
func (cv *ConstValue) Equals(other *ConstValue) bool {
	if cv == nil || other == nil {
		return cv == other
	}
	if cv.Kind != other.Kind {
		return false
	}

	switch cv.Kind {
	case ConstInt:
		v1, ok1 := cv.AsInt()
		v2, ok2 := other.AsInt()
		return ok1 && ok2 && v1.Cmp(v2) == 0
	case ConstFloat:
		v1, ok1 := cv.AsFloat()
		v2, ok2 := other.AsFloat()
		return ok1 && ok2 && v1.Cmp(v2) == 0
	case ConstBool:
		v1, ok1 := cv.AsBool()
		v2, ok2 := other.AsBool()
		return ok1 && ok2 && v1 == v2
	case ConstString:
		v1, ok1 := cv.AsString()
		v2, ok2 := other.AsString()
		return ok1 && ok2 && v1 == v2
	}
	return false
}
