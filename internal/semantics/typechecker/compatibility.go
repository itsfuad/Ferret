package typechecker

import (
	"compiler/internal/types"
	"fmt"
)

// TypeCompatibility represents the relationship between two types
type TypeCompatibility int

const (
	// Incompatible types cannot be used together
	Incompatible TypeCompatibility = iota

	// Identical types are exactly the same
	Identical

	// Assignable means source can be assigned to target (may involve implicit conversion)
	Assignable

	// LosslessConvertible means conversion is safe and doesn't lose information
	LosslessConvertible

	// LossyConvertible means conversion may lose information (requires explicit cast)
	LossyConvertible
)

func (tc TypeCompatibility) String() string {
	switch tc {
	case Incompatible:
		return "incompatible"
	case Identical:
		return "identical"
	case Assignable:
		return "assignable"
	case LosslessConvertible:
		return "lossless convertible"
	case LossyConvertible:
		return "lossy convertible"
	default:
		return "unknown"
	}
}

// checkTypeCompatibility determines if source type can be used where target type is expected
func checkTypeCompatibility(source, target types.SemType) TypeCompatibility {
	// Handle unknown types
	if source.Equals(types.TypeUnknown) || target.Equals(types.TypeUnknown) {
		return Incompatible
	}

	// Identical types
	if source.Equals(target) {
		return Identical
	}

	// UNTYPED literals can be assigned to any compatible concrete type
	if types.IsUntyped(source) {
		if types.IsUntypedInt(source) && types.IsNumeric(target) {
			return Assignable
		}
		if types.IsUntypedFloat(source) && types.IsFloat(target) {
			return Assignable
		}
		return Incompatible
	}

	// Check numeric conversions
	if types.IsNumeric(source) && types.IsNumeric(target) {
		if isLosslessNumericConversion(source, target) {
			return LosslessConvertible
		}
		return LossyConvertible
	}

	// No implicit conversion available
	return Incompatible
}

// isLosslessNumericConversion checks if converting from source to target is lossless
func isLosslessNumericConversion(source, target types.SemType) bool {
	// Extract primitive names
	srcName, srcOk := types.GetPrimitiveName(source)
	tgtName, tgtOk := types.GetPrimitiveName(target)
	if !srcOk || !tgtOk {
		return false
	}

	// Widening conversions that don't lose precision
	losslessConversions := map[types.TYPE_NAME][]types.TYPE_NAME{
		types.TYPE_I8:  {types.TYPE_I16, types.TYPE_I32, types.TYPE_I64, types.TYPE_F32, types.TYPE_F64},
		types.TYPE_I16: {types.TYPE_I32, types.TYPE_I64, types.TYPE_F32, types.TYPE_F64},
		types.TYPE_I32: {types.TYPE_I64, types.TYPE_F64},
		types.TYPE_I64: {types.TYPE_F64},
		types.TYPE_U8:  {types.TYPE_U16, types.TYPE_U32, types.TYPE_U64, types.TYPE_I16, types.TYPE_I32, types.TYPE_I64, types.TYPE_F32, types.TYPE_F64},
		types.TYPE_U16: {types.TYPE_U32, types.TYPE_U64, types.TYPE_I32, types.TYPE_I64, types.TYPE_F32, types.TYPE_F64},
		types.TYPE_U32: {types.TYPE_U64, types.TYPE_I64, types.TYPE_F64},
		types.TYPE_U64: {types.TYPE_F64},
		types.TYPE_F32: {types.TYPE_F64},
	}

	if targets, ok := losslessConversions[srcName]; ok {
		for _, t := range targets {
			if t == tgtName {
				return true
			}
		}
	}

	return false
}

// getConversionError returns a human-readable error message for type incompatibility
func getConversionError(source, target types.SemType, compatibility TypeCompatibility) string {
	switch compatibility {
	case Incompatible:
		return fmt.Sprintf("cannot use type '%s' as type '%s'", source, target)

	case LossyConvertible:
		return fmt.Sprintf("cannot implicitly convert '%s' to '%s' (may lose precision)", source, target)

	case Identical, Assignable, LosslessConvertible:
		// These are not errors
		return ""

	default:
		return fmt.Sprintf("type mismatch: '%s' and '%s'", source, target)
	}
}

// fitsInType checks if a value fits in a given type
func fitsInType(value int64, t types.SemType) bool {
	// Extract primitive type name
	name, ok := types.GetPrimitiveName(t)
	if !ok {
		return false // Non-primitive types don't have numeric ranges
	}

	switch name {
	case types.TYPE_I8:
		return value >= -128 && value <= 127
	case types.TYPE_I16:
		return value >= -32768 && value <= 32767
	case types.TYPE_I32:
		return value >= -2147483648 && value <= 2147483647
	case types.TYPE_I64:
		return true // int64 always fits
	case types.TYPE_U8:
		return value >= 0 && value <= 255
	case types.TYPE_U16:
		return value >= 0 && value <= 65535
	case types.TYPE_U32:
		return value >= 0 && value <= 4294967295
	case types.TYPE_U64:
		return value >= 0
	default:
		return false
	}
}

// getMinimumTypeForValue returns the smallest type that can hold the given value
func getMinimumTypeForValue(value int64) string {
	// For negative values, check signed types
	if value < 0 {
		if value >= -128 {
			return "i8"
		}
		if value >= -32768 {
			return "i16"
		}
		if value >= -2147483648 {
			return "i32"
		}
		return "i64"
	}

	// For positive values, prefer unsigned types but also show signed alternative
	if value <= 127 {
		return "i8 or u8"
	}
	if value <= 255 {
		return "u8 or i16"
	}
	if value <= 32767 {
		return "i16 or u16"
	}
	if value <= 65535 {
		return "u16 or i32"
	}
	if value <= 2147483647 {
		return "i32 or u32"
	}
	if value <= 4294967295 {
		return "u32 or i64"
	}
	return "i64 or u64"
}

// getTypeRange returns a human-readable range for integer types
func getTypeRange(t types.SemType) string {
	// Extract primitive type name
	name, ok := types.GetPrimitiveName(t)
	if !ok {
		return "unknown range"
	}

	switch name {
	case types.TYPE_I8:
		return "-128 to 127"
	case types.TYPE_I16:
		return "-32,768 to 32,767"
	case types.TYPE_I32:
		return "-2,147,483,648 to 2,147,483,647"
	case types.TYPE_I64:
		return "-9,223,372,036,854,775,808 to 9,223,372,036,854,775,807"
	case types.TYPE_U8:
		return "0 to 255"
	case types.TYPE_U16:
		return "0 to 65,535"
	case types.TYPE_U32:
		return "0 to 4,294,967,295"
	case types.TYPE_U64:
		return "0 to 18,446,744,073,709,551,615"
	default:
		return "unknown range"
	}
}
