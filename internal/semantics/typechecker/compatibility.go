package typechecker

import (
	"compiler/internal/types"
	"compiler/internal/utils/numeric"
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

// fitsInType checks if a string literal value fits in a given type.
// Uses NumericValue interface which automatically chooses int64 (fast) or big.Int (precise) representation.
func fitsInType(valueStr string, t types.SemType) bool {
	// Parse as NumericValue (automatically chooses optimal representation)
	numValue, err := numeric.NewNumericValue(valueStr)
	if err != nil {
		return false
	}

	// Extract primitive type name
	name, ok := types.GetPrimitiveName(t)
	if !ok {
		return false
	}

	// Check based on type
	bitSize := types.GetNumberBitSize(name)
	if bitSize == 0 {
		return false // Not a numeric type
	}

	signed := types.IsSigned(name)
	return numValue.FitsInBitSize(int(bitSize), signed)
}

// getMinimumTypeForValue returns the smallest type that can hold the given value.
// For positive values, returns both unsigned and signed alternatives (e.g., "u8 or i16").
// Uses NumericValue interface which automatically chooses optimal representation.
func getMinimumTypeForValue(valueStr string) string {
	// Use NumericValue interface (automatically chooses int64 fast path or big.Int)
	numValue, err := numeric.NewNumericValue(valueStr)
	if err != nil {
		return "unknown"
	}

	// Check signed types first if negative
	if numValue.IsNegative() {
		signedTypes := []struct {
			name    string
			bitSize int
		}{
			{"i8", 8},
			{"i16", 16},
			{"i32", 32},
			{"i64", 64},
			{"i128", 128},
			{"i256", 256},
		}

		for _, t := range signedTypes {
			if numValue.FitsInBitSize(t.bitSize, true) {
				return t.name
			}
		}
	} else {
		// For positive values, show both unsigned and next signed alternative
		types := []struct {
			unsigned    string
			unsignedBit int
			signed      string
			signedBit   int
		}{
			{"u8", 8, "i16", 16},
			{"u16", 16, "i32", 32},
			{"u32", 32, "i64", 64},
			{"u64", 64, "i128", 128},
			{"u128", 128, "i256", 256},
			{"u256", 256, "", 0},
		}

		for _, t := range types {
			if numValue.FitsInBitSize(t.unsignedBit, false) {
				if t.signed != "" {
					// Show both unsigned and signed alternatives
					return fmt.Sprintf("%s or %s", t.unsigned, t.signed)
				}
				// u256 has no larger signed type
				return t.unsigned
			}
		}
	}

	return "too large for any supported type"
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
	case types.TYPE_I128:
		return "-170,141,183,460,469,231,731,687,303,715,884,105,728 to 170,141,183,460,469,231,731,687,303,715,884,105,727"
	case types.TYPE_I256:
		return "-2^255 to 2^255 - 1 (256-bit signed)"
	case types.TYPE_U8:
		return "0 to 255"
	case types.TYPE_U16:
		return "0 to 65,535"
	case types.TYPE_U32:
		return "0 to 4,294,967,295"
	case types.TYPE_U64:
		return "0 to 18,446,744,073,709,551,615"
	case types.TYPE_U128:
		return "0 to 340,282,366,920,938,463,463,374,607,431,768,211,455"
	case types.TYPE_U256:
		return "0 to 2^256 - 1 (256-bit unsigned)"
	default:
		return "unknown range"
	}
}
