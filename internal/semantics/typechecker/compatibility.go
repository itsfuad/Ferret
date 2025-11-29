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
	if source.Equals(types.TypeUntyped) {
		if types.IsNumeric(target) {
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
