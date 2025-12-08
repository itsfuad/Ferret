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

	// Automatic dereferencing: &T is compatible with T
	// This allows reference types to be used transparently
	source = dereferenceType(source)
	target = dereferenceType(target)

	// Special handling for none
	// none can be assigned to any optional type (T?)
	if source.Equals(types.TypeNone) {
		if _, ok := target.(*types.OptionalType); ok {
			return Assignable
		}
		return Incompatible
	}

	// Identical types
	if source.Equals(target) {
		return Identical
	}

	// UNTYPED literals can be assigned to compatible concrete types with restrictions
	if types.IsUntyped(source) {
		// Unwrap target if it's a NamedType to check the underlying type
		targetUnwrapped := types.UnwrapType(target)

		if types.IsUntypedInt(source) && types.IsInteger(targetUnwrapped) {
			return Assignable
		}
		if types.IsUntypedFloat(source) && types.IsFloat(targetUnwrapped) {
			return Assignable
		}
		return Incompatible
	}

	// Check numeric conversions
	if types.IsNumeric(source) && types.IsNumeric(target) {
		// Special case: byte and u8 are internally the same size but require explicit cast
		srcName, srcOk := types.GetPrimitiveName(source)
		tgtName, tgtOk := types.GetPrimitiveName(target)
		if srcOk && tgtOk {
			// byte -> u8 or u8 -> byte requires explicit cast
			if (srcName == types.TYPE_BYTE && tgtName == types.TYPE_U8) || (srcName == types.TYPE_U8 && tgtName == types.TYPE_BYTE) {
				return LossyConvertible
			}
			if types.IsIntegerTypeName(srcName) && types.IsFloatTypeName(tgtName) {

				if isLosslessNumericConversion(source, target) {
					return Assignable
				}

				return LossyConvertible
			}

			if types.IsFloatTypeName(srcName) && types.IsIntegerTypeName(tgtName) {
				return LossyConvertible
			}
		}

		if isLosslessNumericConversion(source, target) {
			return Assignable
		}
		return LossyConvertible
	}

	// Check struct compatibility (unwrap NamedType to get underlying structure)
	srcUnwrapped := types.UnwrapType(source)
	tgtUnwrapped := types.UnwrapType(target)

	// If target is a NamedType and source matches the underlying structure,
	// allow assignment (e.g., { .x = 1, .y = 2 } can be assigned to Point if Point wraps struct { .x: i32, .y: i32 })
	if srcStruct, srcOk := srcUnwrapped.(*types.StructType); srcOk {
		if tgtStruct, tgtOk := tgtUnwrapped.(*types.StructType); tgtOk {
			// Check if structs are structurally compatible
			if areStructsCompatible(srcStruct, tgtStruct) {
				// If target is a NamedType, this is a structural -> nominal conversion (assignable)
				// If both are anonymous or both are named and identical, it's identical
				if source.Equals(target) {
					return Identical
				}
				return Assignable
			}
		}
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
		types.TYPE_I8:   {types.TYPE_I16, types.TYPE_I32, types.TYPE_I64, types.TYPE_I128, types.TYPE_I256, types.TYPE_F32, types.TYPE_F64, types.TYPE_F128, types.TYPE_F256},
		types.TYPE_I16:  {types.TYPE_I32, types.TYPE_I64, types.TYPE_I128, types.TYPE_I256, types.TYPE_F32, types.TYPE_F64, types.TYPE_F128, types.TYPE_F256},
		types.TYPE_I32:  {types.TYPE_I64, types.TYPE_I128, types.TYPE_I256, types.TYPE_F64, types.TYPE_F128, types.TYPE_F256},
		types.TYPE_I64:  {types.TYPE_I128, types.TYPE_I256, types.TYPE_F64, types.TYPE_F128, types.TYPE_F256},
		types.TYPE_I128: {types.TYPE_I256, types.TYPE_F128, types.TYPE_F256},
		types.TYPE_I256: {types.TYPE_F256},
		types.TYPE_U8:   {types.TYPE_U16, types.TYPE_U32, types.TYPE_U64, types.TYPE_I16, types.TYPE_I32, types.TYPE_I64, types.TYPE_F32, types.TYPE_F64, types.TYPE_F128},
		types.TYPE_U16:  {types.TYPE_U32, types.TYPE_U64, types.TYPE_I32, types.TYPE_I64, types.TYPE_F32, types.TYPE_F64, types.TYPE_F128},
		types.TYPE_U32:  {types.TYPE_U64, types.TYPE_I64, types.TYPE_F64, types.TYPE_F128},
		types.TYPE_U64:  {types.TYPE_F64, types.TYPE_F128},
		types.TYPE_U128: {types.TYPE_F128, types.TYPE_F256},
		types.TYPE_U256: {types.TYPE_F256},
		types.TYPE_F32:  {types.TYPE_F64, types.TYPE_F128, types.TYPE_F256},
		types.TYPE_F64:  {types.TYPE_F128, types.TYPE_F256},
		types.TYPE_F128: {types.TYPE_F256},
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
		// Check if target has smaller bit size than source
		srcName, srcOk := types.GetPrimitiveName(source)
		tgtName, tgtOk := types.GetPrimitiveName(target)

		msg := fmt.Sprintf("cannot implicitly convert '%s' to '%s'", source, target)

		if srcOk && tgtOk {
			srcBits := types.GetNumberBitSize(srcName)
			tgtBits := types.GetNumberBitSize(tgtName)
			if tgtBits < srcBits {
				return fmt.Sprintf("%s (possible data loss)", msg)
			}
		}
		// Same size but different types (e.g., byte vs u8)
		return msg

	case Identical, Assignable:
		// These are not errors
		return ""

	default:
		return fmt.Sprintf("type mismatch: '%s' and '%s'", source, target)
	}
}

// countSignificantDigits counts the number of significant digits in a float literal string
// Ignores leading zeros before first non-zero digit and trailing zeros after decimal point
func countSignificantDigits(floatStr string) int {
	// Remove sign
	if len(floatStr) > 0 && (floatStr[0] == '+' || floatStr[0] == '-') {
		floatStr = floatStr[1:]
	}

	// Find the exponent part if exists
	expIndex := -1
	for i, ch := range floatStr {
		if ch == 'e' || ch == 'E' {
			expIndex = i
			break
		}
	}

	// Work with the mantissa only (before exponent)
	mantissa := floatStr
	if expIndex != -1 {
		mantissa = floatStr[:expIndex]
	}

	// Remove trailing zeros after decimal point
	if decimalIndex := -1; true {
		for i, ch := range mantissa {
			if ch == '.' {
				decimalIndex = i
				break
			}
		}
		if decimalIndex != -1 {
			// Has decimal point - trim trailing zeros
			mantissa = trimTrailingZeros(mantissa)
		}
	}

	// Count significant digits (ignoring leading zeros and decimal point)
	count := 0
	leadingZeros := true

	for _, ch := range mantissa {
		if ch == '.' {
			continue
		}
		if ch >= '0' && ch <= '9' {
			if ch != '0' {
				leadingZeros = false
			}
			if !leadingZeros {
				count++
			}
		}
	}

	return count
}

// trimTrailingZeros removes trailing zeros after the decimal point
func trimTrailingZeros(s string) string {
	// Find decimal point
	decimalIndex := -1
	for i, ch := range s {
		if ch == '.' {
			decimalIndex = i
			break
		}
	}
	if decimalIndex == -1 {
		return s // No decimal point
	}

	// Trim from the end
	end := len(s)
	for end > decimalIndex+1 && s[end-1] == '0' {
		end--
	}

	// If we trimmed everything after decimal, keep the decimal point
	return s[:end]
}

// getFloatPrecision returns the approximate decimal precision for float types
func getFloatPrecision(name types.TYPE_NAME) int {
	switch name {
	case types.TYPE_F32:
		return 7 // ~7 decimal digits
	case types.TYPE_F64:
		return 16 // ~15-16 decimal digits
	case types.TYPE_F128:
		return 34 // ~34 decimal digits
	case types.TYPE_F256:
		return 71 // ~71 decimal digits
	default:
		return 0
	}
}

// fitsInType checks if a string literal value fits in a given type.
// Uses NumericValue interface which automatically chooses int64 (fast) or big.Int (precise) representation.
func fitsInType(valueStr string, t types.SemType) bool {
	// Extract primitive type name
	name, ok := types.GetPrimitiveName(t)
	if !ok {
		return false
	}

	// Handle float types - check precision
	if types.IsFloatTypeName(name) {
		digits := countSignificantDigits(valueStr)
		precision := getFloatPrecision(name)
		return digits <= precision
	}

	// Handle integer types - check range
	if types.IsIntegerTypeName(name) {
		numValue, err := numeric.NewNumericValue(valueStr)
		if err != nil {
			return false
		}
		bitSize := types.GetNumberBitSize(name)
		if bitSize == 0 {
			return false
		}
		signed := types.IsSigned(name)
		return numValue.FitsInBitSize(int(bitSize), signed)
	}

	return false
}

// getMinimumTypeForValue returns the smallest type that can hold the given value.
// For positive values, prefers signed types (more natural for integers).
// Uses NumericValue interface which automatically chooses optimal representation.
func getMinimumTypeForValue(valueStr string) types.TYPE_NAME {
	// Use NumericValue interface (automatically chooses int64 fast path or big.Int)
	numValue, err := numeric.NewNumericValue(valueStr)
	if err != nil {
		return types.TYPE_UNKNOWN
	}

	// Check signed types first if negative
	if numValue.IsNegative() {
		signedTypes := []struct {
			name    types.TYPE_NAME
			bitSize int
		}{
			{types.TYPE_I8, 8},
			{types.TYPE_I16, 16},
			{types.TYPE_I32, 32},
			{types.TYPE_I64, 64},
			{types.TYPE_I128, 128},
			{types.TYPE_I256, 256},
		}

		for _, t := range signedTypes {
			if numValue.FitsInBitSize(t.bitSize, true) {
				return t.name
			}
		}
	} else {
		// For positive values, prefer signed types (more natural)
		// Use the next larger signed type to accommodate the value
		types := []struct {
			unsignedBit int
			preferred   types.TYPE_NAME // Prefer signed type
		}{
			{8, types.TYPE_I16},    // 0-255 fits in i16
			{16, types.TYPE_I32},   // 0-65535 fits in i32
			{32, types.TYPE_I64},   // 0-4B fits in i64
			{64, types.TYPE_I128},  // 0-18Q fits in i128
			{128, types.TYPE_I256}, // 0-340U fits in i256
			{256, types.TYPE_U256}, // No larger signed type exists
		}

		for _, t := range types {
			if numValue.FitsInBitSize(t.unsignedBit, false) {
				return t.preferred
			}
		}
	}

	return types.TYPE_UNKNOWN
}

// getMinimumFloatTypeForDigits returns the smallest float type that can hold the given precision
func getMinimumFloatTypeForDigits(digits int) types.TYPE_NAME {
	if digits <= 7 {
		return types.TYPE_F32
	}
	if digits <= 16 {
		return types.TYPE_F64
	}
	if digits <= 34 {
		return types.TYPE_F128
	}
	if digits <= 71 {
		return types.TYPE_F256
	}
	return types.TYPE_UNKNOWN
}