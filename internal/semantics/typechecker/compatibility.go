package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/types"
	"compiler/internal/utils/numeric"

	"fmt"
)

// TypeCompatibility represents the relationship between two types
type TypeCompatibility int

const (
	// Incompatible types cannot be used together (not castable at all)
	Incompatible TypeCompatibility = iota

	// Identical types are exactly the same
	Identical

	// ImplicitCastable means source can be assigned to target (implicit conversion allowed)
	ImplicitCastable

	// ExplicitCastable means conversion requires an explicit cast (but is valid)
	ExplicitCastable
)

func (tc TypeCompatibility) String() string {
	switch tc {
	case Incompatible:
		return "incompatible"
	case Identical:
		return "identical"
	case ImplicitCastable:
		return "implicit castable"
	case ExplicitCastable:
		return "explicit castable"
	default:
		return "unknown"
	}
}

func isImplicitlyCompatible(compatibility TypeCompatibility) bool {
	return compatibility == Identical || compatibility == ImplicitCastable
}

// methodSignaturesMatch checks if two function types match
// This is used to compare method signatures with interface method signatures
// Note: Methods don't include the receiver in their FuncType - the receiver is separate
// So we can directly compare the FuncTypes
func methodSignaturesMatch(methodType, interfaceType *types.FunctionType) bool {
	// Direct comparison - methods don't have receiver in FuncType
	return methodType.Equals(interfaceType)
}

// implementsInterface checks if a type implements an interface
// Returns true if the type has all required methods with matching signatures
func implementsInterface(ctx *context_v2.CompilerContext, mod *context_v2.Module, implType types.SemType, ifaceType *types.InterfaceType) bool {
	// Empty interface: all types implement it
	if len(ifaceType.Methods) == 0 {
		return true
	}

	// Unwrap NamedType to get underlying type, but keep name for method lookup
	var typeName string
	if named, ok := implType.(*types.NamedType); ok {
		typeName = named.Name
		implType = named.Underlying
	}

	// Only named types can implement interfaces (they have methods)
	if typeName == "" {
		return false
	}

	// Look up the type symbol to get its methods
	typeSym, found := lookupTypeSymbol(ctx, mod, typeName)
	if !found || typeSym.Methods == nil {
		return false
	}

	// Check that all interface methods are implemented
	for _, requiredMethod := range ifaceType.Methods {
		methodInfo, hasMethod := typeSym.Methods[requiredMethod.Name]
		if !hasMethod {
			return false
		}

		// Check method signature matches
		// Methods don't include receiver in FuncType, so direct comparison works
		if !methodSignaturesMatch(methodInfo.FuncType, requiredMethod.FuncType) {
			// Debug: check what's different
			if ctx != nil && ctx.Config.Debug {
				fmt.Printf("      [Interface check: method %s signature mismatch]\n", requiredMethod.Name)
				fmt.Printf("        Method: %s\n", methodInfo.FuncType.String())
				fmt.Printf("        Interface: %s\n", requiredMethod.FuncType.String())
				fmt.Printf("        Method return: %s, Interface return: %s\n",
					methodInfo.FuncType.Return.String(), requiredMethod.FuncType.Return.String())
				fmt.Printf("        Method params: %d, Interface params: %d\n",
					len(methodInfo.FuncType.Params), len(requiredMethod.FuncType.Params))
			}
			return false
		}
	}

	return true
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
	// none can be assigned to any optional type (T?) or empty interface (any)
	if source.Equals(types.TypeNone) {
		if _, ok := target.(*types.OptionalType); ok {
			return ImplicitCastable
		}
		// Check if target is an empty interface (interface{})
		if iface, ok := target.(*types.InterfaceType); ok && len(iface.Methods) == 0 {
			return ImplicitCastable
		}
		// Check if target is a named type wrapping an empty interface
		if named, ok := target.(*types.NamedType); ok {
			if iface, ok := named.Underlying.(*types.InterfaceType); ok && len(iface.Methods) == 0 {
				return ImplicitCastable
			}
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
			return ImplicitCastable
		}
		if types.IsUntypedFloat(source) && types.IsFloat(targetUnwrapped) {
			return ImplicitCastable
		}
		return Incompatible
	}

	if isEnumType(source) && isIntegerType(target) {
		return ExplicitCastable
	}
	if isBoolType(source) && isIntegerType(target) {
		return ExplicitCastable
	}

	// Check numeric conversions
	if types.IsNumeric(source) && types.IsNumeric(target) {
		// Special case: byte and u8 are internally the same size but require explicit cast
		srcName, srcOk := types.GetPrimitiveName(source)
		tgtName, tgtOk := types.GetPrimitiveName(target)
		if srcOk && tgtOk {
			// byte -> u8 or u8 -> byte requires explicit cast
			if (srcName == types.TYPE_BYTE && tgtName == types.TYPE_U8) || (srcName == types.TYPE_U8 && tgtName == types.TYPE_BYTE) {
				return ExplicitCastable
			}
			if types.IsIntegerTypeName(srcName) && types.IsFloatTypeName(tgtName) {
				if isLosslessNumericConversion(source, target) {
					return ImplicitCastable
				}
				return ExplicitCastable
			}

			if types.IsFloatTypeName(srcName) && types.IsIntegerTypeName(tgtName) {
				return ExplicitCastable
			}
		}

		if isLosslessNumericConversion(source, target) {
			return ImplicitCastable
		}
		return ExplicitCastable
	}

	// Check interface compatibility
	// If target is an interface, check if source implements it
	if targetIface, ok := target.(*types.InterfaceType); ok {
		// This requires context to look up methods, so we'll handle it in checkTypeCompatibilityWithContext
		// For now, return a special value that indicates we need context
		// Actually, we can't do this here without context. Let's handle it differently.
		// We'll check this in the caller with context.
		_ = targetIface
	}

	// Check if target is a NamedType and source matches its underlying type
	// This allows: i32 -> Integer where type Integer i32;
	// But NOT: Integer -> i32 (reverse) or Integer1 -> Integer2 (named to named)
	// These require explicit casting to preserve type safety
	if targetNamed, ok := target.(*types.NamedType); ok {
		targetUnderlying := targetNamed.Unwrap()
		// Only allow if source is NOT a NamedType (i.e., source is the base type)
		// This ensures we only allow base -> named, not named -> named or named -> base
		if _, sourceIsNamed := source.(*types.NamedType); !sourceIsNamed {
			// Source is a base type, check if it matches the underlying type
			if source.Equals(targetUnderlying) {
				return ImplicitCastable
			}
		}
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
				return ImplicitCastable
			}
		}
	}

	// No implicit conversion available
	return Incompatible
}

// checkTypeCompatibilityWithContext determines if source type can be used where target type is expected
// This version includes context for interface satisfaction checking
func checkTypeCompatibilityWithContext(ctx *context_v2.CompilerContext, mod *context_v2.Module, source, target types.SemType) TypeCompatibility {
	// Handle unknown types
	if source.Equals(types.TypeUnknown) || target.Equals(types.TypeUnknown) {
		return Incompatible
	}

	// Automatic dereferencing: &T is compatible with T
	// This allows reference types to be used transparently
	source = dereferenceType(source)
	target = dereferenceType(target)

	// Special handling for none
	// none can be assigned to any optional type (T?) or empty interface (interface{})
	if source.Equals(types.TypeNone) {
		if _, ok := target.(*types.OptionalType); ok {
			return ImplicitCastable
		}
		// Check if target is an empty interface (any)
		if iface, ok := target.(*types.InterfaceType); ok && len(iface.Methods) == 0 {
			return ImplicitCastable
		}
		// Check if target is a named type wrapping an empty interface
		if named, ok := target.(*types.NamedType); ok {
			if iface, ok := named.Underlying.(*types.InterfaceType); ok && len(iface.Methods) == 0 {
				return ImplicitCastable
			}
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
			return ImplicitCastable
		}
		if types.IsUntypedFloat(source) && types.IsFloat(targetUnwrapped) {
			return ImplicitCastable
		}
		return Incompatible
	}

	if isEnumType(source) && isIntegerType(target) {
		return ExplicitCastable
	}
	if isBoolType(source) && isIntegerType(target) {
		return ExplicitCastable
	}

	// Check numeric conversions
	if types.IsNumericType(source) && types.IsNumericType(target) {
		// Special case: byte and u8 are internally the same size but require explicit cast
		srcName, srcOk := types.GetPrimitiveName(source)
		tgtName, tgtOk := types.GetPrimitiveName(target)
		if srcOk && tgtOk {
			// byte -> u8 or u8 -> byte requires explicit cast
			if (srcName == types.TYPE_BYTE && tgtName == types.TYPE_U8) || (srcName == types.TYPE_U8 && tgtName == types.TYPE_BYTE) {
				return ExplicitCastable
			}
			if types.IsIntegerTypeName(srcName) && types.IsFloatTypeName(tgtName) {
				if isLosslessNumericConversion(source, target) {
					return ImplicitCastable
				}
				return ExplicitCastable
			}
			if types.IsFloatTypeName(srcName) && types.IsIntegerTypeName(tgtName) {
				return ExplicitCastable
			}
		}

		if isLosslessNumericConversion(source, target) {
			return ImplicitCastable
		}
		return ExplicitCastable
	}

	// Check interface compatibility
	// If target is an interface, check if source implements it
	// Handle both direct InterfaceType and NamedType wrapping an InterfaceType
	var targetIface *types.InterfaceType
	if iface, ok := target.(*types.InterfaceType); ok {
		targetIface = iface
	} else if named, ok := target.(*types.NamedType); ok {
		if iface, ok := named.Underlying.(*types.InterfaceType); ok {
			targetIface = iface
		}
	}
	if targetIface != nil {
		if implementsInterface(ctx, mod, source, targetIface) {
			return ImplicitCastable
		}
	}

	// Check if target is a NamedType and source matches its underlying type
	// This allows: i32 -> Integer where type Integer i32; (implicit)
	// But NOT: Integer -> i32 (reverse) or Integer1 -> Integer2 (named to named) - these require explicit cast
	if targetNamed, ok := target.(*types.NamedType); ok {
		targetUnderlying := targetNamed.Unwrap()
		// Only allow if source is NOT a NamedType (i.e., source is the base type)
		// This ensures we only allow base -> named, not named -> named or named -> base
		if _, sourceIsNamed := source.(*types.NamedType); !sourceIsNamed {
			// Source is a base type, check if it matches the underlying type
			if source.Equals(targetUnderlying) {
				return ImplicitCastable
			}
		}
	}

	// Check for explicit castable cases (named -> base, named -> named with same underlying)
	// These require explicit casting to preserve type safety
	srcUnwrapped := types.UnwrapType(source)
	tgtUnwrapped := types.UnwrapType(target)

	// If underlying types are compatible, it's explicitly castable (not implicitly)
	if types.IsNumeric(srcUnwrapped) && types.IsNumeric(tgtUnwrapped) {
		// Both are numeric, can be explicitly cast
		return ExplicitCastable
	}

	// If underlying types are the same, can be explicitly cast
	if srcUnwrapped.Equals(tgtUnwrapped) {
		return ExplicitCastable
	}

	// Check struct compatibility (unwrap NamedType to get underlying structure)
	// If target is a NamedType and source matches the underlying structure,
	// allow assignment (e.g., { .x = 1, .y = 2 } can be assigned to Point if Point wraps struct { .x: i32, .y: i32 })
	if srcStruct, srcOk := srcUnwrapped.(*types.StructType); srcOk {
		if tgtStruct, tgtOk := tgtUnwrapped.(*types.StructType); tgtOk {
			// Check if structs are structurally compatible
			if areStructsCompatible(srcStruct, tgtStruct) {
				// If target is a NamedType, this is a structural -> nominal conversion (implicit castable)
				// If both are anonymous or both are named and identical, it's identical
				if source.Equals(target) {
					return Identical
				}
				return ImplicitCastable
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
// getExplicitCastHint returns a help message suggesting an explicit cast
func getExplicitCastHint(exprText string, targetType types.SemType) string {
	if exprText == "" {
		exprText = "expression"
	}
	return fmt.Sprintf("use an explicit cast: %s as %s", exprText, targetType.String())
}

// getConversionHint returns a hint message based on compatibility result
// Returns empty string if no hint is needed (Identical, ImplicitCastable) or incompatible
func getConversionHint(source, target types.SemType, compatibility TypeCompatibility, exprText string) string {
	switch compatibility {
	case Identical, ImplicitCastable:
		// No hint needed - these work implicitly
		return ""
	case ExplicitCastable:
		// Suggest explicit cast
		return getExplicitCastHint(exprText, target)
	case Incompatible:
		// No hint - cannot be cast
		return ""
	default:
		return ""
	}
}

func getConversionError(source, target types.SemType, compatibility TypeCompatibility) string {
	switch compatibility {
	case Incompatible:
		return fmt.Sprintf("cannot use type '%s' as type '%s'", source, target)
	case ExplicitCastable:
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
		// Same size but different types (e.g., byte vs u8, or named types)
		return msg

	case Identical, ImplicitCastable:
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
