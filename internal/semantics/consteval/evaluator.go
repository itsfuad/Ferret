package consteval

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/tokens"
	"compiler/internal/types"
	"math/big"
	"strconv"
)

// EvaluateExpr attempts to evaluate an expression to a compile-time constant value
// Returns nil if the expression cannot be evaluated at compile-time
func EvaluateExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression) *ConstValue {
	if expr == nil {
		return nil
	}

	switch e := expr.(type) {
	case *ast.BasicLit:
		return evaluateLiteral(e)

	case *ast.IdentifierExpr:
		return evaluateIdentifier(ctx, mod, e)

	case *ast.UnaryExpr:
		return evaluateUnary(ctx, mod, e)

	case *ast.BinaryExpr:
		return evaluateBinary(ctx, mod, e)

	case *ast.ParenExpr:
		return EvaluateExpr(ctx, mod, e.X)

	default:
		// Cannot evaluate: function calls, indexing, field access, etc.
		return nil
	}
}

// evaluateLiteral extracts the constant value from a literal
func evaluateLiteral(lit *ast.BasicLit) *ConstValue {
	switch lit.Kind {
	case ast.INT:
		// Parse as big integer
		val, ok := new(big.Int).SetString(lit.Value, 0)
		if !ok {
			return nil
		}
		// Determine type based on value size
		typ := inferIntType(val)
		return NewBigIntValue(val, typ)

	case ast.FLOAT:
		// Parse as big float
		val, _, err := big.ParseFloat(lit.Value, 10, 256, big.ToNearestEven)
		if err != nil {
			return nil
		}
		// Default to f64 for untyped floats
		return NewBigFloatValue(val, types.TypeF64)

	case ast.STRING:
		// String literal - remove quotes
		str, err := strconv.Unquote(lit.Value)
		if err != nil {
			// If unquote fails, use raw value without quotes
			str = lit.Value
			if len(str) >= 2 && str[0] == '"' && str[len(str)-1] == '"' {
				str = str[1 : len(str)-1]
			}
		}
		return NewStringValue(str)

	case ast.BOOL:
		val := lit.Value == "true"
		return NewBoolValue(val)

	default:
		return nil
	}
}

// inferIntType determines the appropriate integer type based on value size
func inferIntType(val *big.Int) types.SemType {
	// For untyped integer literals, default to i32 unless too large
	if val.IsInt64() {
		i64Val := val.Int64()
		if i64Val >= -128 && i64Val <= 127 {
			return types.TypeI8
		}
		if i64Val >= -32768 && i64Val <= 32767 {
			return types.TypeI16
		}
		if i64Val >= -2147483648 && i64Val <= 2147483647 {
			return types.TypeI32
		}
		return types.TypeI64
	}

	// Too large for i64, try i128 or i256
	return types.TypeI64 // Default fallback
}

// evaluateIdentifier looks up a variable/constant and returns its constant value if known
func evaluateIdentifier(ctx *context_v2.CompilerContext, mod *context_v2.Module, ident *ast.IdentifierExpr) *ConstValue {
	// Special case: true and false are parsed as identifiers but are boolean literals
	if ident.Name == "true" {
		return NewBoolValue(true)
	}
	if ident.Name == "false" {
		return NewBoolValue(false)
	}

	if mod == nil || mod.CurrentScope == nil {
		return nil
	}

	// Look up symbol in current scope
	sym, ok := mod.CurrentScope.Lookup(ident.Name)
	if !ok || sym == nil {
		return nil
	}

	// Return the stored constant value if available
	if sym.ConstValue != nil && sym.ConstValue.IsConstant() {
		// Type assertion to get the actual ConstValue
		if cv, ok := sym.ConstValue.(*ConstValue); ok {
			return cv
		}
	}

	return nil
}

// evaluateUnary evaluates unary operations (-x, !x, +x)
func evaluateUnary(ctx *context_v2.CompilerContext, mod *context_v2.Module, unary *ast.UnaryExpr) *ConstValue {
	operand := EvaluateExpr(ctx, mod, unary.X)
	if operand == nil {
		return nil
	}

	switch unary.Op.Kind {
	case tokens.MINUS_TOKEN:
		// Numeric negation
		if val, ok := operand.AsInt(); ok {
			negated := new(big.Int).Neg(val)
			return NewBigIntValue(negated, operand.Type)
		}
		if val, ok := operand.AsFloat(); ok {
			negated := new(big.Float).Neg(val)
			return NewBigFloatValue(negated, operand.Type)
		}

	case tokens.PLUS_TOKEN:
		// Unary plus - just return the operand
		if operand.Kind == ConstInt || operand.Kind == ConstFloat {
			return operand
		}

	case tokens.NOT_TOKEN:
		// Logical negation
		if val, ok := operand.AsBool(); ok {
			return NewBoolValue(!val)
		}
	}

	return nil
}

// evaluateBinary evaluates binary operations
func evaluateBinary(ctx *context_v2.CompilerContext, mod *context_v2.Module, binary *ast.BinaryExpr) *ConstValue {
	left := EvaluateExpr(ctx, mod, binary.X)
	right := EvaluateExpr(ctx, mod, binary.Y)

	if left == nil || right == nil {
		return nil
	}

	// Type compatibility check
	if left.Kind != right.Kind {
		return nil // Cannot operate on different types
	}

	switch binary.Op.Kind {
	// Arithmetic operations
	case tokens.PLUS_TOKEN:
		return evalAdd(left, right)
	case tokens.MINUS_TOKEN:
		return evalSubtract(left, right)
	case tokens.MUL_TOKEN:
		return evalMultiply(left, right)
	case tokens.DIV_TOKEN:
		return evalDivide(left, right)
	case tokens.MOD_TOKEN:
		return evalModulo(left, right)

	// Comparison operations
	case tokens.DOUBLE_EQUAL_TOKEN:
		return NewBoolValue(left.Equals(right))
	case tokens.NOT_EQUAL_TOKEN:
		return NewBoolValue(!left.Equals(right))
	case tokens.LESS_TOKEN:
		return evalLessThan(left, right)
	case tokens.GREATER_TOKEN:
		return evalGreaterThan(left, right)
	case tokens.LESS_EQUAL_TOKEN:
		return evalLessThanEqual(left, right)
	case tokens.GREATER_EQUAL_TOKEN:
		return evalGreaterThanEqual(left, right)

	// Logical operations
	case tokens.AND_TOKEN:
		return evalLogicalAnd(left, right)
	case tokens.OR_TOKEN:
		return evalLogicalOr(left, right)
	}

	return nil
}

// Arithmetic helpers
func evalAdd(left, right *ConstValue) *ConstValue {
	if leftVal, ok := left.AsInt(); ok {
		if rightVal, ok := right.AsInt(); ok {
			result := new(big.Int).Add(leftVal, rightVal)
			return NewBigIntValue(result, left.Type)
		}
	}
	if leftVal, ok := left.AsFloat(); ok {
		if rightVal, ok := right.AsFloat(); ok {
			result := new(big.Float).Add(leftVal, rightVal)
			return NewBigFloatValue(result, left.Type)
		}
	}
	return nil
}

func evalSubtract(left, right *ConstValue) *ConstValue {
	if leftVal, ok := left.AsInt(); ok {
		if rightVal, ok := right.AsInt(); ok {
			result := new(big.Int).Sub(leftVal, rightVal)
			return NewBigIntValue(result, left.Type)
		}
	}
	if leftVal, ok := left.AsFloat(); ok {
		if rightVal, ok := right.AsFloat(); ok {
			result := new(big.Float).Sub(leftVal, rightVal)
			return NewBigFloatValue(result, left.Type)
		}
	}
	return nil
}

func evalMultiply(left, right *ConstValue) *ConstValue {
	if leftVal, ok := left.AsInt(); ok {
		if rightVal, ok := right.AsInt(); ok {
			result := new(big.Int).Mul(leftVal, rightVal)
			return NewBigIntValue(result, left.Type)
		}
	}
	if leftVal, ok := left.AsFloat(); ok {
		if rightVal, ok := right.AsFloat(); ok {
			result := new(big.Float).Mul(leftVal, rightVal)
			return NewBigFloatValue(result, left.Type)
		}
	}
	return nil
}

func evalDivide(left, right *ConstValue) *ConstValue {
	if leftVal, ok := left.AsInt(); ok {
		if rightVal, ok := right.AsInt(); ok {
			if rightVal.Sign() == 0 {
				return nil // Division by zero
			}
			result := new(big.Int).Div(leftVal, rightVal)
			return NewBigIntValue(result, left.Type)
		}
	}
	if leftVal, ok := left.AsFloat(); ok {
		if rightVal, ok := right.AsFloat(); ok {
			if rightVal.Sign() == 0 {
				return nil // Division by zero
			}
			result := new(big.Float).Quo(leftVal, rightVal)
			return NewBigFloatValue(result, left.Type)
		}
	}
	return nil
}

func evalModulo(left, right *ConstValue) *ConstValue {
	if leftVal, ok := left.AsInt(); ok {
		if rightVal, ok := right.AsInt(); ok {
			if rightVal.Sign() == 0 {
				return nil // Modulo by zero
			}
			result := new(big.Int).Mod(leftVal, rightVal)
			return NewBigIntValue(result, left.Type)
		}
	}
	return nil
}

// Comparison helpers
func evalLessThan(left, right *ConstValue) *ConstValue {
	cmp := compareValues(left, right)
	if cmp == nil {
		return nil
	}
	return NewBoolValue(*cmp < 0)
}

func evalGreaterThan(left, right *ConstValue) *ConstValue {
	cmp := compareValues(left, right)
	if cmp == nil {
		return nil
	}
	return NewBoolValue(*cmp > 0)
}

func evalLessThanEqual(left, right *ConstValue) *ConstValue {
	cmp := compareValues(left, right)
	if cmp == nil {
		return nil
	}
	return NewBoolValue(*cmp <= 0)
}

func evalGreaterThanEqual(left, right *ConstValue) *ConstValue {
	cmp := compareValues(left, right)
	if cmp == nil {
		return nil
	}
	return NewBoolValue(*cmp >= 0)
}

// compareValues returns -1, 0, 1 for less, equal, greater
func compareValues(left, right *ConstValue) *int {
	if leftVal, ok := left.AsInt(); ok {
		if rightVal, ok := right.AsInt(); ok {
			cmp := leftVal.Cmp(rightVal)
			return &cmp
		}
	}
	if leftVal, ok := left.AsFloat(); ok {
		if rightVal, ok := right.AsFloat(); ok {
			cmp := leftVal.Cmp(rightVal)
			return &cmp
		}
	}
	if leftVal, ok := left.AsString(); ok {
		if rightVal, ok := right.AsString(); ok {
			var cmp int
			if leftVal < rightVal {
				cmp = -1
			} else if leftVal > rightVal {
				cmp = 1
			} else {
				cmp = 0
			}
			return &cmp
		}
	}
	return nil
}

// Logical helpers
func evalLogicalAnd(left, right *ConstValue) *ConstValue {
	if leftVal, ok := left.AsBool(); ok {
		if rightVal, ok := right.AsBool(); ok {
			return NewBoolValue(leftVal && rightVal)
		}
	}
	return nil
}

func evalLogicalOr(left, right *ConstValue) *ConstValue {
	if leftVal, ok := left.AsBool(); ok {
		if rightVal, ok := right.AsBool(); ok {
			return NewBoolValue(leftVal || rightVal)
		}
	}
	return nil
}

// EvaluateAsInt is a convenience function that evaluates an expression and returns its integer value
// Returns (value, true) if successful, (0, false) otherwise
func EvaluateAsInt(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression) (int64, bool) {
	val := EvaluateExpr(ctx, mod, expr)
	if val == nil {
		return 0, false
	}
	return val.AsInt64()
}

// EvaluateAsBool is a convenience function that evaluates an expression and returns its boolean value
// Returns (value, true) if successful, (false, false) otherwise
func EvaluateAsBool(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression) (bool, bool) {
	val := EvaluateExpr(ctx, mod, expr)
	if val == nil {
		return false, false
	}
	return val.AsBool()
}

// FormatValue returns a human-readable representation of a constant value
func FormatValue(val *ConstValue) string {
	if val == nil {
		return "<non-constant>"
	}
	return val.String()
}
