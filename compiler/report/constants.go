package report

// general error messages
const (
	MISSING_NAME      = "Expected variable name"
	MISSING_TYPE_NAME = "Expected type name"
	INVALID_TYPE_NAME = "Invalid type name"
)

// Error messages for number literals
const (
	INT_OUT_OF_RANGE   = "Integer value out of range"
	FLOAT_OUT_OF_RANGE = "Float value out of range"
	INVALID_NUMBER     = "Invalid number format"
)

// Error messages for array literals
const (
	ARRAY_EMPTY          = "Array literal must have at least one value"
	ARRAY_TRAILING_COMMA = "Trailing comma not allowed in array literal"
)

// Error messages for expected tokens
const (
	EXPECTED_NUMBER      = "Expected number"
	EXPECTED_STRING      = "Expected string"
	EXPECTED_SEMI_COLON  = "Expected ';'"
	EXPECTED_COMMA       = "Expected ','"
	EXPECTED_COLON       = "Expected ':'"
	EXPECTED_EQUALS      = "Expected '='"
	EXPECTED_OPEN_BRACE  = "Expected '{'"
	EXPECTED_CLOSE_BRACE = "Expected '}'"
)

// Error messages for variable declarations
const (
	MISMATCHED_VARIABLE_AND_TYPE_COUNT = "Mismatched variable and type count"
	SINGLE_VALUE_MULTIPLE_VARIABLES    = "Single value cannot be assigned to multiple variables"
)

// Error messages for binary expressions
const (
	MISSING_RIGHT_OPERAND = "Missing right operand"
	MISSING_LEFT_OPERAND  = "Missing left operand"
)

// Error messages for unary expressions
const (
	INVALID_CONSECUTIVE_OPERATORS = "Invalid consecutive operators"
)

// Error messages for comparison expressions
const (
	INVALID_COMPARISON_OPERATOR = "Invalid comparison operator"
)

// Error messages for increment/decrement operations
const (
	INVALID_INCREMENT_OPERAND     = "Invalid operand for increment operator"
	INVALID_DECREMENT_OPERAND     = "Invalid operand for decrement operator"
	INVALID_CONSECUTIVE_INCREMENT = "Invalid consecutive increment operators"
	INVALID_CONSECUTIVE_DECREMENT = "Invalid consecutive decrement operators"
	INVALID_MIX_OF_INCREMENT      = "Invalid mix of prefix and postfix increment operators"
	INVALID_MIX_OF_DECREMENT      = "Invalid mix of prefix and postfix decrement operators"
)

// Error messages for array operations
const (
	MISSING_INDEX_EXPRESSION   = "Missing array index expression"
	INVALID_INDEX_EXPRESSION   = "Invalid array index expression"
	INVALID_ARRAY_ELEMENT_TYPE = "Invalid array element type"
)

// Error messages for type declarations
const (
	EXPECTED_TYPE_NAME = "Expected type name"
	EXPECTED_TYPE      = "Expected type after type name"
	UNEXPECTED_TOKEN   = "Unexpected token"
)

// Error messages for object/struct operations
const (
	EXPECTED_FIELD_NAME           = "Expected field name"
	EXPECTED_FIELD_TYPE           = "Expected field type"
	EXPECTED_FIELD_VALUE          = "Expected field value"
	EXPECTED_COMMA_OR_CLOSE_BRACE = "Expected ',' or '}' after field"
	DUPLICATE_FIELD_NAME          = "Duplicate field name"
	EMPTY_STRUCT_NOT_ALLOWED      = "Empty structs are not allowed - must have at least one field"
	EXPECTED_STRUCT_KEYWORD       = "Expected 'struct' keyword"
)

// Error messages for syntax errors
const (
	UNEXPECTED_CURLY_BRACE = "Unexpected '{' - object literals can only appear in expression contexts (after '=', ':', ',', or 'return')"
)

// Method declaration errors
const (
	EXPECTED_FUNCTION_KEYWORD = "Expected 'fn' keyword"
	EXPECTED_RECEIVER_NAME    = "Expected receiver name"
	EXPECTED_RECEIVER_TYPE    = "Expected receiver type"
	EXPECTED_METHOD_NAME      = "Expected method name"
	EXPECTED_RETURN_TYPE      = "Expected return type"
	DUPLICATE_METHOD_NAME     = "Duplicate method name"
	EXPECTED_PARAMETER_NAME   = "Expected parameter name"
	EXPECTED_PARAMETER_TYPE   = "Expected parameter type"
)

// bracket errors
const (
	EXPECTED_OPEN_BRACKET  = "Expected '['"
	EXPECTED_CLOSE_BRACKET = "Expected ']'"
	EXPECTED_OPEN_CURLY    = "Expected '{'"
	EXPECTED_CLOSE_CURLY   = "Expected '}'"
	EXPECTED_OPEN_PAREN    = "Expected '('"
	EXPECTED_CLOSE_PAREN   = "Expected ')'"
)

// Statement errors
const (
	EXPECTED_RETURN_KEYWORD = "Expected 'return' keyword"
)

// New constant for the new error message
const (
	TRAILING_COMMA_NOT_ALLOWED = "Trailing comma not allowed in struct type definition"
)
