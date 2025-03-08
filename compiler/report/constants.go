package report

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
	EXPECTED_NUMBER        = "Expected number"
	EXPECTED_STRING        = "Expected string"
	EXPECTED_CLOSE_BRACKET = "Expected ']' after array elements"
)
