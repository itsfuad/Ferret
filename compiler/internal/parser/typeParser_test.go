package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
	"ferret/compiler/testUtils"
	"testing"
)

func assertIntType(t *testing.T, result ast.DataType, expected types.TYPE_NAME, bitSize int, unsigned bool) {
	t.Helper()
	intType, ok := result.(*ast.IntType)
	if !ok {
		t.Fatalf("expected *ast.IntType, got %T", result)
	}
	if intType.TypeName != expected {
		t.Errorf(testUtils.ErrMsgFmt, "TypeName", intType.TypeName, expected)
	}
	if intType.BitSize != bitSize {
		t.Errorf(testUtils.ErrMsgFmt, "BitSize", intType.BitSize, bitSize)
	}
	if intType.Unsigned != unsigned {
		t.Errorf(testUtils.ErrMsgFmt, "Unsigned", intType.Unsigned, unsigned)
	}
}

func assertFloatType(t *testing.T, result ast.DataType, expected types.TYPE_NAME, bitSize int) {
	t.Helper()
	floatType, ok := result.(*ast.FloatType)
	if !ok {
		t.Fatalf("expected *ast.FloatType, got %T", result)
	}
	if floatType.TypeName != expected {
		t.Errorf(testUtils.ErrMsgFmt, "TypeName", floatType.TypeName, expected)
	}
	if floatType.BitSize != bitSize {
		t.Errorf(testUtils.ErrMsgFmt, "BitSize", floatType.BitSize, bitSize)
	}
}

func assertType(t *testing.T, result ast.DataType, expected types.TYPE_NAME, input string) {
	t.Helper()
	if result == nil {
		t.Fatalf("parseType() returned nil for input %s", input)
	}
	if result.Type() != expected {
		t.Errorf(testUtils.ErrMsgFmt, "Type()", result.Type(), expected)
	}
}

func TestParseIntegerType(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected types.TYPE_NAME
		bitSize  int
		unsigned bool
	}{
		{"i8", "i8", types.INT8, 8, true},
		{"i16", "i16", types.INT16, 16, true},
		{"i32", "i32", types.INT32, 32, true},
		{"i64", "i64", types.INT64, 64, true},
		{"u8", "u8", types.UINT8, 8, false},
		{"u16", "u16", types.UINT16, 16, false},
		{"u32", "u32", types.UINT32, 32, false},
		{"u64", "u64", types.UINT64, 64, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filePath := testUtils.CreateTestFileWithContent(t, tt.input)
			p := New(filePath, false)
			p.tokens = []lexer.Token{
				{Kind: lexer.IDENTIFIER_TOKEN, Value: tt.input},
				{Kind: lexer.EOF_TOKEN},
			}

			result := parseIntegerType(p)
			if result == nil {
				t.Fatalf("parseIntegerType() returned nil for input %s", tt.input)
			}
			assertIntType(t, result, tt.expected, tt.bitSize, tt.unsigned)
		})
	}
}

func TestParseFloatType(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected types.TYPE_NAME
		bitSize  int
	}{
		{"f32", "f32", types.FLOAT32, 32},
		{"f64", "f64", types.FLOAT64, 64},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filePath := testUtils.CreateTestFileWithContent(t, tt.input)
			p := New(filePath, false)
			p.tokens = []lexer.Token{
				{Kind: lexer.IDENTIFIER_TOKEN, Value: tt.input},
				{Kind: lexer.EOF_TOKEN},
			}

			result := parseFloatType(p)
			if result == nil {
				t.Fatalf("parseFloatType() returned nil for input %s", tt.input)
			}
			assertFloatType(t, result, tt.expected, tt.bitSize)
		})
	}
}

func TestParseArrayType(t *testing.T) {

	tests := []struct {
		input    string
		isValid  bool
		desc     string
		expected types.TYPE_NAME
	}{
		{
			desc:     "Basic integer array type",
			input:    "type Array []i32;",
			isValid:  true,
			expected: types.ARRAY,
		},
		{
			desc:     "Basic float array type",
			input:    "type Array []f64;",
			isValid:  true,
			expected: types.ARRAY,
		},
		{
			desc:     "Basic string array type",
			input:    "type Array []str;",
			isValid:  true,
			expected: types.ARRAY,
		},
		{
			desc:     "Basic boolean array type",
			input:    "type Array []bool;",
			isValid:  true,
			expected: types.ARRAY,
		},
		{
			desc:     "Basic object array type",
			input:    "type Array []User;",
			isValid:  true,
			expected: types.ARRAY,
		},
		{
			desc:     "Invalid array",
			input:    "type Array []",
			isValid:  false,
			expected: types.ARRAY,
		},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			testParseWithPanic(t, tt.input, tt.desc, tt.isValid)
		})
	}
}

func TestParseType(t *testing.T) {
	tests := []struct {
		input    string
		isValid  bool
		desc     string
		expected types.TYPE_NAME
	}{
		{
			desc:     "Basic integer type",
			input:    "i32",
			isValid:  true,
			expected: types.INT32,
		},
		{
			desc:     "Basic float type",
			input:    "f64",
			isValid:  true,
			expected: types.FLOAT64,
		},
		{
			desc:     "String type",
			input:    "str",
			isValid:  true,
			expected: types.STRING,
		},
		{
			desc:     "Boolean type",
			input:    "bool",
			isValid:  true,
			expected: types.BOOL,
		},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			filePath := testUtils.CreateTestFileWithContent(t, tt.input)
			p := New(filePath, false)
			result := parseType(p)

			if !tt.isValid && result != nil {
				t.Errorf("Expected nil result for invalid input %s", tt.input)
				return
			}

			if tt.isValid {
				assertType(t, result, tt.expected, tt.input)
			}
		})
	}
}

func TestTypeDeclaration(t *testing.T) {
	tests := []struct {
		input   string
		isValid bool
		desc    string
	}{
		{"type Integer i32;", true, "Basic type declaration"},
		{"type String str;", true, "String type declaration"},
		{"type Numbers []i32;", true, "Array type declaration"},
		{"type;", false, "Missing type name"},
		{"type Integer;", false, "Missing underlying type"},
		{"type Integer i32", false, "Missing semicolon"},
		{"type 123 i32;", false, "Invalid type name"},
		{"type Integer [];", false, "Invalid underlying type"},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			nodes := testParseWithPanic(t, tt.input, tt.desc, tt.isValid)
			if !tt.isValid && len(nodes) > 0 {
				t.Errorf("%s: expected no nodes, got %d", tt.desc, len(nodes))
			}
		})
	}
}

func TestObjectTypeParsing(t *testing.T) {
	tests := []struct {
		input   string
		isValid bool
		desc    string
	}{
		// Valid cases
		{"type User { name: str, age: i32 };", true, "Basic object type"},
		{"type Nested { user: { name: str, age: i32 } };", true, "Nested object type"},
		{"type WithArray { scores: []i32 };", true, "Object type with array field"},
		{"type Single { value: i32 };", true, "Single field object type"},
		{"type WithTrailing { name: str, };", true, "Object type with trailing comma"},

		// Invalid cases
		{"type Empty { };", false, "Empty object type"},
		{"type Invalid { name };", false, "Missing field type"},
		{"type Invalid { name: };", false, "Missing type after colon"},
		{"type Invalid { : str };", false, "Missing field name"},
		{"type Invalid { name: str age: i32 };", false, "Missing comma between fields"},
		{"type Invalid { name: str, name: i32 };", false, "Duplicate field names"},
		{"type Invalid { \"name\": str };", false, "Non-identifier field name"},
		{"type User { name: str, age: i32 }", false, "Missing semicolon"},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			nodes := testParseWithPanic(t, tt.input, tt.desc, tt.isValid)
			if !tt.isValid && len(nodes) > 0 {
				t.Errorf("%s: expected no nodes, got %d", tt.desc, len(nodes))
			}
		})
	}
}
