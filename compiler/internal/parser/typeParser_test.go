package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
	"ferret/compiler/testUtils"
	"testing"
)

const (
	errMsgFmt = "%s = %v, want %v"
)

func assertIntType(t *testing.T, result ast.DataType, expected types.TYPE_NAME, bitSize int, unsigned bool) {
	t.Helper()
	intType, ok := result.(*ast.IntType)
	if !ok {
		t.Fatalf("expected *ast.IntType, got %T", result)
	}
	if intType.TypeName != expected {
		t.Errorf(errMsgFmt, "TypeName", intType.TypeName, expected)
	}
	if intType.BitSize != bitSize {
		t.Errorf(errMsgFmt, "BitSize", intType.BitSize, bitSize)
	}
	if intType.Unsigned != unsigned {
		t.Errorf(errMsgFmt, "Unsigned", intType.Unsigned, unsigned)
	}
}

func assertFloatType(t *testing.T, result ast.DataType, expected types.TYPE_NAME, bitSize int) {
	t.Helper()
	floatType, ok := result.(*ast.FloatType)
	if !ok {
		t.Fatalf("expected *ast.FloatType, got %T", result)
	}
	if floatType.TypeName != expected {
		t.Errorf(errMsgFmt, "TypeName", floatType.TypeName, expected)
	}
	if floatType.BitSize != bitSize {
		t.Errorf(errMsgFmt, "BitSize", floatType.BitSize, bitSize)
	}
}

func assertArrayType(t *testing.T, result ast.DataType, expected types.TYPE_NAME) {
	t.Helper()
	arrayType, ok := result.(*ast.ArrayType)
	if !ok {
		t.Fatalf("expected *ast.ArrayType, got %T", result)
	}
	if arrayType.TypeName != expected {
		t.Errorf(errMsgFmt, "TypeName", arrayType.TypeName, expected)
	}
	if arrayType.ElementType == nil {
		t.Error("ElementType is nil")
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
		name     string
		input    []lexer.Token
		expected types.TYPE_NAME
	}{
		{
			name: "array of i32",
			input: []lexer.Token{
				{Kind: lexer.OPEN_BRACKET},
				{Kind: lexer.CLOSE_BRACKET},
				{Kind: lexer.IDENTIFIER_TOKEN, Value: "i32"},
				{Kind: lexer.EOF_TOKEN},
			},
			expected: types.ARRAY,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filePath := testUtils.CreateTestFile(t)
			p := New(filePath, false)
			p.tokens = tt.input

			result := parseArrayType(p)
			if result == nil {
				t.Fatalf("parseArrayType() returned nil for input %s", tt.name)
			}
			assertArrayType(t, result, tt.expected)
		})
	}
}

func TestParseType(t *testing.T) {
	tests := []struct {
		name     string
		input    []lexer.Token
		expected types.TYPE_NAME
	}{
		{
			name: "i32",
			input: []lexer.Token{
				{Kind: lexer.IDENTIFIER_TOKEN, Value: "i32"},
				{Kind: lexer.EOF_TOKEN},
			},
			expected: types.INT32,
		},
		{
			name: "f64",
			input: []lexer.Token{
				{Kind: lexer.IDENTIFIER_TOKEN, Value: "f64"},
				{Kind: lexer.EOF_TOKEN},
			},
			expected: types.FLOAT64,
		},
		{
			name: "str",
			input: []lexer.Token{
				{Kind: lexer.IDENTIFIER_TOKEN, Value: "str"},
				{Kind: lexer.EOF_TOKEN},
			},
			expected: types.STRING,
		},
		{
			name: "bool",
			input: []lexer.Token{
				{Kind: lexer.IDENTIFIER_TOKEN, Value: "bool"},
				{Kind: lexer.EOF_TOKEN},
			},
			expected: types.BOOL,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			filePath := testUtils.CreateTestFile(t)
			p := New(filePath, false)
			p.tokens = tt.input

			result := parseType(p)
			if result == nil {
				t.Fatalf("parseType() returned nil for input %s", tt.name)
			}

			if result.Type() != tt.expected {
				t.Errorf(errMsgFmt, "Type()", result.Type(), tt.expected)
			}
		})
	}
}
