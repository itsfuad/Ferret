package parser

import (
	"testing"
)

func TestLiteralParsing(t *testing.T) {
	tests := []struct {
		input   string
		isValid bool
		desc    string
	}{
		// Integer literals
		{"let x = 42;", true, "Decimal integer literal"},
		{"let x = -42;", true, "Negative decimal integer literal"},
		{"let x = 1_000_000;", true, "Decimal integer with underscores"},
		{"let x = 0xFF;", true, "Hexadecimal literal"},
		{"let x = 0xff;", true, "Lowercase hexadecimal literal"},
		{"let x = 0xDEAD_BEEF;", true, "Hexadecimal with underscores"},
		{"let x = 0o777;", true, "Octal literal"},
		{"let x = 0o1_234_567;", true, "Octal with underscores"},
		{"let x = 0b1010;", true, "Binary literal"},
		{"let x = 0b1010_1010;", true, "Binary with underscores"},

		// Float literals
		{"let x = 3.14;", true, "Float literal"},
		{"let x = -3.14;", true, "Negative float literal"},
		{"let x = 1_234.567_89;", true, "Float with underscores"},
		{"let x = 1.2e-10;", true, "Scientific notation"},
		{"let x = 1.2E+10;", true, "Scientific notation with uppercase E"},
		{"let x = -1.2e-10;", true, "Negative scientific notation"},
		{"let x = 1_234.567_89e-10;", true, "Scientific notation with underscores"},

		// String literals
		{"let x = \"hello\";", true, "String literal"},
		{"let x = true;", true, "Boolean literal true"},
		{"let x = false;", true, "Boolean literal false"},

		// Array literals
		{"let x = [1, 2, 3];", true, "Array literal with numbers"},
		{"let x = [\"a\", \"b\"];", true, "Array literal with strings"},
		{"let x = [1, \"a\", true];", true, "Array literal with mixed types"},
		{"let x: []i32 = [1];", true, "Array literal with type annotation"},
		{"let x: []str = [\"hello\"];", true, "String array with type annotation"},

		// Invalid literals
		{"let x = \"unclosed;", false, "Unclosed string literal"},
		{"let x = 12.34.56;", false, "Invalid float literal"},
		{"let x = 0x;", false, "Invalid hex literal"},
		{"let x = 0xG;", false, "Invalid hex digit"},
		{"let x = 0o8;", false, "Invalid octal digit"},
		{"let x = 0o;", false, "Invalid octal literal"},
		{"let x = 0b;", false, "Invalid binary literal"},
		{"let x = 0b2;", false, "Invalid binary digit"},
		{"let x = 1.2e;", false, "Invalid scientific notation"},
		{"let x = [];", false, "Empty array literal not allowed"},
		{"let x = [1, 2;", false, "Unclosed array literal"},
		{"let x = [,];", false, "Invalid array literal with just comma"},
		{"let x = [1 2];", false, "Array literal missing comma"},
		{"let x = [1, 2,];", false, "Array literal with trailing comma"},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			nodes := testParseWithPanic(t, tt.input, tt.desc, tt.isValid)

			if len(nodes) == 0 && tt.isValid {
				t.Errorf("%s: expected nodes, got none", tt.desc)
			}
		})
	}
}

func TestObjectLiteralParsing(t *testing.T) {
	tests := []struct {
		input   string
		isValid bool
		desc    string
	}{
		// Valid cases
		{`let x = { name: "John", age: 20 };`, true, "Basic object literal"},
		{`let x = { name: "John", age: 20, scores: [1, 2, 3] };`, true, "Object with array field"},
		{`let x = { user: { name: "John", age: 20 } };`, true, "Nested object literal"},
		{`let x = { single: 42 };`, true, "Single field object"},
		{`let x = { name: "John" };`, true, "Object without trailing comma"},
		{`let x = { name: "John", };`, true, "Object with trailing comma"},

		// Invalid cases
		{`let x = { };`, false, "Empty object not allowed"},
		{`let x = { name };`, false, "Missing field value"},
		{`let x = { name: };`, false, "Missing value after colon"},
		{`let x = { : "John" };`, false, "Missing field name"},
		{`let x = { name: "John" age: 20 };`, false, "Missing comma between fields"},
		{`let x = { name: "John", name: "Jane" };`, false, "Duplicate field names"},
		{`let x = { "name": "John" };`, false, "Non-identifier field name"},
		{`let x = { name: "John" }`, false, "Missing semicolon"},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			nodes := testParseWithPanic(t, tt.input, tt.desc, tt.isValid)

			if len(nodes) == 0 && tt.isValid {
				t.Errorf("%s: expected nodes, got none", tt.desc)
			}
		})
	}
}
