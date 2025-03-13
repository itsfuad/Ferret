package parser

import (
	"ferret/compiler/testUtils"
	"testing"
)

func TestParserBasics(t *testing.T) {
	tests := []struct {
		input   string
		isValid bool
		desc    string
	}{
		{"let x = 42;", true, "Variable declaration with initialization"},
		{"const y = true;", true, "Constant declaration"},
		{"x = 42; y = 10;", true, "Multiple statements"},
		{"let x; x = 42;", true, "Declaration followed by assignment"},
		{"let x, y;\nx, y = 1, 2;", true, "Multiple declaration and assignment"},
		{"let x: i32 = 42;", true, "Typed variable declaration"},
		{"", true, "Empty file"},
		{"a;", true, "Single unused statement"},
		{"a, b;", true, "Multiple unused statements"},
		{"let", false, "Incomplete declaration"},
		{"x =", false, "Incomplete assignment"},
		{"let x = ;", false, "Missing expression in declaration"},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			nodes := testParseWithPanic(t, tt.input, tt.desc, tt.isValid)

			if len(nodes) == 0 && tt.isValid && tt.input != "" {
				t.Errorf(testUtils.ErrNoNodes, tt.desc)
			}
		})
	}
}
