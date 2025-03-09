package parser

import (
	"testing"
)

func TestParseVarDecl(t *testing.T) {
	tests := []struct {
		input   string
		isValid bool
		desc    string
	}{
		{"let x;", true, "Valid single variable declaration"},
		{"let x, y;", true, "Valid multiple variable declaration"},
		{"let x: i32;", true, "Variable with type annotation"},
		{"let x, y: i32, str;", true, "Multiple variables with type annotations"},
		{"let x = 42;", true, "Variable initialized with a value"},
		{"let x, y = 42, 3.14;", true, "Multiple variables initialized with values"},
		{"let x, y: i32 = 10, 20;", true, "Typed variables initialized"},
		{"let p, q: i32, str = 10, \"hello\";", true, "Multiple typed variables initialized"},
		{"let x, y = p;", true, "Mismatched variable and value count"},
		{"let x, y: i32;", true, "Shared type annotation"},
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
