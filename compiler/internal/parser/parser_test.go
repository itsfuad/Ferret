package parser_test

import (
	"ferret/compiler/internal/parser"
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
		{"let x, y; x, y = 1, 2;", true, "Multiple declaration and assignment"},
		{"let x: i32 = 42;", true, "Typed variable declaration"},
		{"", true, "Empty file"},
		{"let", false, "Incomplete declaration"},
		{"x =", false, "Incomplete assignment"},
		{"let x = ;", false, "Missing expression in declaration"},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			filePath := testUtils.CreateTestFileWithContent(t, tt.input)
			p := parser.New(filePath, false)

			defer func() {
				if r := recover(); r != nil {
					if tt.isValid {
						t.Errorf("%s: expected no panic, got %v", tt.desc, r)
					}
				}
			}()

			nodes := p.Parse()

			if len(nodes) == 0 && tt.isValid && tt.input != "" {
				t.Errorf("%s: expected nodes, got none", tt.desc)
			}
		})
	}
}
