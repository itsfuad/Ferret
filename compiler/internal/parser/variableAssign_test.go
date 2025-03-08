package parser_test

import (
	"ferret/compiler/internal/parser"
	"ferret/compiler/testUtils"
	"testing"
)

func TestParseAssignment(t *testing.T) {
	tests := []struct {
		input   string
		isValid bool
		desc    string
	}{
		{"x = 42;", true, "Single variable assignment"},
		{"x, y = 42, 3.14;", true, "Multiple variable assignment"},
		{"x, y, z = 1, 2, 3;", true, "Three variable assignment"},
		{"x = \"hello\";", true, "String assignment"},
		{"x, y = 10, true;", true, "Mixed type assignment"},
		{"x, y = 42;", false, "Mismatched variables and values count"},
		{"x = 1, 2;", false, "More values than variables"},
		{"x, y = ;", false, "Missing values in assignment"},
		{"x, y = 1", false, "Missing semicolon"},
		{"x, = 1, 2;", false, "Invalid variable list"},
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

			if len(nodes) == 0 && tt.isValid {
				t.Errorf("%s: expected nodes, got none", tt.desc)
			}
		})
	}
}
