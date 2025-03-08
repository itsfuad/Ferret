package parser_test

import (
	"ferret/compiler/internal/parser"
	"ferret/compiler/testUtils"
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
		{"let x, y = 10;", false, "Mismatched variable and value count"},
		{"let x, y: i32;", false, "Mismatched variable and type count"},
	}

	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {

			filePath := testUtils.CreateTestFileWithContent(t, tt.input)
			p := parser.New(filePath, false)
			defer func() {
				if r := recover(); r != nil {
					if tt.isValid {
						t.Errorf("%s: expected no panic, got %v", tt.desc, r)
					} else {
						t.Logf("%s: expected panic, got %v", tt.desc, r)
					}
				}
			}()

			p.Parse()
		})
	}
}
