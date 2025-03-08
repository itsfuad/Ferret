package parser_test

import (
	"ferret/compiler/internal/parser"
	"ferret/compiler/testUtils"
	"testing"
)

func TestExpressionParsing(t *testing.T) {
	tests := []struct {
		input   string
		isValid bool
		desc    string
	}{
		{"let x = a + b;", true, "Binary addition"},
		{"let x = a - b;", true, "Binary subtraction"},
		{"let x = a * b;", true, "Binary multiplication"},
		{"let x = a / b;", true, "Binary division"},
		{"let x = -a;", true, "Unary negation"},
		{"let x = !a;", true, "Unary not"},
		{"let x = a && b;", true, "Logical AND"},
		{"let x = a || b;", true, "Logical OR"},
		{"let x = (a + b) * c;", true, "Parenthesized expression"},
		{"let x = a > b;", true, "Greater than comparison"},
		{"let x = a < b;", true, "Less than comparison"},
		{"let x = a >= b;", true, "Greater than or equal comparison"},
		{"let x = a <= b;", true, "Less than or equal comparison"},
		{"let x = a == b;", true, "Equality comparison"},
		{"let x = a != b;", true, "Inequality comparison"},
		{"let x = a + ;", false, "Missing right operand"},
		{"let x = * b;", false, "Missing left operand"},
		{"let x = (a + b;", false, "Unclosed parenthesis"},
		{"let x = a + + b;", false, "Invalid consecutive operators"},
		{"let x = a++;", true, "Postfix increment"},
		{"let x = a--;", true, "Postfix decrement"},
		{"let x = ++a;", true, "Prefix increment"},
		{"let x = --a;", true, "Prefix decrement"},
		{"let x = ++++a;", false, "Invalid consecutive prefix increment"},
		{"let x = ----a;", false, "Invalid consecutive prefix decrement"},
		{"let x = a++++b;", false, "Invalid consecutive postfix increment"},
		{"let x = a----b;", false, "Invalid consecutive postfix decrement"},
		{"let x = ++a++;", false, "Invalid mix of prefix and postfix increment"},
		{"let x = --a--;", false, "Invalid mix of prefix and postfix decrement"},
		{"let x = ++;", false, "Missing operand for prefix increment"},
		{"let x = --;", false, "Missing operand for prefix decrement"},
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
