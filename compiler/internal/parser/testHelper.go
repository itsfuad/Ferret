package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/testUtils"
	"testing"
)

func testParseWithPanic(t *testing.T, input string, desc string, isValid bool) []ast.Node {
	t.Helper()
	filePath := testUtils.CreateTestFileWithContent(t, input)
	p := New(filePath, false)

	defer func() {
		r := recover()
		// expected panic only for invalid tests
		if isValid {
			if r != nil {
				t.Errorf(testUtils.ErrPanic, desc, r)
			}
		} else {
			if r == nil {
				t.Errorf(testUtils.ErrNoPanic, desc)
			}
		}
	}()

	nodes := p.Parse()

	return nodes
}
