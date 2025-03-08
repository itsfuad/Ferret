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
		if r := recover(); r != nil {
			if isValid {
				t.Errorf(testUtils.ErrPanic, desc, r)
			}
		}
	}()

	nodes := p.Parse()

	return nodes
}
