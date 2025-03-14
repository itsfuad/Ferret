package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/test"
	"ferret/compiler/testUtils"
	"testing"
)

func testParseWithPanic(t *testing.T, input string, desc string, isValid bool) {
	t.Helper()
	filePath := testUtils.CreateTestFileWithContent(t, input)
	p := New(filePath, false)

	nodes := []ast.Node{}

	defer func() {
		r := recover()

		test.TestInfo.Total++

		if isValid {
			// expectation is no error. So we expect no panic, or nodes len > 0
			if r == nil || len(nodes) > 0 {
				test.TestInfo.Passed++
			} else {
				test.TestInfo.Failed++
				test.TestInfo.Details = append(test.TestInfo.Details, desc+" (expected no panic or 0 nodes)")
				t.Errorf("expected no panic or no 0 nodes, got panic or %d nodes", len(nodes))
			}
		} else {
			// expectation is panic. So we expect either panic or node len == 0
			if r != nil || len(nodes) == 0 {
				test.TestInfo.Passed++
			} else {
				test.TestInfo.Failed++
				test.TestInfo.Details = append(test.TestInfo.Details, desc+" (expected panic or 0 nodes)")
				t.Errorf("expected panic or 0 nodes, got no panic or %d nodes", len(nodes))
			}
		}
	}()

	nodes = p.Parse()
}
