package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/testUtils"
	"fmt"
	"testing"
)

// TestResult tracks the results of test cases
type TestResult struct {
	Total   int
	Passed  int
	Failed  int
	Details []string
}

// Global test result tracker
var testResult = &TestResult{}

// ResetTestResult resets the test result tracker
func ResetTestResult() {
	testResult = &TestResult{}
}

// PrintTestResult prints the summary of test results
func PrintTestResult() {
	if testResult.Total > 0 {
		passRate := float64(testResult.Passed) / float64(testResult.Total) * 100
		fmt.Println("\nTest Results:")
		fmt.Println("-------------")
		fmt.Println("Total Tests:", testResult.Total)
		fmt.Println("Passed:", testResult.Passed)
		fmt.Println("Failed:", testResult.Failed)
		fmt.Printf("Pass Rate: %.2f%%\n", passRate)

		if testResult.Failed > 0 {
			fmt.Println("\nFailed Tests:")
			for _, detail := range testResult.Details {
				fmt.Println("-", detail)
			}
		}
	}
}

func testParseWithPanic(t *testing.T, input string, desc string, isValid bool) []ast.Node {
	t.Helper()
	filePath := testUtils.CreateTestFileWithContent(t, input)
	p := New(filePath, false)

	defer func() {
		r := recover()
		testResult.Total++

		// expected panic only for invalid tests
		if isValid {
			if r != nil {
				testResult.Failed++
				testResult.Details = append(testResult.Details, desc+" (unexpected panic)")
				t.Errorf(testUtils.ErrPanic, desc, r)
			} else {
				testResult.Passed++
			}
		} else {
			if r == nil {
				testResult.Failed++
				testResult.Details = append(testResult.Details, desc+" (expected panic)")
				t.Errorf(testUtils.ErrNoPanic, desc)
			} else {
				testResult.Passed++
			}
		}
	}()

	nodes := p.Parse()
	return nodes
}
