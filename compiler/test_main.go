package main

import (
	"ferret/compiler/test"
	"os"
	"testing"
)

func TestMain(m *testing.M) {
	// Reset test results before running tests
	test.ResetTestResult()

	// Run all tests
	code := m.Run()

	// Print test results
	test.PrintTestResult()

	os.Exit(code)
}
