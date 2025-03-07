package parser

import (
	"os"
	"path/filepath"
	"testing"
)

// createTestFile creates a temporary test file and returns its path
func createTestFile(t *testing.T) string {
	dir := t.TempDir()
	filePath := filepath.Join(dir, "test.fer")
	file, err := os.Create(filePath)
	if err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}
	file.Close()
	return filePath
}
