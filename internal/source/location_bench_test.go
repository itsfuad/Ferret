package source

import (
	"os"
	"path/filepath"
	"testing"
)

// BenchmarkGetSourceLines benchmarks reading entire file
func BenchmarkGetSourceLines(b *testing.B) {
	// Create a large test file
	tmpDir := b.TempDir()
	testFile := filepath.Join(tmpDir, "large.fer")

	// Create a file with 1000 lines
	var content string
	for i := 0; i < 1000; i++ {
		content += "let variable_" + string(rune(i)) + " := 42;\n"
	}

	err := os.WriteFile(testFile, []byte(content), 0644)
	if err != nil {
		b.Fatalf("Failed to create test file: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, _ = GetSourceLines(testFile)
	}
}

// BenchmarkGetSourceLinesRange benchmarks reading specific line range
func BenchmarkGetSourceLinesRange(b *testing.B) {
	// Create a large test file
	tmpDir := b.TempDir()
	testFile := filepath.Join(tmpDir, "large.fer")

	// Create a file with 1000 lines
	var content string
	for i := 0; i < 1000; i++ {
		content += "let variable_" + string(rune(i)) + " := 42;\n"
	}

	err := os.WriteFile(testFile, []byte(content), 0644)
	if err != nil {
		b.Fatalf("Failed to create test file: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Read only line 500
		_, _ = GetSourceLinesRange(testFile, 500, 500, nil)
	}
}

// BenchmarkGetTextSingleLine benchmarks GetText() for single line extraction
func BenchmarkGetTextSingleLine(b *testing.B) {
	// Create a large test file
	tmpDir := b.TempDir()
	testFile := filepath.Join(tmpDir, "large.fer")

	// Create a file with 1000 lines
	var content string
	for i := 0; i < 1000; i++ {
		content += "let variable_" + string(rune(i)) + " := 42;\n"
	}

	err := os.WriteFile(testFile, []byte(content), 0644)
	if err != nil {
		b.Fatalf("Failed to create test file: %v", err)
	}

	loc := NewLocation(
		&testFile,
		&Position{Line: 500, Column: 1},
		&Position{Line: 500, Column: 20},
	)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = loc.GetText(nil)
	}
}

// BenchmarkGetTextMultiLine benchmarks GetText() for multi-line extraction
func BenchmarkGetTextMultiLine(b *testing.B) {
	tmpDir := b.TempDir()
	testFile := filepath.Join(tmpDir, "large.fer")

	// Create a file with 1000 lines
	var content string
	for i := 0; i < 1000; i++ {
		content += "let variable_" + string(rune(i)) + " := 42;\n"
	}

	err := os.WriteFile(testFile, []byte(content), 0644)
	if err != nil {
		b.Fatalf("Failed to create test file: %v", err)
	}

	// Extract 10 lines from the middle
	loc := NewLocation(
		&testFile,
		&Position{Line: 500, Column: 1},
		&Position{Line: 509, Column: 20},
	)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = loc.GetText(nil)
	}
}
