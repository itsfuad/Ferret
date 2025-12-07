package source

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLocationGetText(t *testing.T) {
	// Create a temporary test file
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.fer")

	content := `let x := 42;
let y := "hello world";
let z := x + y;`

	err := os.WriteFile(testFile, []byte(content), 0644)
	if err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	tests := []struct {
		name     string
		location *Location
		expected string
	}{
		{
			name: "single line - first declaration",
			location: NewLocation(
				&testFile,
				&Position{Line: 1, Column: 1},
				&Position{Line: 1, Column: 13},
			),
			expected: "let x := 42;",
		},
		{
			name: "single line - identifier only",
			location: NewLocation(
				&testFile,
				&Position{Line: 1, Column: 5},
				&Position{Line: 1, Column: 6},
			),
			expected: "x",
		},
		{
			name: "single line - string literal",
			location: NewLocation(
				&testFile,
				&Position{Line: 2, Column: 10},
				&Position{Line: 2, Column: 23},
			),
			expected: "\"hello world\"",
		},
		{
			name: "multi-line - all lines",
			location: NewLocation(
				&testFile,
				&Position{Line: 1, Column: 1},
				&Position{Line: 3, Column: 16},
			),
			expected: "let x := 42;\nlet y := \"hello world\";\nlet z := x + y;",
		},
		{
			name: "invalid location - nil filename",
			location: NewLocation(
				nil,
				&Position{Line: 1, Column: 1},
				&Position{Line: 1, Column: 5},
			),
			expected: "",
		},
		{
			name: "invalid location - out of range line",
			location: NewLocation(
				&testFile,
				&Position{Line: 10, Column: 1},
				&Position{Line: 10, Column: 5},
			),
			expected: "",
		},
		{
			name: "invalid location - out of range column",
			location: NewLocation(
				&testFile,
				&Position{Line: 1, Column: 100},
				&Position{Line: 1, Column: 200},
			),
			expected: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.location.GetText(nil)
			if result != tt.expected {
				t.Errorf("GetText() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestGetSourceLines(t *testing.T) {
	// Create a temporary test file
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.fer")

	content := "line1\nline2\nline3"
	err := os.WriteFile(testFile, []byte(content), 0644)
	if err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	lines, err := GetSourceLines(testFile)
	if err != nil {
		t.Fatalf("GetSourceLines() error = %v", err)
	}

	expected := []string{"line1", "line2", "line3"}
	if len(lines) != len(expected) {
		t.Fatalf("GetSourceLines() returned %d lines, want %d", len(lines), len(expected))
	}

	for i, line := range lines {
		if line != expected[i] {
			t.Errorf("Line %d = %q, want %q", i+1, line, expected[i])
		}
	}
}

func TestGetSourceLinesEmpty(t *testing.T) {
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "empty.fer")

	err := os.WriteFile(testFile, []byte(""), 0644)
	if err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	lines, err := GetSourceLines(testFile)
	if err != nil {
		t.Fatalf("GetSourceLines() error = %v", err)
	}

	if len(lines) != 0 {
		t.Errorf("GetSourceLines() for empty file returned %d lines, want 0", len(lines))
	}
}

func TestGetSourceLinesNonexistent(t *testing.T) {
	_, err := GetSourceLines("/nonexistent/file.fer")
	if err == nil {
		t.Error("GetSourceLines() expected error for nonexistent file, got nil")
	}
}

func TestGetSourceLinesRange(t *testing.T) {
	// Create a temporary test file
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.fer")

	content := "line1\nline2\nline3\nline4\nline5"
	err := os.WriteFile(testFile, []byte(content), 0644)
	if err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	tests := []struct {
		name      string
		startLine int
		endLine   int
		expected  []string
		wantErr   bool
	}{
		{
			name:      "single line",
			startLine: 2,
			endLine:   2,
			expected:  []string{"line2"},
		},
		{
			name:      "multiple lines",
			startLine: 2,
			endLine:   4,
			expected:  []string{"line2", "line3", "line4"},
		},
		{
			name:      "first line",
			startLine: 1,
			endLine:   1,
			expected:  []string{"line1"},
		},
		{
			name:      "last line",
			startLine: 5,
			endLine:   5,
			expected:  []string{"line5"},
		},
		{
			name:      "all lines",
			startLine: 1,
			endLine:   5,
			expected:  []string{"line1", "line2", "line3", "line4", "line5"},
		},
		{
			name:      "invalid range - start < 1",
			startLine: 0,
			endLine:   2,
			wantErr:   true,
		},
		{
			name:      "invalid range - end < start",
			startLine: 3,
			endLine:   2,
			wantErr:   true,
		},
		{
			name:      "out of range line",
			startLine: 10,
			endLine:   12,
			wantErr:   true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			lines, err := GetSourceLinesRange(testFile, tt.startLine, tt.endLine, nil)
			if tt.wantErr {
				if err == nil {
					t.Error("GetSourceLinesRange() expected error, got nil")
				}
				return
			}

			if err != nil {
				t.Fatalf("GetSourceLinesRange() error = %v", err)
			}

			if len(lines) != len(tt.expected) {
				t.Fatalf("GetSourceLinesRange() returned %d lines, want %d", len(lines), len(tt.expected))
			}

			for i, line := range lines {
				if line != tt.expected[i] {
					t.Errorf("Line %d = %q, want %q", i, line, tt.expected[i])
				}
			}
		})
	}
}

func TestGetSourceLinesRangeNonexistent(t *testing.T) {
	_, err := GetSourceLinesRange("/nonexistent/file.fer", 1, 5, nil)
	if err == nil {
		t.Error("GetSourceLinesRange() expected error for nonexistent file, got nil")
	}
}
