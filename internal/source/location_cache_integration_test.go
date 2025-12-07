package source

import (
	"testing"
)

// mockCache implements SourceCache interface for testing
type mockCache struct {
	files map[string][]string
}

func (m *mockCache) GetLinesRange(filepath string, startLine, endLine int) ([]string, bool) {
	lines, ok := m.files[filepath]
	if !ok {
		return nil, false
	}
	if startLine < 1 || endLine > len(lines) {
		return nil, false
	}
	return lines[startLine-1 : endLine], true
}

// TestGetTextWithCache verifies that GetText uses the cache when provided
func TestGetTextWithCache(t *testing.T) {
	filepath := "/virtual/test.fer"
	cache := &mockCache{
		files: map[string][]string{
			filepath: {
				"let x := 42;",
				"let y := \"hello\";",
				"let z := 3.14;",
			},
		},
	}

	tests := []struct {
		name     string
		location *Location
		want     string
	}{
		{
			name: "single line from cache",
			location: NewLocation(
				&filepath,
				&Position{Line: 1, Column: 1},
				&Position{Line: 1, Column: 4},
			),
			want: "let",
		},
		{
			name: "identifier from cache",
			location: NewLocation(
				&filepath,
				&Position{Line: 1, Column: 5},
				&Position{Line: 1, Column: 6},
			),
			want: "x",
		},
		{
			name: "full line from cache",
			location: NewLocation(
				&filepath,
				&Position{Line: 2, Column: 1},
				&Position{Line: 2, Column: 18},
			),
			want: `let y := "hello";`,
		},
		{
			name: "multi-line from cache",
			location: NewLocation(
				&filepath,
				&Position{Line: 1, Column: 1},
				&Position{Line: 3, Column: 15},
			),
			want: `let x := 42;
let y := "hello";
let z := 3.14;`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.location.GetText(cache)
			if got != tt.want {
				t.Errorf("GetText() = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestGetSourceLinesRangeWithCache verifies that GetSourceLinesRange uses cache
func TestGetSourceLinesRangeWithCache(t *testing.T) {
	filepath := "/virtual/cached.fer"
	cache := &mockCache{
		files: map[string][]string{
			filepath: {"line 1", "line 2", "line 3", "line 4", "line 5"},
		},
	}

	lines, err := GetSourceLinesRange(filepath, 2, 4, cache)
	if err != nil {
		t.Fatalf("GetSourceLinesRange() error = %v", err)
	}

	expected := []string{"line 2", "line 3", "line 4"}
	if len(lines) != len(expected) {
		t.Fatalf("got %d lines, want %d", len(lines), len(expected))
	}

	for i, line := range lines {
		if line != expected[i] {
			t.Errorf("line %d: got %q, want %q", i, line, expected[i])
		}
	}
}
