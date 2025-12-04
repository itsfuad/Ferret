package diagnostics

import (
	"bytes"
	"strings"
	"testing"
)

func TestSyntaxHighlighter_Keywords(t *testing.T) {
	sh := NewSyntaxHighlighter(true)

	tests := []struct {
		name     string
		input    string
		hasColor bool
	}{
		{
			name:     "let keyword",
			input:    "let x := 42;",
			hasColor: true,
		},
		{
			name:     "const keyword",
			input:    "const PI := 3.14;",
			hasColor: true,
		},
		{
			name:     "fn keyword",
			input:    "fn hello() {}",
			hasColor: true,
		},
		{
			name:     "if else keywords",
			input:    "if x > 0 { } else { }",
			hasColor: true,
		},
		{
			name:     "return keyword",
			input:    "return 42;",
			hasColor: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output := sh.HighlightLine(tt.input)
			// Check if ANSI color codes are present
			hasColors := strings.Contains(output, "\x1b[")
			if hasColors != tt.hasColor {
				t.Errorf("HighlightLine() hasColors = %v, want %v", hasColors, tt.hasColor)
			}
		})
	}
}

func TestSyntaxHighlighter_Types(t *testing.T) {
	sh := NewSyntaxHighlighter(true)

	tests := []string{
		"let x: i32 = 10;",
		"let y: f64 = 3.14;",
		"let flag: bool = true;",
		"let text: str = \"hello\";",
		"let ch: char = 'a';",
	}

	for _, input := range tests {
		t.Run(input, func(t *testing.T) {
			output := sh.HighlightLine(input)
			// Should have color codes for types
			if !strings.Contains(output, "\x1b[") {
				t.Errorf("HighlightLine() should contain color codes for: %s", input)
			}
		})
	}
}

func TestSyntaxHighlighter_Literals(t *testing.T) {
	sh := NewSyntaxHighlighter(true)

	tests := []struct {
		name  string
		input string
	}{
		{"string", `let s = "hello world";`},
		{"character", `let c = 'a';`},
		{"integer", "let n = 42;"},
		{"float", "let f = 3.14159;"},
		{"escaped string", `let s = "hello \"world\"";`},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			output := sh.HighlightLine(tt.input)
			// Should have color codes
			if !strings.Contains(output, "\x1b[") {
				t.Errorf("HighlightLine() should contain color codes for: %s", tt.input)
			}
		})
	}
}

func TestSyntaxHighlighter_Comments(t *testing.T) {
	sh := NewSyntaxHighlighter(true)

	input := "let x = 42; // This is a comment"
	output := sh.HighlightLine(input)

	// Should have color codes for the comment
	if !strings.Contains(output, "\x1b[") {
		t.Error("HighlightLine() should contain color codes for comments")
	}
}

func TestSyntaxHighlighter_Toggle(t *testing.T) {
	sh := NewSyntaxHighlighter(false)

	// Disabled - should not have colors
	input := "let x: i32 = 42;"
	output := sh.HighlightLine(input)
	if strings.Contains(output, "\x1b[") {
		t.Error("HighlightLine() should not contain color codes when disabled")
	}
	if output != input {
		t.Errorf("HighlightLine() when disabled should return input unchanged, got: %s", output)
	}

	// Enable and check again
	sh.Enable()
	output = sh.HighlightLine(input)
	if !strings.Contains(output, "\x1b[") {
		t.Error("HighlightLine() should contain color codes when enabled")
	}

	// Disable and check again
	sh.Disable()
	output = sh.HighlightLine(input)
	if strings.Contains(output, "\x1b[") {
		t.Error("HighlightLine() should not contain color codes after disabling")
	}
}

func TestSyntaxHighlighter_HighlightWithColor(t *testing.T) {
	sh := NewSyntaxHighlighter(true)

	var buf bytes.Buffer
	input := "let x: i32 = 42;"

	sh.HighlightWithColor(input, &buf)

	output := buf.String()
	if !strings.Contains(output, "\x1b[") {
		t.Error("HighlightWithColor() should write color codes to buffer")
	}
}

func TestSyntaxHighlighter_Tokens(t *testing.T) {
	sh := NewSyntaxHighlighter(true)

	input := "let x: i32 = 42;"
	tokens := sh.Highlight(input)

	if len(tokens) == 0 {
		t.Error("Highlight() should return tokens")
	}

	// Check that we have different token types
	foundKeyword := false
	foundType := false
	foundNumber := false

	for _, token := range tokens {
		if strings.TrimSpace(token.Text) == "let" {
			foundKeyword = true
		}
		if strings.TrimSpace(token.Text) == "i32" {
			foundType = true
		}
		if strings.TrimSpace(token.Text) == "42" {
			foundNumber = true
		}
	}

	if !foundKeyword {
		t.Error("Highlight() should tokenize 'let' as keyword")
	}
	if !foundType {
		t.Error("Highlight() should tokenize 'i32' as type")
	}
	if !foundNumber {
		t.Error("Highlight() should tokenize '42' as number")
	}
}

func TestStripColors(t *testing.T) {
	input := "\x1b[31mRed text\x1b[0m and \x1b[32mGreen text\x1b[0m"
	expected := "Red text and Green text"

	output := StripColors(input)
	if output != expected {
		t.Errorf("StripColors() = %q, want %q", output, expected)
	}
}

func TestEmitter_SyntaxHighlightingToggle(t *testing.T) {
	var buf bytes.Buffer
	emitter := NewEmitter(&buf)

	// Should be enabled by default
	if !emitter.highlighter.IsEnabled() {
		t.Error("Syntax highlighting should be enabled by default")
	}

	// Test disable
	emitter.DisableSyntaxHighlighting()
	if emitter.highlighter.IsEnabled() {
		t.Error("Syntax highlighting should be disabled after DisableSyntaxHighlighting()")
	}

	// Test enable
	emitter.EnableSyntaxHighlighting()
	if !emitter.highlighter.IsEnabled() {
		t.Error("Syntax highlighting should be enabled after EnableSyntaxHighlighting()")
	}

	// Test SetSyntaxHighlighting
	emitter.SetSyntaxHighlighting(false)
	if emitter.highlighter.IsEnabled() {
		t.Error("Syntax highlighting should be disabled after SetSyntaxHighlighting(false)")
	}

	emitter.SetSyntaxHighlighting(true)
	if !emitter.highlighter.IsEnabled() {
		t.Error("Syntax highlighting should be enabled after SetSyntaxHighlighting(true)")
	}
}
