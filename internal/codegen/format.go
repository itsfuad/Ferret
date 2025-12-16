package codegen

import (
	"strings"
	"unicode"
)

// FormatCSource applies a minimal indentation pass to generated C code.
// It is intentionally lightweight (no external tools) and is only meant to
// improve readability when emitting kept C files. It does not change semantics.
func FormatCSource(src string) string {
	lines := strings.Split(src, "\n")
	var out []string
	indent := 0
	indentStr := "  "

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		if trimmed == "" {
			out = append(out, "")
			continue
		}

		firstNonSpace := firstNonSpaceChar(line)
		lineIndent := indent
		if firstNonSpace == '}' && lineIndent > 0 {
			lineIndent--
		}

		out = append(out, strings.Repeat(indentStr, lineIndent)+strings.TrimLeftFunc(line, unicode.IsSpace))

		opens, closes := countBraces(line)
		indent += opens - closes
		if indent < 0 {
			indent = 0
		}
	}

	return strings.Join(out, "\n")
}

func countBraces(line string) (int, int) {
	open := 0
	close := 0
	inSingle := false
	inDouble := false
	escape := false

	for _, r := range line {
		if escape {
			escape = false
			continue
		}
		if r == '\\' {
			escape = true
			continue
		}
		if r == '\'' && !inDouble {
			inSingle = !inSingle
			continue
		}
		if r == '"' && !inSingle {
			inDouble = !inDouble
			continue
		}
		if inSingle || inDouble {
			continue
		}
		switch r {
		case '{':
			open++
		case '}':
			close++
		}
	}
	return open, close
}

func firstNonSpaceChar(s string) rune {
	for _, r := range s {
		if !unicode.IsSpace(r) {
			return r
		}
	}
	return 0
}
