package utils

import "compiler/internal/utils/strings"

// isExported checks if a name is exported based on capitalization (Go-style)
// Exported names start with uppercase letters
func IsExported(name string) bool {
	return strings.IsCapitalized(name)
}
