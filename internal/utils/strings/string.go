package strings

import "strings"

func IsCapitalized(str string) bool {
	if len(str) == 0 {
		return false
	}
	firstChar := rune(str[0])
	return firstChar >= 'A' && firstChar <= 'Z'
}

func Pluralize(singular, plural string, count int) string {
	if count == 1 {
		return singular
	}
	return plural
}

// ToIdentifier converts a path-ish string into a stable identifier-ish form.
// Useful for generating file names / C symbols / header guards from module paths.
func ToIdentifier(s string) string {
	if s == "" {
		return ""
	}
	replacer := strings.NewReplacer(
		"/", "_",
		"\\", "_",
		"-", "_",
		".", "_",
		"::", "_",
	)
	return replacer.Replace(s)
}

// ToHeaderGuard converts a module path into a C header guard name (including suffix).
func ToHeaderGuard(s string) string {
	return strings.ToUpper(ToIdentifier(s)) + "_H"
}
