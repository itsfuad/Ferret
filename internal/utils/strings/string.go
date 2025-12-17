package strings

import "strings"

func IsCapitalized(str string) bool {
	if len(str) == 0 {
		return false
	}
	firstChar := rune(str[0])
	return firstChar >= 'A' && firstChar <= 'Z'
}

func ToSentenceCase(str string) string {
	if len(str) == 0 {
		return str
	}
	return strings.ToUpper(str[:1]) + str[1:]
}

func ToUpperCase(str string) string {
	return strings.ToUpper(str)
}

func ToLowerCase(str string) string {
	return strings.ToLower(str)
}

func Pluralize(singular, plural string, count int) string {
	if count == 1 {
		return singular
	}
	return plural
}

// Contains checks if a string contains a substring.
// This is a more efficient implementation than manual searching.
func Contains(s, substr string) bool {
	return strings.Contains(s, substr)
}
