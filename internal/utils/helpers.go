package utils

func Ternary[T any](condition bool, trueValue, falseValue T) T {
	if condition {
		return trueValue
	}
	return falseValue
}

// isExported checks if a name is exported based on capitalization (Go-style)
// Exported names start with uppercase letters
func IsExported(name string) bool {
	if len(name) == 0 {
		return false
	}
	firstChar := rune(name[0])
	return firstChar >= 'A' && firstChar <= 'Z'
}