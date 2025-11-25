package colors

import (
	"fmt"
	"io"
)

// Print methods (default to stdout)
func (c COLOR) Printf(format string, args ...interface{}) {
	fmt.Printf(string(c)+format+string(RESET), args...)
}

func (c COLOR) Println(args ...interface{}) {
	fmt.Print(string(c))
	fmt.Println(args...)
	fmt.Print(string(RESET))
}

func (c COLOR) Print(args ...interface{}) {
	fmt.Print(string(c))
	fmt.Print(args...)
	fmt.Print(string(RESET))
}

// Fprint methods (write to specific writer)
func (c COLOR) Fprintf(w io.Writer, format string, args ...interface{}) {
	fmt.Fprintf(w, string(c)+format+string(RESET), args...)
}

func (c COLOR) Fprintln(w io.Writer, args ...interface{}) {
	fmt.Fprint(w, string(c))
	fmt.Fprintln(w, args...)
	fmt.Fprint(w, string(RESET))
}

func (c COLOR) Fprint(w io.Writer, args ...interface{}) {
	fmt.Fprint(w, string(c))
	fmt.Fprint(w, args...)
	fmt.Fprint(w, string(RESET))
}

func (c COLOR) Sprintf(format string, args ...interface{}) string {
	return string(c) + fmt.Sprintf(format, args...) + string(RESET)
}

func (c COLOR) Sprintln(args ...interface{}) string {
	return string(c) + fmt.Sprintln(args...) + string(RESET)
}

func (c COLOR) Sprint(args ...interface{}) string {
	return string(c) + fmt.Sprint(args...) + string(RESET)
}

// Helper functions
func PrintWithColor(color COLOR, args ...interface{}) {
	color.Print(args...)
}

func FprintWithColor(w io.Writer, color COLOR, args ...interface{}) {
	color.Fprint(w, args...)
}

func SprintWithColor(color COLOR, args ...interface{}) string {
	return color.Sprint(args...)
}

// StripANSI removes ANSI color codes from a string
func StripANSI(s string) string {
	result := ""
	inEscape := false
	for i := 0; i < len(s); i++ {
		if s[i] == '\033' && i+1 < len(s) && s[i+1] == '[' {
			inEscape = true
			i++
			continue
		}
		if inEscape {
			if (s[i] >= 'A' && s[i] <= 'Z') || (s[i] >= 'a' && s[i] <= 'z') {
				inEscape = false
			}
			continue
		}
		result += string(s[i])
	}
	return result
}

// ConvertANSIToHTML converts ANSI color codes to HTML span tags
func ConvertANSIToHTML(text string) string {
	// First, escape HTML entities
	result := replaceAll(text, "&", "&amp;")
	result = replaceAll(result, "<", "&lt;")
	result = replaceAll(result, ">", "&gt;")

	// Then replace ANSI codes with HTML
	ansiToHTMLColors := map[string]string{
		"\033[0m":        "</span>",
		"\033[30m":       "<span style=\"color: #000000\">",
		"\033[31m":       "<span style=\"color: #ef4444\">",
		"\033[32m":       "<span style=\"color: #10b981\">",
		"\033[33m":       "<span style=\"color: #f59e0b\">",
		"\033[34m":       "<span style=\"color: #3b82f6\">",
		"\033[35m":       "<span style=\"color: #a855f7\">",
		"\033[36m":       "<span style=\"color: #06b6d4\">",
		"\033[37m":       "<span style=\"color: #d1d5db\">",
		"\033[90m":       "<span style=\"color: #808080\">",
		"\033[91m":       "<span style=\"color: #f87171\">",
		"\033[92m":       "<span style=\"color: #34d399\">",
		"\033[93m":       "<span style=\"color: #fbbf24\">",
		"\033[94m":       "<span style=\"color: #60a5fa\">",
		"\033[95m":       "<span style=\"color: #c084fc\">",
		"\033[96m":       "<span style=\"color: #22d3ee\">",
		"\033[97m":       "<span style=\"color: #f3f4f6\">",
		"\033[1m":        "<span style=\"font-weight: bold\">",
		"\033[1;31m":     "<span style=\"color: #ef4444; font-weight: bold\">",
		"\033[1;32m":     "<span style=\"color: #10b981; font-weight: bold\">",
		"\033[1;33m":     "<span style=\"color: #f59e0b; font-weight: bold\">",
		"\033[1;34m":     "<span style=\"color: #3b82f6; font-weight: bold\">",
		"\033[1;35m":     "<span style=\"color: #a855f7; font-weight: bold\">",
		"\033[1;36m":     "<span style=\"color: #06b6d4; font-weight: bold\">",
		"\033[1;37m":     "<span style=\"color: #d1d5db; font-weight: bold\">",
		"\033[38;5;208m": "<span style=\"color: #ff8700\">",
		"\033[38;5;130m": "<span style=\"color: #af5f00\">",
		"\033[38;5;136m": "<span style=\"color: #af8700\">",
		"\033[38;5;213m": "<span style=\"color: #ff87ff\">",
		"\033[38;5;37m":  "<span style=\"color: #00af87\">",
		"\033[38;5;87m":  "<span style=\"color: #5fffff\">",
		"\033[38;5;201m": "<span style=\"color: #ff00ff\">",
		"\033[38;5;250m": "<span style=\"color: #bcbcbc\">",
		"\033[38;5;240m": "<span style=\"color: #585858\">",
		"\033[38;5;81m":  "<span style=\"color: #5fd7ff\">",
		"\033[38;5;120m": "<span style=\"color: #87ff87\">",
		"\033[38;5;229m": "<span style=\"color: #ffffaf\">",
	}

	for ansi, html := range ansiToHTMLColors {
		result = replaceAll(result, ansi, html)
	}

	return result
}

// Helper function for string replacement
func replaceAll(s, old, new string) string {
	result := ""
	for i := 0; i < len(s); {
		if i+len(old) <= len(s) && s[i:i+len(old)] == old {
			result += new
			i += len(old)
		} else {
			result += string(s[i])
			i++
		}
	}
	return result
}
