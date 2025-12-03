package fs

import (
	//"compiler/config"
	"os"
	"path/filepath"
	"strings"
)

// Check if file exists and is a regular file
func IsValidFile(filename string) bool {
	fileInfo, err := os.Stat(filename)
	return err == nil && fileInfo.Mode().IsRegular()
}

func IsDir(path string) bool {
	fileInfo, err := os.Stat(path)
	return err == nil && fileInfo.Mode().IsDir()
}

// NormalizePath normalizes a path to use forward slashes and removes leading/trailing slashes.
// This is a utility for FirstPart and LastPart to handle various input formats.
func NormalizePath(path string) string {
	if path == "" {
		return ""
	}
	// Replace backslashes with forward slashes
	normalized := strings.ReplaceAll(path, "\\", "/")
	// Remove leading and trailing slashes
	normalized = strings.Trim(normalized, "/")
	return normalized
}

// NormalizeImportPath normalizes an import path with additional processing:
// - Trims leading/trailing whitespace
// - Replaces backslashes with forward slashes
// - Collapses multiple consecutive slashes into one
// - Trims leading/trailing slashes
//
// Examples:
//   - " myproject/utils " -> "myproject/utils"
//   - "myproject//utils"  -> "myproject/utils"
//   - "/myproject/utils/" -> "myproject/utils"
//   - "myproject\utils"   -> "myproject/utils"
func NormalizeImportPath(importPath string) string {
	// Trim whitespace
	importPath = strings.TrimSpace(importPath)

	if importPath == "" {
		return ""
	}

	// Replace backslashes with forward slashes
	importPath = strings.ReplaceAll(importPath, "\\", "/")

	// Collapse multiple slashes into one
	for strings.Contains(importPath, "//") {
		importPath = strings.ReplaceAll(importPath, "//", "/")
	}

	// Trim leading/trailing slashes
	importPath = strings.Trim(importPath, "/")

	return importPath
}

// FirstPart extracts the first component of a path.
// Handles various path formats (Unix, Windows) and normalizes them automatically.
// Returns the first path segment without extension.
// Examples:
//   - "myproject/utils" -> "myproject"
//   - "dir\\file.txt" -> "dir"
//   - "/root/file.txt" -> "root"
//   - "" -> ""
func FirstPart(path string) string {
	if path == "" {
		return ""
	}

	normalized := NormalizePath(path)
	if normalized == "" {
		return ""
	}

	// Find first slash
	idx := strings.Index(normalized, "/")
	if idx == -1 {
		// No slash, return whole path without extension
		return strings.TrimSuffix(normalized, filepath.Ext(normalized))
	}

	// Return first segment without extension
	return strings.TrimSuffix(normalized[:idx], filepath.Ext(normalized[:idx]))
}

// LastPart extracts the last component of a path.
// Handles various path formats (Unix, Windows) and normalizes them automatically.
// Returns the last path segment without extension.
// Examples:
//   - "myproject/utils" -> "utils"
//   - "dir\\file.txt" -> "file"
//   - "dir/subdir/file.txt" -> "file"
//   - "" -> ""
func LastPart(path string) string {
	if path == "" {
		return ""
	}

	normalized := NormalizePath(path)
	if normalized == "" {
		return ""
	}

	// Find last slash
	idx := strings.LastIndex(normalized, "/")
	if idx == -1 {
		// No slash, return whole path without extension
		return strings.TrimSuffix(normalized, filepath.Ext(normalized))
	}

	// Return last segment without extension
	return strings.TrimSuffix(normalized[idx+1:], filepath.Ext(normalized[idx+1:]))
}
