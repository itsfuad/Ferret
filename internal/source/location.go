package source

import (
	"bufio"
	"fmt"
	"os"
)

// Location represents a span of source code with start and end positions
type Location struct {
	Start    *Position
	End      *Position
	Filename *string
}

// NewLocation creates a new Location with the given start and end positions
func NewLocation(filename *string, start, end *Position) *Location {
	return &Location{
		Filename: filename,
		Start:    start,
		End:      end,
	}
}

// Contains checks if the given position is within this location
func (l *Location) Contains(pos *Position) bool {
	if l.Start.Line > pos.Line || (l.Start.Line == pos.Line && l.Start.Column > pos.Column) {
		return false
	}
	if l.End.Line < pos.Line || (l.End.Line == pos.Line && l.End.Column < pos.Column) {
		return false
	}
	return true
}

func (l *Location) String() string {
	if l.Start == nil || l.End == nil {
		return "location(unknown)"
	}

	return fmt.Sprintf("location(%d:%d - %d:%d)", l.Start.Line, l.Start.Column, l.End.Line, l.End.Column)
}

// GetText extracts the source code text for this location.
// Returns empty string if the location is invalid or file cannot be read.
// Optimized to read only the required lines from the file.
func (l *Location) GetText() string {
	if l.Filename == nil || l.Start == nil || l.End == nil {
		return ""
	}

	lineStart := l.Start.Line
	lineEnd := l.End.Line
	colStart := l.Start.Column
	colEnd := l.End.Column

	// Validate line numbers
	if lineStart < 1 || lineEnd < 1 || lineStart > lineEnd {
		return ""
	}

	// Read only the lines we need
	lines, err := GetSourceLinesRange(*l.Filename, lineStart, lineEnd)
	if err != nil {
		return ""
	}

	// Single line case
	if lineStart == lineEnd {
		if len(lines) == 0 {
			return ""
		}
		line := lines[0]
		// Validate column bounds
		if colStart < 1 || colStart > len(line)+1 || colEnd < 1 || colEnd > len(line)+1 {
			return ""
		}
		return line[colStart-1 : colEnd-1]
	}

	// Multi-line case
	if len(lines) == 0 {
		return ""
	}

	var result string
	for i, line := range lines {
		actualLine := lineStart + i
		switch actualLine {
		case lineStart:
			// First line: from colStart to end
			if colStart >= 1 && colStart <= len(line)+1 {
				result += line[colStart-1:]
			}
		case lineEnd:
			// Last line: from start to colEnd
			if colEnd >= 1 && colEnd <= len(line)+1 {
				result += "\n" + line[:colEnd-1]
			}
		default:
			// Middle lines: entire line
			result += "\n" + line
		}
	}

	return result
}

// GetSourceLines reads a file and splits it into lines.
// This is a utility function that can be reused across the codebase.
// For better performance when reading specific line ranges, use GetSourceLinesRange.
func GetSourceLines(filepath string) ([]string, error) {
	content, err := os.ReadFile(filepath)
	if err != nil {
		return nil, err
	}

	if len(content) == 0 {
		return []string{}, nil
	}

	var lines []string
	start := 0
	for i := 0; i < len(content); i++ {
		if content[i] == '\n' {
			lines = append(lines, string(content[start:i]))
			start = i + 1
		}
	}
	// Add the last line if there's remaining content
	if start < len(content) {
		lines = append(lines, string(content[start:]))
	}
	return lines, nil
}

// GetSourceLinesRange reads only the specified range of lines from a file.
// This is more efficient than reading the entire file when you only need a few lines.
// Lines are 1-indexed. Returns the lines from startLine to endLine (inclusive).
func GetSourceLinesRange(filepath string, startLine, endLine int) ([]string, error) {
	if startLine < 1 || endLine < startLine {
		return nil, fmt.Errorf("invalid line range: %d-%d", startLine, endLine)
	}

	file, err := os.Open(filepath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lines := make([]string, 0, endLine-startLine+1)
	currentLine := 0

	for scanner.Scan() {
		currentLine++
		if currentLine < startLine {
			continue // Skip lines before our range
		}
		if currentLine > endLine {
			break // Stop reading after our range
		}
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	// Check if we got the expected number of lines
	if len(lines) == 0 && currentLine < startLine {
		return nil, fmt.Errorf("line %d out of range (file has %d lines)", startLine, currentLine)
	}

	return lines, nil
}
