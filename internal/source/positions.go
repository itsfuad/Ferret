package source

// Position represents a specific location in the source code with line, column, and index information.
type Position struct {
	Line   int // Line number in the source code.
	Column int // Column number in the source code.
	Index  int // Index in the source code.
}

// Advance updates the Position by advancing it based on the bytes in the provided string.
// It increments the line number for newline bytes and the column number for other bytes.
// The index is incremented for each byte in the string.
// For tab characters, it advances the column by 4 spaces.
//
// Parameters:
// - toSkip: A string containing bytes to advance the position by.
//
// Returns:
// - A pointer to the updated Position.
func (p *Position) Advance(toSkip string) *Position {
	prevWasTab := false
	for _, char := range toSkip {
		switch char {
		case '\n':
			p.Line++
			p.Column = 1
			p.Index++
			prevWasTab = false
		case '\t':
			p.Column += 4 // Standard tab width
			p.Index++
			prevWasTab = true
		default:
			// Original behavior: don't increment column immediately after a tab
			if !prevWasTab {
				p.Column++
			}
			// Advance Index by the number of bytes this rune occupies
			p.Index += len(string(char))
			prevWasTab = false
		}
	}
	return p
}
