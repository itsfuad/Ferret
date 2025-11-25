package diagnostics

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"compiler/colors"
	"compiler/internal/source"
)

const (
	STR_MULTIPLIER = "%*d | "
	LINE_POS       = "%s--> %s:%d:%d\n"
)



// SourceCache caches source file contents for error reporting
type SourceCache struct {
	files map[string][]string
}

func NewSourceCache() *SourceCache {
	return &SourceCache{
		files: make(map[string][]string),
	}
}

// GetLine retrieves a specific line from a source file
func (sc *SourceCache) GetLine(filepath string, line int) (string, error) {
	if lines, ok := sc.files[filepath]; ok {
		if line > 0 && line <= len(lines) {
			return lines[line-1], nil
		}
		return "", fmt.Errorf("line %d out of range", line)
	}

	// Load file
	file, err := os.Open(filepath)
	if err != nil {
		return "", err
	}
	defer file.Close()

	lines := make([]string, 0)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return "", err
	}

	sc.files[filepath] = lines

	if line > 0 && line <= len(lines) {
		return lines[line-1], nil
	}

	return "", fmt.Errorf("line %d out of range", line)
}

// Emitter handles the rendering and output of diagnostics
type Emitter struct {
	cache               *SourceCache
	writer              io.Writer         // Where to write output (os.Stdout, string builder, etc.)
	currentLineNumWidth int               // Line number width for current diagnostic
}

// labelContext groups parameters for printing labels to reduce parameter count
type labelContext struct {
	filepath     string
	line         int
	startLine    int
	endLine      int
	startCol     int
	endCol       int
	label        Label
	lineNumWidth int
	severity     Severity
}

// NewEmitter creates an emitter that writes to a specific writer
func NewEmitter(w io.Writer) *Emitter {
	return &Emitter{
		cache:  NewSourceCache(),
		writer: w,
	}
}

// calculateLineNumWidthForDiagnostic calculates the line number width needed for all lines displayed in this diagnostic
func (e *Emitter) calculateLineNumWidthForDiagnostic(diag *Diagnostic) int {
	lineNumbers := make(map[int]bool)

	// Collect all line numbers that will be displayed
	for _, label := range diag.Labels {
		if label.Location == nil || label.Location.Start == nil {
			continue
		}

		start := label.Location.Start
		end := label.Location.End
		if end == nil {
			end = start
		}

		// Add the label lines
		for line := start.Line; line <= end.Line; line++ {
			lineNumbers[line] = true
		}

		// Add context lines (previous line if it exists and isn't empty)
		if start.Line > 1 {
			// We would need to check if the previous line has content, but for width calculation
			// we can just include it if it might be shown
			lineNumbers[start.Line-1] = true
		}
	}

	// Find the maximum line number
	maxLine := 0
	for line := range lineNumbers {
		if line > maxLine {
			maxLine = line
		}
	}

	// Return the width needed for this line number
	if maxLine == 0 {
		return 1
	}
	return len(fmt.Sprintf("%d", maxLine))
}

func (e *Emitter) Emit(diag *Diagnostic) {
	// Calculate line number width based on all lines that will be displayed for this diagnostic
	e.currentLineNumWidth = e.calculateLineNumWidthForDiagnostic(diag)

	// Print severity and message
	e.printHeader(diag)

	// Rust-style: If we have labels, print them intelligently
	if len(diag.Labels) > 0 {
		// Verify we have exactly one primary label
		primaryCount := 0
		var primaryLabel Label
		secondaryLabels := []Label{}

		for _, label := range diag.Labels {
			if label.Style == Primary {
				primaryCount++
				primaryLabel = label
			} else {
				secondaryLabels = append(secondaryLabels, label)
			}
		}

		if primaryCount == 0 {
			// No primary label - this shouldn't happen but handle gracefully
			for _, label := range diag.Labels {
				e.printLabel(diag.FilePath, label, diag.Severity)
			}
		} else if primaryCount > 1 {
			// Multiple primary labels - print warning
			labels := []string{}
			for _, label := range diag.Labels {
				labels = append(labels, fmt.Sprintf("%v", label))
			}
			panic("INTERNAL COMPILER ERROR: Multiple primary labels in diagnostic!: " + strings.Join(labels, ", "))
		} else {
			// Exactly one primary label - use Rust-style rendering
			if len(secondaryLabels) == 0 {
				// Only primary label
				e.printLabel(diag.FilePath, primaryLabel, diag.Severity)
			} else if len(secondaryLabels) == 1 &&
				primaryLabel.Location != nil &&
				primaryLabel.Location.Start != nil &&
				secondaryLabels[0].Location != nil &&
				secondaryLabels[0].Location.Start != nil &&
				primaryLabel.Location.Start.Line == secondaryLabels[0].Location.Start.Line {
				// Primary + one secondary on same line - use compact format
				e.printCompactDualLabel(diag.FilePath, primaryLabel, secondaryLabels[0], diag.Severity)
			} else {
				// Primary + multiple secondaries OR different lines - use routed format
				e.printRoutedLabels(diag.FilePath, primaryLabel, secondaryLabels, diag.Severity)
			}
		}
	}

	// Print notes
	for _, note := range diag.Notes {
		e.printNote(note)
	}

	// Print help
	if diag.Help != "" {
		e.printHelp(diag.Help)
	}

	fmt.Fprintln(e.writer)
}

func (e *Emitter) printHeader(diag *Diagnostic) {
	var color colors.COLOR
	var severityStr string

	switch diag.Severity {
	case Error:
		color = colors.BOLD_RED
		severityStr = "error"
	case Warning:
		color = colors.BOLD_YELLOW
		severityStr = "warning"
	case Info:
		color = colors.BOLD_CYAN
		severityStr = "info"
	case Hint:
		color = colors.BOLD_PURPLE
		severityStr = "hint"
	}

	color.Fprint(e.writer, severityStr)
	if diag.Code != "" {
		fmt.Fprintf(e.writer, "[%s]", diag.Code)
	}
	fmt.Fprint(e.writer, ": ")
	color.Fprintln(e.writer, diag.Message)
}

func (e *Emitter) printLabel(filepath string, label Label, severity Severity) {
	if label.Location == nil || label.Location.Start == nil {
		return
	}

	start := label.Location.Start
	end := label.Location.End
	if end == nil {
		end = start
	}

	// Print line number gutter width
	lineNumWidth := len(fmt.Sprintf("%d", start.Line))
	// Print location header
	colors.BLUE.Fprintf(e.writer, LINE_POS, strings.Repeat(" ", lineNumWidth), filepath, start.Line, start.Column)
	if end.Line > start.Line {
		endWidth := len(fmt.Sprintf("%d", end.Line))
		if endWidth > lineNumWidth {
			lineNumWidth = endWidth
		}
	}

	// Print separator
	fmt.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
	colors.GREY.Fprintln(e.writer, " |")

	// Create context for label printing
	ctx := labelContext{
		filepath:     filepath,
		startLine:    start.Line,
		endLine:      end.Line,
		startCol:     start.Column,
		endCol:       end.Column,
		label:        label,
		lineNumWidth: lineNumWidth,
		severity:     severity,
	}

	// For single-line errors
	if start.Line == end.Line {
		ctx.line = start.Line
		e.printSingleLineLabel(ctx)
	} else {
		// For multi-line errors
		e.printMultiLineLabel(ctx)
	}
}

func (e *Emitter) printSingleLineLabel(ctx labelContext) {
	// Try to get previous line for context (if not empty)
	if ctx.line > 1 {
		prevLine, err := e.cache.GetLine(ctx.filepath, ctx.line-1)
		if err == nil && strings.TrimSpace(prevLine) != "" {
			// Print previous line in grey for context
			colors.GREY.Fprintf(e.writer, STR_MULTIPLIER, ctx.lineNumWidth, ctx.line-1)
			colors.GREY.Fprintln(e.writer, prevLine)
		}
	}

	// Get the error line
	sourceLine, err := e.cache.GetLine(ctx.filepath, ctx.line)
	if err != nil {
		return
	}

	// Print line number and source (line number in grey)
	colors.GREY.Fprintf(e.writer, STR_MULTIPLIER, ctx.lineNumWidth, ctx.line)
	fmt.Fprintln(e.writer, sourceLine)

	// Print underline
	fmt.Fprint(e.writer, strings.Repeat(" ", ctx.lineNumWidth))
	colors.GREY.Fprint(e.writer, " |")

	// Calculate underline position and length
	padding := ctx.startCol - 1
	length := ctx.endCol - ctx.startCol
	if length <= 0 {
		length = 1
	}

	// Choose color and style based on severity and label style
	var underlineColor colors.COLOR
	var underlineChar string

	if ctx.label.Style == Primary {
		// Primary labels use severity-based colors
		switch ctx.severity {
		case Error:
			underlineColor = colors.RED
		case Warning:
			underlineColor = colors.YELLOW
		case Info:
			underlineColor = colors.BLUE
		case Hint:
			underlineColor = colors.PURPLE
		default:
			underlineColor = colors.RED
		}
		// Use ^ for single character, ~ for multiple
		if length == 1 {
			underlineChar = "^"
		} else {
			underlineChar = "~"
		}
	} else {
		underlineColor = colors.BLUE
		underlineChar = "-"
	}

	// Print padding and underline
	fmt.Fprint(e.writer, strings.Repeat(" ", padding))
	underlineColor.Fprint(e.writer, strings.Repeat(underlineChar, length))

	// Print label message (only if not empty)
	if ctx.label.Message != "" {
		underlineColor.Fprintf(e.writer, " %s", ctx.label.Message)
	}
	fmt.Fprintln(e.writer)

	// Print separator
	colors.GREY.Fprint(e.writer, strings.Repeat(" ", ctx.lineNumWidth))
	colors.GREY.Fprintln(e.writer, " |")
}

func (e *Emitter) printMultiLineLabel(ctx labelContext) {
	// Print start line
	sourceLine, err := e.cache.GetLine(ctx.filepath, ctx.startLine)
	if err != nil {
		return
	}

	// Check if there's content before the label on the start line
	hasContentBefore := ctx.startCol > 1

	// Line numbers and | are always grey
	lineNumColor := colors.GREY
	pipeColor := colors.GREY

	if hasContentBefore {
		lineNumColor = colors.GREY
	}

	lineNumColor.Fprintf(e.writer, STR_MULTIPLIER, ctx.lineNumWidth, ctx.startLine)
	fmt.Fprintln(e.writer, sourceLine)

	// Print underline for start
	lineNumColor.Fprint(e.writer, strings.Repeat(" ", ctx.lineNumWidth))
	pipeColor.Fprint(e.writer, " | ")

	var underlineColor colors.COLOR
	if ctx.label.Style == Primary {
		// Primary labels use severity-based colors
		switch ctx.severity {
		case Error:
			underlineColor = colors.BOLD_RED
		case Warning:
			underlineColor = colors.BOLD_YELLOW
		case Info:
			underlineColor = colors.BOLD_CYAN
		case Hint:
			underlineColor = colors.BOLD_PURPLE
		default:
			underlineColor = colors.RED
		}
	} else {
		underlineColor = colors.BLUE
	}

	padding := ctx.startCol - 1
	fmt.Fprint(e.writer, strings.Repeat(" ", padding))
	//underline the start to end of the first line. (skip the spaces before and after)
	//underlineColor.Fprint(e.writer, "^")
	if ctx.startCol < len(sourceLine) {
		underlineColor.Fprint(e.writer, strings.Repeat("~", len(strings.TrimSpace(sourceLine))-(ctx.startCol-1))+"\n")
	}

	// Print ellipsis for middle lines if there are many
	if ctx.endLine-ctx.startLine > 5 {
		lineNumColor.Fprint(e.writer, strings.Repeat(" ", ctx.lineNumWidth))
		pipeColor.Fprintln(e.writer, "...")
	} else {
		// Print middle lines
		for i := ctx.startLine + 1; i < ctx.endLine; i++ {
			line, err := e.cache.GetLine(ctx.filepath, i)
			if err != nil {
				continue
			}
			lineNumColor.Fprintf(e.writer, STR_MULTIPLIER, ctx.lineNumWidth, i)
			fmt.Fprintln(e.writer, line)
		}
	}

	// Print end line
	endSourceLine, err := e.cache.GetLine(ctx.filepath, ctx.endLine)
	if err == nil {
		lineNumColor.Fprintf(e.writer, STR_MULTIPLIER, ctx.lineNumWidth, ctx.endLine)
		fmt.Fprintln(e.writer, endSourceLine)

		// Print underline for end
		lineNumColor.Fprint(e.writer, strings.Repeat(" ", ctx.lineNumWidth))
		pipeColor.Fprint(e.writer, " | ")
		endPadding := ctx.endCol - 1
		fmt.Fprint(e.writer, strings.Repeat(" ", endPadding))
		underlineColor.Fprint(e.writer, "^")

		if ctx.label.Message != "" {
			underlineColor.Fprintf(e.writer, " %s", ctx.label.Message)
		}
		fmt.Fprintln(e.writer)
	}

	// Print separator
	lineNumColor.Fprint(e.writer, strings.Repeat(" ", ctx.lineNumWidth))
	pipeColor.Fprintln(e.writer, " |")
}

func (e *Emitter) printNote(note Note) {
	padding := 0
	if e.currentLineNumWidth > 0 {
		padding = e.currentLineNumWidth + 1 // +1 for the space before |
	}
	fmt.Fprint(e.writer, strings.Repeat(" ", padding))
	colors.CYAN.Fprint(e.writer, "= note: ")
	fmt.Fprintln(e.writer, note.Message)
}

func (e *Emitter) printHelp(help string) {
	padding := 2
	if e.currentLineNumWidth > 0 {
		padding = e.currentLineNumWidth + 1 // +1 for the space before |
	}
	fmt.Fprint(e.writer, strings.Repeat(" ", padding))
	colors.GREEN.Fprint(e.writer, "= help: ")
	fmt.Fprintln(e.writer, help)
}

// printCompactDualLabel prints two labels on same line (Rust-style)
// Rightmost label gets inline message, leftmost gets connector line below
func (e *Emitter) printCompactDualLabel(filepath string, primary Label, secondary Label, severity Severity) {
	if primary.Location == nil || primary.Location.Start == nil {
		return
	}
	if secondary.Location == nil || secondary.Location.Start == nil {
		return
	}

	line := primary.Location.Start.Line

	primaryStart := primary.Location.Start
	primaryEnd := primary.Location.End
	if primaryEnd == nil {
		primaryEnd = primaryStart
	}

	secondaryStart := secondary.Location.Start
	secondaryEnd := secondary.Location.End
	if secondaryEnd == nil {
		secondaryEnd = secondaryStart
	}

	// Determine which label is leftmost (gets connector) and rightmost (gets inline)
	var leftLabel, rightLabel Label
	var leftStart, leftEnd, rightStart, rightEnd *source.Position

	if primaryStart.Column < secondaryStart.Column {
		// Primary is on the left
		leftLabel = primary
		leftStart, leftEnd = primaryStart, primaryEnd
		rightLabel = secondary
		rightStart, rightEnd = secondaryStart, secondaryEnd
	} else {
		// Secondary is on the left (or same position)
		leftLabel = secondary
		leftStart, leftEnd = secondaryStart, secondaryEnd
		rightLabel = primary
		rightStart, rightEnd = primaryStart, primaryEnd
	}

	lineNumWidth := len(fmt.Sprintf("%d", line))

	// Print location header (point to rightmost/inline label)
	colors.BLUE.Fprintf(e.writer, LINE_POS, strings.Repeat(" ", lineNumWidth), filepath, line, rightStart.Column)

	// Print separator
	colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
	colors.GREY.Fprintln(e.writer, " |")

	// Try to get previous line for context (if not empty)
	if line > 1 {
		prevLine, err := e.cache.GetLine(filepath, line-1)
		if err == nil && strings.TrimSpace(prevLine) != "" {
			// Print previous line in grey for context
			colors.GREY.Fprintf(e.writer, STR_MULTIPLIER, lineNumWidth, line-1)
			colors.GREY.Fprintln(e.writer, prevLine)
		}
	}

	// Get source line
	sourceLine, err := e.cache.GetLine(filepath, line)
	if err != nil {
		return
	}

	// Print line number and source
	colors.GREY.Fprintf(e.writer, STR_MULTIPLIER, lineNumWidth, line)
	fmt.Fprintln(e.writer, sourceLine)

	// Calculate positions
	leftPadding := leftStart.Column - 1
	leftLength := leftEnd.Column - leftStart.Column
	if leftLength <= 0 {
		leftLength = 1
	}

	rightPadding := rightStart.Column - 1
	rightLength := rightEnd.Column - rightStart.Column
	if rightLength <= 0 {
		rightLength = 1
	}

	// Get colors based on label style
	leftColor := colors.BLUE
	rightColor := colors.BLUE

	if leftLabel.Style == Primary {
		leftColor = e.getSeverityColor(severity)
	}
	if rightLabel.Style == Primary {
		rightColor = e.getSeverityColor(severity)
	}

	// Determine character style based on label Style field
	var leftChar, rightChar string

	if leftLabel.Style == Primary {
		if leftLength == 1 {
			leftChar = "^"
		} else {
			leftChar = "~"
		}
	} else {
		leftChar = "-"
	}

	if rightLabel.Style == Primary {
		if rightLength == 1 {
			rightChar = "^"
		} else {
			rightChar = "~"
		}
	} else {
		rightChar = "-"
	}

	// Line 1: Show both underlines, right (inline) label's message
	colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
	colors.GREY.Fprint(e.writer, " | ")

	// Left label first
	fmt.Fprint(e.writer, strings.Repeat(" ", leftPadding))
	leftColor.Fprint(e.writer, strings.Repeat(leftChar, leftLength))

	// Space between labels
	spaceBetween := rightPadding - leftPadding - leftLength
	fmt.Fprint(e.writer, strings.Repeat(" ", spaceBetween))

	// Right label with inline message
	rightColor.Fprint(e.writer, strings.Repeat(rightChar, rightLength))
	if rightLabel.Message != "" {
		rightColor.Fprintf(e.writer, " %s", rightLabel.Message)
	}
	fmt.Fprintln(e.writer)

	// Line 2: Show vertical connector for left label
	colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
	colors.GREY.Fprint(e.writer, " | ")
	fmt.Fprint(e.writer, strings.Repeat(" ", leftPadding))
	leftColor.Fprintln(e.writer, "|")

	// Line 3: Show left label message with -- prefix (always dashes for connector)
	colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
	colors.GREY.Fprint(e.writer, " | ")
	fmt.Fprint(e.writer, strings.Repeat(" ", leftPadding))
	leftColor.Fprint(e.writer, "--") // Always use -- for connector message, regardless of style
	if leftLabel.Message != "" {
		leftColor.Fprintf(e.writer, " %s", leftLabel.Message)
	}
	fmt.Fprintln(e.writer)

	// Print separator
	colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
	colors.GREY.Fprintln(e.writer, " |")
}

// printRoutedLabels prints primary + multiple secondaries with routing (Rust-style)
// Handles cases where secondary labels are on different lines or need routing
func (e *Emitter) printRoutedLabels(filepath string, primary Label, secondaries []Label, severity Severity) {
	if primary.Location == nil || primary.Location.Start == nil {
		return
	}

	primaryLine := primary.Location.Start.Line
	primaryCol := primary.Location.Start.Column


	// Collect all line numbers we need to show
	lineNumbers := []int{primaryLine}
	for _, sec := range secondaries {
		if sec.Location != nil && sec.Location.Start != nil {
			secLine := sec.Location.Start.Line
			// Add line if not already included
			found := false
			for _, ln := range lineNumbers {
				if ln == secLine {
					found = true
					break
				}
			}
			if !found {
				lineNumbers = append(lineNumbers, secLine)
			}
		}
	}

	// Sort line numbers
	for i := 0; i < len(lineNumbers); i++ {
		for j := i + 1; j < len(lineNumbers); j++ {
			if lineNumbers[i] > lineNumbers[j] {
				lineNumbers[i], lineNumbers[j] = lineNumbers[j], lineNumbers[i]
			}
		}
	}

	lineNumWidth := len(fmt.Sprintf("%d", lineNumbers[len(lineNumbers)-1]))

	// Print location header
	colors.BLUE.Fprintf(e.writer, LINE_POS, strings.Repeat(" ", lineNumWidth), filepath, primaryLine, primaryCol)

	// Print separator
	colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
	colors.GREY.Fprintln(e.writer, " |")

	primaryColor := e.getSeverityColor(severity)
	secondaryColor := colors.BLUE

	// Print each line with appropriate labels
	for idx, lineNum := range lineNumbers {
		// Print ellipsis if there's a gap between lines
		if idx > 0 {
			prevLine := lineNumbers[idx-1]
			if lineNum-prevLine > 1 {
				colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
				colors.GREY.Fprintln(e.writer, "...")
				colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
				colors.GREY.Fprintln(e.writer, " |")
			}
		}

		// Try to get previous line for context (if not empty)
		if lineNum > 1 {
			// Check if previous line is not already shown
			isPrevShown := false
			if idx > 0 && lineNumbers[idx-1] == lineNum-1 {
				isPrevShown = true
			}

			if !isPrevShown {
				prevLine, err := e.cache.GetLine(filepath, lineNum-1)
				if err == nil && strings.TrimSpace(prevLine) != "" {
					// Print previous line in grey for context
					colors.GREY.Fprintf(e.writer, STR_MULTIPLIER, lineNumWidth, lineNum-1)
					colors.GREY.Fprintln(e.writer, prevLine)
				}
			}
		}

		sourceLine, err := e.cache.GetLine(filepath, lineNum)
		if err != nil {
			continue
		}

		// Print line number and source
		colors.GREY.Fprintf(e.writer, STR_MULTIPLIER, lineNumWidth, lineNum)
		fmt.Fprintln(e.writer, sourceLine)

		// Check for secondaries on this line first
		hasSecondary := false
		for _, sec := range secondaries {
			if sec.Location != nil && sec.Location.Start != nil && sec.Location.Start.Line == lineNum {
				hasSecondary = true
				break
			}
		}

		// Check if primary is on this line
		hasPrimary := lineNum == primaryLine

		// Only print underline section if there are labels on this line
		if hasPrimary || hasSecondary {
			colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
			colors.GREY.Fprint(e.writer, " | ")

			// Print primary label if on this line
			if hasPrimary {
				primaryStart := primary.Location.Start
				primaryEnd := primary.Location.End
				if primaryEnd == nil {
					primaryEnd = primaryStart
				}

				padding := primaryStart.Column - 1
				length := primaryEnd.Column - primaryStart.Column
				if length <= 0 {
					length = 1
				}

				char := "^"
				if length > 1 {
					char = "~"
				}

				fmt.Fprint(e.writer, strings.Repeat(" ", padding))
				primaryColor.Fprint(e.writer, strings.Repeat(char, length))
				if primary.Message != "" {
					primaryColor.Fprintf(e.writer, " %s", primary.Message)
				}
				fmt.Fprintln(e.writer)
			} else if hasSecondary {
				// Print secondary labels if on this line (and primary is not)
				for _, sec := range secondaries {
					if sec.Location != nil && sec.Location.Start != nil && sec.Location.Start.Line == lineNum {
						secStart := sec.Location.Start
						secEnd := sec.Location.End
						if secEnd == nil {
							secEnd = secStart
						}

						padding := secStart.Column - 1
						length := secEnd.Column - secStart.Column
						if length <= 0 {
							length = 1
						}

						fmt.Fprint(e.writer, strings.Repeat(" ", padding))
						secondaryColor.Fprint(e.writer, strings.Repeat("-", length))
						if sec.Message != "" {
							secondaryColor.Fprintf(e.writer, " %s", sec.Message)
						}
						fmt.Fprintln(e.writer)
						// Only print first secondary on this line
						break
					}
				}
			}
		}
	}

	// Print separator
	colors.GREY.Fprint(e.writer, strings.Repeat(" ", lineNumWidth))
	colors.GREY.Fprintln(e.writer, " |")
}

// getSeverityColor returns the color for a given severity
func (e *Emitter) getSeverityColor(severity Severity) colors.COLOR {
	switch severity {
	case Error:
		return colors.RED
	case Warning:
		return colors.YELLOW
	case Info:
		return colors.BLUE
	case Hint:
		return colors.PURPLE
	default:
		return colors.RED
	}
}