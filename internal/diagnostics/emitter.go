package diagnostics

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"

	"compiler/colors"
	"compiler/internal/source"
)

const (
	// Gutter formatting
	GUTTER_FMT   = "%*d | "
	GUTTER_BLANK = "%*s | "

	LINE_POS = "%s--> %s:%d:%d\n"
)

// SourceCache caches source file contents for error reporting
type SourceCache struct {
	files map[string][]string
	mu    sync.RWMutex // Protects files map during concurrent access
}

func NewSourceCache() *SourceCache {
	return &SourceCache{files: make(map[string][]string)}
}

// AddSource adds source content to the cache for a virtual file path
func (sc *SourceCache) AddSource(filepath, content string) {
	lines := strings.Split(content, "\n")
	sc.mu.Lock()
	sc.files[filepath] = lines
	sc.mu.Unlock()
}

// GetLine retrieves a specific line from a source file
func (sc *SourceCache) GetLine(filepath string, line int) (string, error) {
	sc.mu.RLock()
	lines, ok := sc.files[filepath]
	sc.mu.RUnlock()
	
	if ok {
		if line > 0 && line <= len(lines) {
			return lines[line-1], nil
		}
		return "", fmt.Errorf("line %d out of range", line)
	}

	file, err := os.Open(filepath)
	if err != nil {
		return "", err
	}
	defer file.Close()

	lines = make([]string, 0)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		return "", err
	}

	sc.mu.Lock()
	sc.files[filepath] = lines
	sc.mu.Unlock()

	if line > 0 && line <= len(lines) {
		return lines[line-1], nil
	}
	return "", fmt.Errorf("line %d out of range", line)
}

// Emitter handles the rendering and output of diagnostics
type Emitter struct {
	cache               *SourceCache
	writer              io.Writer
	currentLineNumWidth int // gutter width for current diagnostic
}

// labelContext groups parameters for printing labels
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
	return &Emitter{cache: NewSourceCache(), writer: w}
}

// ---- Gutter helpers (single source of truth) ----

// printGutter prints " <line> | " with consistent width/color.
func (e *Emitter) printGutter(line int) {
	colors.GREY.Fprintf(e.writer, GUTTER_FMT, e.currentLineNumWidth, line)
}

// printCurrentGutter prints " <line> | " in white (for main/source lines).
func (e *Emitter) printCurrentGutter(line int) {
	// if your colors package uses BOLD_WHITE, swap it in here
	colors.WHITE.Fprintf(e.writer, GUTTER_FMT, e.currentLineNumWidth, line)
}

// printBlankGutter prints "     | " with consistent width/color.
func (e *Emitter) printBlankGutter() {
	colors.GREY.Fprintf(e.writer, GUTTER_BLANK, e.currentLineNumWidth, "")
}

func (e *Emitter) printBlankGutterWithColor(color colors.COLOR) {
	color.Fprintf(e.writer, GUTTER_BLANK, e.currentLineNumWidth, "")
}

// printPipeOnly prints a separator line aligned under the gutter.
func (e *Emitter) printPipeOnly() {
	e.printBlankGutter()
	colors.GREY.Fprintln(e.writer)
}

// printPrevNonEmptyLine prints the previous non-empty line (if any) in grey.
func (e *Emitter) printPrevNonEmptyLine(filepath string, line int) {
	if line <= 1 {
		return
	}
	prevLine, err := e.cache.GetLine(filepath, line-1)
	if err != nil {
		return
	}
	if strings.TrimSpace(prevLine) == "" {
		return
	}
	e.printGutter(line - 1)
	colors.GREY.Fprintln(e.writer, prevLine)
}

// calculateLineNumWidthForDiagnostic calculates the gutter width needed for all lines displayed in this diagnostic
func (e *Emitter) calculateLineNumWidthForDiagnostic(diag *Diagnostic) int {
	lineNumbers := make(map[int]bool)

	for _, label := range diag.Labels {
		if label.Location == nil || label.Location.Start == nil {
			continue
		}

		start := label.Location.Start
		end := label.Location.End
		if end == nil {
			end = start
		}

		for line := start.Line; line <= end.Line; line++ {
			lineNumbers[line] = true
		}

		if start.Line > 1 {
			lineNumbers[start.Line-1] = true
		}
	}

	maxLine := 0
	for line := range lineNumbers {
		if line > maxLine {
			maxLine = line
		}
	}

	if maxLine == 0 {
		return 1
	}
	return len(fmt.Sprintf("%d", maxLine))
}

func (e *Emitter) Emit(diag *Diagnostic) {
	e.currentLineNumWidth = e.calculateLineNumWidthForDiagnostic(diag)

	// Print severity/message first (before file locations)
	e.printDiagnosticHeader(diag)

	if len(diag.Labels) > 0 {
		// Group labels by filepath
		labelsByFile := make(map[string][]Label)
		var files []string

		for _, label := range diag.Labels {
			filepath := label.FilePath
			if filepath == "" {
				filepath = diag.FilePath
			}

			if _, exists := labelsByFile[filepath]; !exists {
				files = append(files, filepath)
			}
			labelsByFile[filepath] = append(labelsByFile[filepath], label)
		}

		// Emit labels grouped by file
		for _, filepath := range files {
			labels := labelsByFile[filepath]

			// Count primary labels for this file
			primaryCount := 0
			var primaryLabel Label
			secondaryLabels := []Label{}

			for _, label := range labels {
				if label.Style == Primary {
					primaryCount++
					primaryLabel = label
				} else {
					secondaryLabels = append(secondaryLabels, label)
				}
			}

			// Print file location header
			e.printFileLocationHeader(filepath, labels)

			if primaryCount == 0 {
				for _, label := range labels {
					e.printLabel(filepath, label, diag.Severity)
				}
			} else if primaryCount > 1 {
				labelStrs := []string{}
				for _, label := range labels {
					labelStrs = append(labelStrs, fmt.Sprintf("%v", label))
				}
				panic("INTERNAL COMPILER ERROR: Multiple primary labels in diagnostic!: " + strings.Join(labelStrs, ", "))
			} else {
				if len(secondaryLabels) == 0 {
					e.printLabel(filepath, primaryLabel, diag.Severity)
				} else if len(secondaryLabels) == 1 &&
					primaryLabel.Location != nil &&
					primaryLabel.Location.Start != nil &&
					secondaryLabels[0].Location != nil &&
					secondaryLabels[0].Location.Start != nil &&
					primaryLabel.Location.Start.Line == secondaryLabels[0].Location.Start.Line {
					e.printCompactDualLabel(filepath, primaryLabel, secondaryLabels[0], diag.Severity)
				} else {
					e.printRoutedLabels(filepath, primaryLabel, secondaryLabels, diag.Severity)
				}
			}
		}
	} else {
		// No labels, print a simple arrow header
		e.printSimpleArrowHeader(diag)
	}

	for _, note := range diag.Notes {
		e.printNote(note)
	}

	if diag.Help != "" {
		e.printHelp(diag.Help)
	}

	fmt.Fprintln(e.writer)
}

// headerPosition picks the best position to show in the header.
func (e *Emitter) headerPosition(diag *Diagnostic) (line, col int) {
	// Prefer primary label start
	for _, l := range diag.Labels {
		if l.Style == Primary && l.Location != nil && l.Location.Start != nil {
			return l.Location.Start.Line, l.Location.Start.Column
		}
	}
	// Otherwise use first label start
	for _, l := range diag.Labels {
		if l.Location != nil && l.Location.Start != nil {
			return l.Location.Start.Line, l.Location.Start.Column
		}
	}
	return 1, 1
}

// printDiagnosticHeader prints the severity and message first (before any file locations)
// Example:
//
//	error[CODE]: message
func (e *Emitter) printDiagnosticHeader(diag *Diagnostic) {
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

// printSimpleArrowHeader prints arrow and file location for diagnostics without labels
func (e *Emitter) printSimpleArrowHeader(diag *Diagnostic) {
	line, col := e.headerPosition(diag)

	colors.BLUE.Fprintf(
		e.writer,
		LINE_POS,
		strings.Repeat(" ", e.currentLineNumWidth),
		diag.FilePath,
		line,
		col,
	)
}

// printFileLocationHeader prints the file location arrow for a group of labels
// Example:
//
//	--> /path/to/file.fer:5:21
//
// or for secondary files:
//
//	--> file.fer:3:1
func (e *Emitter) printFileLocationHeader(filepath string, labels []Label) {
	// Find the primary label in this group for positioning
	var line, col int
	found := false

	for _, label := range labels {
		if label.Style == Primary && label.Location != nil && label.Location.Start != nil {
			line = label.Location.Start.Line
			col = label.Location.Start.Column
			found = true
			break
		}
	}

	// If no primary, use first label
	if !found {
		for _, label := range labels {
			if label.Location != nil && label.Location.Start != nil {
				line = label.Location.Start.Line
				col = label.Location.Start.Column
				break
			}
		}
	}

	// Arrow position line aligned to gutter width
	colors.BLUE.Fprintf(
		e.writer,
		LINE_POS,
		strings.Repeat(" ", e.currentLineNumWidth),
		filepath,
		line,
		col,
	)
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

	ctx := labelContext{
		filepath:     filepath,
		startLine:    start.Line,
		endLine:      end.Line,
		startCol:     start.Column,
		endCol:       end.Column,
		label:        label,
		lineNumWidth: e.currentLineNumWidth,
		severity:     severity,
	}

	if start.Line == end.Line {
		ctx.line = start.Line
		e.printSingleLineLabel(ctx)
	} else {
		e.printMultiLineLabel(ctx)
	}
}

func (e *Emitter) printSingleLineLabel(ctx labelContext) {
	// One blank separator line under header
	e.printPipeOnly()

	// Previous non-empty line in grey (context)
	e.printPrevNonEmptyLine(ctx.filepath, ctx.line)

	sourceLine, err := e.cache.GetLine(ctx.filepath, ctx.line)
	if err != nil {
		// If we can't get the source line, show the label message anyway
		if ctx.label.Message != "" {
			e.printBlankGutter()
			var color colors.COLOR
			if ctx.label.Style == Primary {
				color = e.getSeverityColor(ctx.severity)
			} else {
				color = colors.BLUE
			}
			color.Fprintf(e.writer, "^ %s", ctx.label.Message)
			fmt.Fprintln(e.writer)
		}
		e.printPipeOnly()
		return
	}

	e.printCurrentGutter(ctx.line)
	fmt.Fprintln(e.writer, sourceLine)

	// Underline leader
	e.printBlankGutter()

	padding := ctx.startCol - 1
	length := ctx.endCol - ctx.startCol
	if length <= 0 {
		length = 1
	}

	var underlineColor colors.COLOR
	var underlineChar string

	if ctx.label.Style == Primary {
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
		if length == 1 {
			underlineChar = "^"
		} else {
			underlineChar = "~"
		}
	} else {
		underlineColor = colors.BLUE
		underlineChar = "-"
	}

	fmt.Fprint(e.writer, strings.Repeat(" ", padding))
	underlineColor.Fprint(e.writer, strings.Repeat(underlineChar, length))

	if ctx.label.Message != "" {
		underlineColor.Fprintf(e.writer, " %s", ctx.label.Message)
	}
	fmt.Fprintln(e.writer)

	// Closing separator
	e.printPipeOnly()
}

func (e *Emitter) printMultiLineLabel(ctx labelContext) {
	// One blank separator line under header
	e.printPipeOnly()

	// Previous non-empty line in grey (context)
	e.printPrevNonEmptyLine(ctx.filepath, ctx.startLine)

	startSourceLine, err := e.cache.GetLine(ctx.filepath, ctx.startLine)
	if err != nil {
		return
	}

	e.printCurrentGutter(ctx.startLine)
	fmt.Fprintln(e.writer, startSourceLine)

	// Print underline for start
	e.printBlankGutterWithColor(colors.WHITE)

	var underlineColor colors.COLOR
	if ctx.label.Style == Primary {
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
	if ctx.startCol <= len(startSourceLine) {
		underlineColor.Fprint(
			e.writer,
			strings.Repeat("~", len(startSourceLine)-(ctx.startCol-1)),
		)
	}
	fmt.Fprintln(e.writer)

	// Middle lines
	if ctx.endLine-ctx.startLine > 5 {
		colors.WHITE.Fprintln(e.writer, fmt.Sprintf("%*s...", e.currentLineNumWidth, ""))
	} else {
		for i := ctx.startLine + 1; i < ctx.endLine; i++ {
			line, err := e.cache.GetLine(ctx.filepath, i)
			if err != nil {
				continue
			}
			e.printCurrentGutter(i)
			fmt.Fprintln(e.writer, line)
		}
	}

	// End line
	endSourceLine, err := e.cache.GetLine(ctx.filepath, ctx.endLine)
	if err == nil {
		// End line should be white like other displayed source lines
		e.printCurrentGutter(ctx.endLine)
		fmt.Fprintln(e.writer, endSourceLine)

		e.printBlankGutter()
		endPadding := ctx.endCol - 1
		fmt.Fprint(e.writer, strings.Repeat(" ", endPadding))
		underlineColor.Fprint(e.writer, "^")
		if ctx.label.Message != "" {
			underlineColor.Fprintf(e.writer, " %s", ctx.label.Message)
		}
		fmt.Fprintln(e.writer)
	}

	// Closing separator
	e.printPipeOnly()
}

func (e *Emitter) printNote(note Note) {
	padding := e.currentLineNumWidth + 1
	fmt.Fprint(e.writer, strings.Repeat(" ", padding))
	colors.CYAN.Fprint(e.writer, "= note: ")
	fmt.Fprintln(e.writer, note.Message)
}

func (e *Emitter) printHelp(help string) {
	padding := e.currentLineNumWidth + 1
	fmt.Fprint(e.writer, strings.Repeat(" ", padding))
	colors.GREEN.Fprint(e.writer, "= help: ")
	fmt.Fprintln(e.writer, help)
}

// printCompactDualLabel prints two labels on same line (Rust-style)
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

	var leftLabel, rightLabel Label
	var leftStart, leftEnd, rightStart, rightEnd *source.Position

	if primaryStart.Column < secondaryStart.Column {
		leftLabel = primary
		leftStart, leftEnd = primaryStart, primaryEnd
		rightLabel = secondary
		rightStart, rightEnd = secondaryStart, secondaryEnd
	} else {
		leftLabel = secondary
		leftStart, leftEnd = secondaryStart, secondaryEnd
		rightLabel = primary
		rightStart, rightEnd = primaryStart, primaryEnd
	}

	// Header already printed by caller
	e.printPipeOnly()

	// Previous non-empty line in grey (context)
	e.printPrevNonEmptyLine(filepath, line)

	sourceLine, err := e.cache.GetLine(filepath, line)
	if err != nil {
		return
	}
	e.printCurrentGutter(line)
	fmt.Fprintln(e.writer, sourceLine)

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

	leftColor := colors.BLUE
	rightColor := colors.BLUE
	if leftLabel.Style == Primary {
		leftColor = e.getSeverityColor(severity)
	}
	if rightLabel.Style == Primary {
		rightColor = e.getSeverityColor(severity)
	}

	leftChar := "-"
	if leftLabel.Style == Primary {
		if leftLength == 1 {
			leftChar = "^"
		} else {
			leftChar = "~"
		}
	}

	rightChar := "-"
	if rightLabel.Style == Primary {
		if rightLength == 1 {
			rightChar = "^"
		} else {
			rightChar = "~"
		}
	}

	// Line 1: both underlines, right label inline message
	e.printBlankGutter()
	fmt.Fprint(e.writer, strings.Repeat(" ", leftPadding))
	leftColor.Fprint(e.writer, strings.Repeat(leftChar, leftLength))

	spaceBetween := rightPadding - leftPadding - leftLength
	if spaceBetween < 0 {
		spaceBetween = 1
	}
	fmt.Fprint(e.writer, strings.Repeat(" ", spaceBetween))

	rightColor.Fprint(e.writer, strings.Repeat(rightChar, rightLength))
	if rightLabel.Message != "" {
		rightColor.Fprintf(e.writer, " %s", rightLabel.Message)
	}
	fmt.Fprintln(e.writer)

	// Line 2: vertical connector for left label
	e.printBlankGutter()
	fmt.Fprint(e.writer, strings.Repeat(" ", leftPadding))
	leftColor.Fprintln(e.writer, "|")

	// Line 3: left label message
	e.printBlankGutter()
	fmt.Fprint(e.writer, strings.Repeat(" ", leftPadding))
	leftColor.Fprint(e.writer, "--")
	if leftLabel.Message != "" {
		leftColor.Fprintf(e.writer, " %s", leftLabel.Message)
	}
	fmt.Fprintln(e.writer)

	e.printPipeOnly()
}

// printRoutedLabels prints primary + multiple secondaries with routing (Rust-style)
func (e *Emitter) printRoutedLabels(filepath string, primary Label, secondaries []Label, severity Severity) {
	if primary.Location == nil || primary.Location.Start == nil {
		return
	}

	primaryLine := primary.Location.Start.Line

	lineNumbers := []int{primaryLine}
	for _, sec := range secondaries {
		if sec.Location != nil && sec.Location.Start != nil {
			secLine := sec.Location.Start.Line
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

	for i := 0; i < len(lineNumbers); i++ {
		for j := i + 1; j < len(lineNumbers); j++ {
			if lineNumbers[i] > lineNumbers[j] {
				lineNumbers[i], lineNumbers[j] = lineNumbers[j], lineNumbers[i]
			}
		}
	}

	// Header already printed by caller
	e.printPipeOnly()

	primaryColor := e.getSeverityColor(severity)
	secondaryColor := colors.BLUE

	for idx, lineNum := range lineNumbers {
		if idx > 0 {
			prevLine := lineNumbers[idx-1]
			if lineNum-prevLine > 1 {
				colors.GREY.Fprintln(e.writer, fmt.Sprintf("%*s...", e.currentLineNumWidth, ""))
				e.printPipeOnly()
			}
		}

		// Previous non-empty line in grey (context)
		if lineNum > 1 {
			isPrevShown := idx > 0 && lineNumbers[idx-1] == lineNum-1
			if !isPrevShown {
				e.printPrevNonEmptyLine(filepath, lineNum)
			}
		}

		sourceLine, err := e.cache.GetLine(filepath, lineNum)
		if err != nil {
			continue
		}

		e.printCurrentGutter(lineNum)
		fmt.Fprintln(e.writer, sourceLine)

		hasSecondary := false
		for _, sec := range secondaries {
			if sec.Location != nil && sec.Location.Start != nil && sec.Location.Start.Line == lineNum {
				hasSecondary = true
				break
			}
		}
		hasPrimary := lineNum == primaryLine

		if hasPrimary || hasSecondary {
			e.printBlankGutter()

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
						break
					}
				}
			}
		}
	}

	e.printPipeOnly()
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
