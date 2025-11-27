package diagnostics

import (
	"bytes"
	"compiler/colors"
	"fmt"
	"io"
	"os"
	"sync"
)

const (
	compileFailedMsg          = "\nCompilation failed with %d error(s)"
	andWarningMsg             = " and %d warning(s)"
	compileSuccessWithWarning = "\nCompilation succeeded with %d warning(s)\n"
)

// DiagnosticBag collects diagnostics during compilation
type DiagnosticBag struct {
	diagnostics []*Diagnostic
	mu          sync.Mutex
	errorCount  int
	warnCount   int
	sourceCache *SourceCache
}

// NewDiagnosticBag creates a new diagnostic bag for a file
func NewDiagnosticBag(filepath string) *DiagnosticBag {
	return &DiagnosticBag{
		diagnostics: make([]*Diagnostic, 0),
		sourceCache: NewSourceCache(),
	}
}

// AddSourceContent adds source content for a file path (for in-memory compilation)
func (db *DiagnosticBag) AddSourceContent(filepath, content string) {
	db.sourceCache.AddSource(filepath, content)
}

// Add adds a diagnostic to the bag
func (db *DiagnosticBag) Add(diag *Diagnostic) {
	db.mu.Lock()
	defer db.mu.Unlock()

	db.diagnostics = append(db.diagnostics, diag)

	switch diag.Severity {
	case Error:
		db.errorCount++
	case Warning:
		db.warnCount++
	}
}

// HasErrors returns true if there are any errors
func (db *DiagnosticBag) HasErrors() bool {
	db.mu.Lock()
	defer db.mu.Unlock()
	return db.errorCount > 0
}

// ErrorCount returns the number of errors
func (db *DiagnosticBag) ErrorCount() int {
	db.mu.Lock()
	defer db.mu.Unlock()
	return db.errorCount
}

// WarningCount returns the number of warnings
func (db *DiagnosticBag) WarningCount() int {
	db.mu.Lock()
	defer db.mu.Unlock()
	return db.warnCount
}

// Diagnostics returns a copy of all diagnostics (thread-safe)
func (db *DiagnosticBag) Diagnostics() []*Diagnostic {
	db.mu.Lock()
	defer db.mu.Unlock()
	// Return a copy to prevent races if caller iterates while other goroutines append
	result := make([]*Diagnostic, len(db.diagnostics))
	copy(result, db.diagnostics)
	return result
}

func (db *DiagnosticBag) EmitAll() {

	emitter := NewEmitter(os.Stderr)

	db.mu.Lock()

	diagnostics := make([]*Diagnostic, len(db.diagnostics))

	// copy diagnostics to avoid holding lock during emit
	copy(diagnostics, db.diagnostics)

	db.mu.Unlock()
	for _, diag := range diagnostics {
		emitter.Emit(diag)
	}

	db.printSummary(os.Stderr)
}

// EmitAllToString emits all diagnostics to a string with ANSI codes, using provided source cache
func (db *DiagnosticBag) EmitAllToString() string {

	var buf bytes.Buffer
	emitter := &Emitter{
		cache:  db.sourceCache,
		writer: &buf,
	}

	db.mu.Lock()
	diagnostics := make([]*Diagnostic, len(db.diagnostics))

	copy(diagnostics, db.diagnostics)

	db.mu.Unlock()

	for _, diag := range diagnostics {
		emitter.Emit(diag)
	}

	db.printSummary(&buf)

	return buf.String()
}

// EmitAllToHTML emits all diagnostics to an HTML string, using provided source cache
func (db *DiagnosticBag) EmitAllToHTML() string {
	ansiOutput := db.EmitAllToString()
	return colors.ConvertANSIToHTML(ansiOutput)
}

func (db *DiagnosticBag) printSummary(w io.Writer) {
	db.mu.Lock()
	defer db.mu.Unlock()

	if db.errorCount > 0 {
		colors.RED.Fprintf(w, compileFailedMsg, db.errorCount)
		if db.warnCount > 0 {
			colors.RED.Fprintf(w, andWarningMsg, db.warnCount)
		}
		fmt.Fprintln(w)
	} else if db.warnCount > 0 {
		colors.ORANGE.Fprintf(w, compileSuccessWithWarning, db.warnCount)
	}
}

// Clear removes all diagnostics
func (db *DiagnosticBag) Clear() {
	db.mu.Lock()
	defer db.mu.Unlock()
	db.diagnostics = make([]*Diagnostic, 0)
	db.errorCount = 0
	db.warnCount = 0
}
