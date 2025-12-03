package diagnostics

import (
	"compiler/internal/source"
	"sync"
	"testing"
)

func TestNewDiagnosticBag(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	if bag == nil {
		t.Fatal("NewDiagnosticBag returned nil")
	}

	if bag.ErrorCount() != 0 {
		t.Errorf("Expected 0 errors, got %d", bag.ErrorCount())
	}

	if bag.WarningCount() != 0 {
		t.Errorf("Expected 0 warnings, got %d", bag.WarningCount())
	}

	if bag.HasErrors() {
		t.Error("Expected HasErrors() to be false for empty bag")
	}
}

func TestDiagnosticBag_AddError(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	diag := NewError("test error")
	bag.Add(diag)

	if !bag.HasErrors() {
		t.Error("Expected HasErrors() to be true after adding error")
	}

	if bag.ErrorCount() != 1 {
		t.Errorf("Expected 1 error, got %d", bag.ErrorCount())
	}

	if bag.WarningCount() != 0 {
		t.Errorf("Expected 0 warnings, got %d", bag.WarningCount())
	}
}

func TestDiagnosticBag_AddWarning(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	diag := NewWarning("test warning")
	bag.Add(diag)

	if bag.HasErrors() {
		t.Error("Expected HasErrors() to be false when only warnings present")
	}

	if bag.ErrorCount() != 0 {
		t.Errorf("Expected 0 errors, got %d", bag.ErrorCount())
	}

	if bag.WarningCount() != 1 {
		t.Errorf("Expected 1 warning, got %d", bag.WarningCount())
	}
}

func TestDiagnosticBag_MultipleDiagnostics(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	// Add multiple errors and warnings
	bag.Add(NewError("error 1"))
	bag.Add(NewWarning("warning 1"))
	bag.Add(NewError("error 2"))
	bag.Add(NewWarning("warning 2"))
	bag.Add(NewError("error 3"))

	if bag.ErrorCount() != 3 {
		t.Errorf("Expected 3 errors, got %d", bag.ErrorCount())
	}

	if bag.WarningCount() != 2 {
		t.Errorf("Expected 2 warnings, got %d", bag.WarningCount())
	}

	if !bag.HasErrors() {
		t.Error("Expected HasErrors() to be true")
	}

	diagnostics := bag.Diagnostics()
	if len(diagnostics) != 5 {
		t.Errorf("Expected 5 diagnostics, got %d", len(diagnostics))
	}
}

func TestDiagnosticBag_DiagnosticsCopy(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	bag.Add(NewError("error 1"))
	bag.Add(NewWarning("warning 1"))

	// Get copy of diagnostics
	diags1 := bag.Diagnostics()

	// Add more diagnostics
	bag.Add(NewError("error 2"))

	// Get another copy
	diags2 := bag.Diagnostics()

	// First copy should not be affected
	if len(diags1) != 2 {
		t.Errorf("Expected first copy to have 2 diagnostics, got %d", len(diags1))
	}

	if len(diags2) != 3 {
		t.Errorf("Expected second copy to have 3 diagnostics, got %d", len(diags2))
	}
}

func TestDiagnosticBag_ThreadSafety(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	var wg sync.WaitGroup
	numGoroutines := 10
	diagnosticsPerGoroutine := 10

	// Spawn multiple goroutines adding diagnostics
	for i := 0; i < numGoroutines; i++ {
		wg.Add(1)
		go func(id int) {
			defer wg.Done()
			for j := 0; j < diagnosticsPerGoroutine; j++ {
				if j%2 == 0 {
					bag.Add(NewError("concurrent error"))
				} else {
					bag.Add(NewWarning("concurrent warning"))
				}
			}
		}(i)
	}

	wg.Wait()

	expectedTotal := numGoroutines * diagnosticsPerGoroutine
	expectedErrors := numGoroutines * (diagnosticsPerGoroutine / 2)
	expectedWarnings := numGoroutines * (diagnosticsPerGoroutine / 2)

	if bag.ErrorCount() != expectedErrors {
		t.Errorf("Expected %d errors, got %d", expectedErrors, bag.ErrorCount())
	}

	if bag.WarningCount() != expectedWarnings {
		t.Errorf("Expected %d warnings, got %d", expectedWarnings, bag.WarningCount())
	}

	diagnostics := bag.Diagnostics()
	if len(diagnostics) != expectedTotal {
		t.Errorf("Expected %d total diagnostics, got %d", expectedTotal, len(diagnostics))
	}
}

func TestDiagnosticBag_AddSourceContent(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	content := "let x := 42;\nlet y := 100;"
	bag.AddSourceContent("test.fer", content)

	// Source cache should have the content
	line, _ := bag.sourceCache.GetLine("test.fer", 1)
	if line != "let x := 42;" {
		t.Errorf("Expected first line to be 'let x := 42;', got %q", line)
	}

	line, _ = bag.sourceCache.GetLine("test.fer", 2)
	if line != "let y := 100;" {
		t.Errorf("Expected second line to be 'let y := 100;', got %q", line)
	}
}

func TestDiagnosticBag_WithLocations(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	content := "let x := 42;"
	bag.AddSourceContent("test.fer", content)

	loc := source.NewLocation(
		nil,
		&source.Position{Line: 1, Column: 5},
		&source.Position{Line: 1, Column: 6},
	)

	diag := NewError("undefined variable")
	diag.Labels = append(diag.Labels, Label{
		Location: loc,
		Message:  "variable 'x' not defined",
		Style:    Primary,
	})

	bag.Add(diag)

	if !bag.HasErrors() {
		t.Error("Expected HasErrors() to be true")
	}

	diagnostics := bag.Diagnostics()
	if len(diagnostics) != 1 {
		t.Fatalf("Expected 1 diagnostic, got %d", len(diagnostics))
	}

	if len(diagnostics[0].Labels) != 1 {
		t.Errorf("Expected 1 label, got %d", len(diagnostics[0].Labels))
	}

	if diagnostics[0].Labels[0].Location.Start.Line != 1 {
		t.Errorf("Expected label at line 1, got line %d", diagnostics[0].Labels[0].Location.Start.Line)
	}
}

func TestDiagnosticBag_EmptyBag(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	// Test empty bag behavior
	if bag.HasErrors() {
		t.Error("Empty bag should not have errors")
	}

	if bag.ErrorCount() != 0 {
		t.Errorf("Empty bag should have 0 errors, got %d", bag.ErrorCount())
	}

	if bag.WarningCount() != 0 {
		t.Errorf("Empty bag should have 0 warnings, got %d", bag.WarningCount())
	}

	diagnostics := bag.Diagnostics()
	if len(diagnostics) != 0 {
		t.Errorf("Empty bag should return empty slice, got %d diagnostics", len(diagnostics))
	}
}

func TestDiagnosticBag_InfoAndHint(t *testing.T) {
	bag := NewDiagnosticBag("test.fer")

	infoDiag := &Diagnostic{
		Severity: Info,
		Message:  "informational message",
	}

	hintDiag := &Diagnostic{
		Severity: Hint,
		Message:  "hint message",
	}

	bag.Add(infoDiag)
	bag.Add(hintDiag)

	// Info and hints should not count as errors or warnings
	if bag.HasErrors() {
		t.Error("Info and hint diagnostics should not be counted as errors")
	}

	if bag.ErrorCount() != 0 {
		t.Errorf("Expected 0 errors, got %d", bag.ErrorCount())
	}

	if bag.WarningCount() != 0 {
		t.Errorf("Expected 0 warnings, got %d", bag.WarningCount())
	}

	// But they should be in the diagnostics list
	diagnostics := bag.Diagnostics()
	if len(diagnostics) != 2 {
		t.Errorf("Expected 2 diagnostics, got %d", len(diagnostics))
	}
}
