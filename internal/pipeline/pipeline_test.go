package pipeline

import (
	"os"
	"path/filepath"
	"testing"

	"compiler/internal/context_v2"
)

func TestPipelineBasic(t *testing.T) {
	// Create temporary test directory
	tmpDir := t.TempDir()

	// Create a simple test file
	mainContent := `let x := 42;`
	mainPath := filepath.Join(tmpDir, "main.fer")
	if err := os.WriteFile(mainPath, []byte(mainContent), 0644); err != nil {
		t.Fatalf("Failed to create test file: %v", err)
	}

	// Create config
	config := &context_v2.Config{
		ProjectName: "test",
		ProjectRoot: tmpDir,
		Extension:   ".fer",
	}

	// Create context
	ctx := context_v2.New(config, false)
	if err := ctx.SetEntryPoint(mainPath); err != nil {
		t.Fatalf("Failed to set entry point: %v", err)
	}

	// Create and run pipeline
	p := New(ctx, false)
	if err := p.Run(); err != nil {
		t.Errorf("Pipeline failed: %v", err)
	}

	// Verify module was added
	if ctx.ModuleCount() != 1 {
		t.Errorf("Expected 1 module, got %d", ctx.ModuleCount())
	}

	// Verify module is parsed
	phase := ctx.GetModulePhase(ctx.EntryModule)
	if phase != context_v2.PhaseParsed {
		t.Errorf("Expected PhaseParsed, got %v", phase)
	}
}
