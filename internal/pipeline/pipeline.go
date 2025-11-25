// Package pipeline orchestrates the compilation phases for the Ferret compiler.
//
// Architecture follows industry-standard multi-pass compiler design:
//
// Phase 1: Discovery (Sequential)
//   - Start from entry point
//   - Follow import statements to discover all modules
//   - Build dependency graph
//   - Detect circular dependencies
//   - IMPORTANT: Requires imports to be at top-level and before other declarations
//     This is enforced by the parser to enable deterministic discovery
//
// Phase 2: Lexing + Parsing (Parallel where safe)
//   - Can parallelize files without unresolved imports
//   - Produces AST for each module
//   - No cross-file dependencies in this phase
//
// Phase 3: Symbol Collection (Future)
//   - Build symbol tables for each module
//   - Collect top-level declarations
//
// Phase 4: Resolution (Future)
//   - Resolve import statements
//   - Link symbols across modules
//
// Phase 5: Type Checking (Future)
//   - Validate type correctness
//
// Phase 6: Code Generation (Future)
//   - Emit target code (native, WASM, etc.)
//
// Design considerations:
//   - WASM compatibility: All file I/O abstracted through context
//   - Per-module phase tracking prevents redundant work
//   - Dependency graph enables parallel compilation where safe
//   - Import validation: Like Go, imports MUST be at the top and before declarations
//     This makes discovery deterministic and prevents import order issues
package pipeline

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"sync"

	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
)

// Pipeline coordinates the compilation process
type Pipeline struct {
	ctx   *context_v2.CompilerContext
	debug bool
}

// New creates a new compilation pipeline
func New(ctx *context_v2.CompilerContext, debug bool) *Pipeline {
	return &Pipeline{
		ctx:   ctx,
		debug: debug,
	}
}

// Run executes the full compilation pipeline
func (p *Pipeline) Run() error {
	// Phase 1: Discovery - sequential, follows imports
	if err := p.runDiscoveryPhase(); err != nil {
		return err
	}

	// Phase 2: Lex + Parse - can parallelize known files
	if err := p.runLexParsePhase(); err != nil {
		return err
	}

	// TODO: Future phases
	// Phase 3: Symbol Collection
	// Phase 4: Resolution
	// Phase 5: Type Checking
	// Phase 6: Code Generation

	if p.debug {
		colors.GREEN.Printf("\n✓ Compilation successful! (%d modules)\n", p.ctx.ModuleCount())
	}

	return nil
}

// runDiscoveryPhase discovers all modules by following imports (sequential)
func (p *Pipeline) runDiscoveryPhase() error {
	if p.debug {
		colors.CYAN.Printf("\n[Phase 1] Discovery\n")
	}

	if err := p.discoverModules(); err != nil {
		return fmt.Errorf("discovery phase failed: %w", err)
	}

	if p.ctx.HasErrors() {
		return fmt.Errorf("discovery phase completed with errors")
	}

	if p.debug {
		colors.GREEN.Printf("✓ Discovery completed\n")
	}

	return nil
}

// runLexParsePhase lexes and parses all discovered modules
// For now sequential, but architecture supports parallel processing
func (p *Pipeline) runLexParsePhase() error {
	if p.debug {
		colors.CYAN.Printf("\n[Phase 2] Lex + Parse\n")
	}

	moduleNames := p.ctx.GetModuleNames()

	// Get modules that need lex+parse
	var toParse []*context_v2.Module
	for _, name := range moduleNames {
		module, _ := p.ctx.GetModule(name)
		if module.Phase < context_v2.PhaseParsed {
			toParse = append(toParse, module)
		}
	}

	if len(toParse) == 0 {
		if p.debug {
			colors.BLUE.Printf("  All modules already parsed\n")
		}
		return nil
	}

	// Process in parallel (all files already discovered)
	if err := p.lexParseParallel(toParse); err != nil {
		return fmt.Errorf("lex/parse phase failed: %w", err)
	}

	if p.ctx.HasErrors() {
		return fmt.Errorf("lex/parse phase completed with errors")
	}

	if p.debug {
		colors.GREEN.Printf("✓ Lex + Parse completed\n")
	}

	return nil
}

// lexParseParallel processes multiple modules in parallel
func (p *Pipeline) lexParseParallel(modules []*context_v2.Module) error {
	var wg sync.WaitGroup
	errorChan := make(chan error, len(modules))

	for _, module := range modules {
		wg.Add(1)
		go func(m *context_v2.Module) {
			defer wg.Done()

			if err := p.lexParseModule(m); err != nil {
				errorChan <- err
			}
		}(module)
	}

	wg.Wait()
	close(errorChan)

	// Collect errors
	for err := range errorChan {
		if err != nil {
			p.ctx.ReportError(err.Error(), nil)
		}
	}

	if p.debug {
		colors.BLUE.Printf("  Processed %d module(s)\n", len(modules))
	}

	return nil
}

// lexParseModule lexes and parses a single module
func (p *Pipeline) lexParseModule(module *context_v2.Module) error {
	// Lex
	tokenizer := lexer.New(module.FilePath, module.Content)
	tokens := tokenizer.Tokenize(false)

	// Parse
	diag := diagnostics.NewDiagnosticBag(module.FilePath)
	astModule := parser.Parse(tokens, module.FilePath, diag)

	// Update module
	module.AST = astModule
	module.Phase = context_v2.PhaseParsed

	// Merge diagnostics
	p.mergeDiagnostics(diag)

	return nil
}

// discoverModules discovers all modules by following imports (sequential BFS)
// This phase MUST be sequential because we discover imports on-the-fly
func (p *Pipeline) discoverModules() error {
	queue := []string{p.ctx.EntryModule}
	discovered := make(map[string]bool)

	if p.debug {
		colors.BLUE.Printf("  Entry point: %s\n", p.ctx.EntryModule)
	}

	moduleCount := 0

	for len(queue) > 0 {
		importPath := queue[0]
		queue = queue[1:]

		if discovered[importPath] {
			continue
		}
		discovered[importPath] = true

		// Resolve import path to file path
		filePath, modType, err := p.ctx.ImportPathToFilePath(importPath)
		if err != nil {
			p.ctx.ReportError(
				fmt.Sprintf("cannot resolve module %q: %v", importPath, err),
				nil,
			)
			continue
		}

		// Read source file (for WASM, this could be abstracted)
		content, err := os.ReadFile(filePath)
		if err != nil {
			p.ctx.ReportError(
				fmt.Sprintf("cannot read file %s: %v", filePath, err),
				nil,
			)
			continue
		}

		// Quick scan to extract imports (minimal parsing)
		imports := p.quickScanImports(filePath, string(content))

		// Create module stub (not yet lexed/parsed)
		module := &context_v2.Module{
			FilePath: filePath,
			Type:     modType,
			Phase:    context_v2.PhaseNotStarted,
			Symbols:  context_v2.NewSymbolTable(p.ctx.Universe),
			Content:  string(content),
		}

		// Add to context
		p.ctx.AddModule(importPath, module)
		moduleCount++

		if p.debug {
			colors.PURPLE.Printf("  [%d] %s (%s)\n", moduleCount, importPath, modType)
		}

		// Queue imports and build dependency graph
		for _, imp := range imports {
			// Add dependency (checks for cycles)
			if err := p.ctx.AddDependency(importPath, imp); err != nil {
				p.ctx.ReportError(err.Error(), nil)
				continue
			}

			// Queue for discovery
			if !discovered[imp] {
				queue = append(queue, imp)
			}
		}
	}

	if p.debug {
		colors.BLUE.Printf("  Discovered %d module(s)\n", moduleCount)
	}

	return nil
}

// quickScanImports does a quick scan to extract import paths
// Uses minimal lexing - just looking for 'import "path"' patterns
// This avoids full parsing during discovery phase
func (p *Pipeline) quickScanImports(filePath, content string) []string {
	// For now, do full lex+parse to get imports reliably
	// TODO: Optimize with regex-based scanning if needed
	tokenizer := lexer.New(filePath, content)
	tokens := tokenizer.Tokenize(false)

	diag := diagnostics.NewDiagnosticBag(filePath)
	astModule := parser.Parse(tokens, filePath, diag)

	p.mergeDiagnostics(diag)

	return p.extractImports(astModule)
}

// extractImports extracts import paths from an AST module
func (p *Pipeline) extractImports(astModule *ast.Module) []string {
	var imports []string

	for _, node := range astModule.Nodes {
		if impStmt, ok := node.(*ast.ImportStmt); ok {
			// Import path is a BasicLit (string literal)
			if impStmt.Path != nil {
				importPath := impStmt.Path.Value
				// Remove quotes from string literal
				importPath = strings.Trim(importPath, "\"")
				if importPath != "" {
					imports = append(imports, importPath)
				}
			}
		}
	}

	return imports
}

// mergeDiagnostics merges diagnostics from parser into context
func (p *Pipeline) mergeDiagnostics(diag *diagnostics.DiagnosticBag) {
	// Get all diagnostics from parser
	for _, d := range diag.Diagnostics() {
		p.ctx.Diagnostics.Add(d)
	}
}

// PrintSummary prints a summary of the compilation
func (p *Pipeline) PrintSummary() {
	fmt.Println()
	colors.CYAN.Println("═══════════════════════════════════════")
	colors.CYAN.Println("        COMPILATION SUMMARY")
	colors.CYAN.Println("═══════════════════════════════════════")

	fmt.Printf("Entry Module: %s\n", p.ctx.EntryModule)
	fmt.Printf("Total Modules: %d\n\n", p.ctx.ModuleCount())

	// List all modules
	moduleNames := p.ctx.GetModuleNames()
	sort.Strings(moduleNames)

	for _, name := range moduleNames {
		module, _ := p.ctx.GetModule(name)
		fmt.Printf("  • %s (%s)\n", name, module.Type)
	}

	colors.CYAN.Println("═══════════════════════════════════════")
}

// PrintModuleDetails prints detailed information about a specific module
func (p *Pipeline) PrintModuleDetails(importPath string) {
	module, exists := p.ctx.GetModule(importPath)
	if !exists {
		colors.RED.Printf("Module %q not found\n", importPath)
		return
	}

	colors.CYAN.Printf("\n━━━ Module: %s ━━━\n", importPath)
	fmt.Printf("File Path: %s\n", module.FilePath)
	fmt.Printf("Type: %s\n", module.Type)
	fmt.Printf("Phase: %s\n", module.Phase)
	fmt.Printf("AST Nodes: %d\n", len(module.AST.Nodes))

	fmt.Println()
}
