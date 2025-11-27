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
	"compiler/internal/source"
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

	for _, module := range modules {
		wg.Add(1)
		go func(m *context_v2.Module) {
			defer wg.Done()
			p.lexParseModule(m)
		}(module)
	}

	wg.Wait()

	if p.debug {
		colors.BLUE.Printf("  Processed %d module(s)\n", len(modules))
	}

	return nil
}

// lexParseModule lexes and parses a single module
// All errors are reported through diagnostics, not returned
func (p *Pipeline) lexParseModule(module *context_v2.Module) {
	// Lex
	tokenizer := lexer.New(module.FilePath, module.Content)
	tokens := tokenizer.Tokenize(false)

	// Parse
	diag := diagnostics.NewDiagnosticBag(module.FilePath)
	astModule := parser.Parse(tokens, module.FilePath, diag)

	// Update module
	module.AST = astModule
	module.Phase = context_v2.PhaseParsed

	// Merge diagnostics (thread-safe)
	p.mergeDiagnostics(diag)
}

// discoverModules discovers all modules by following imports (sequential BFS)
// This phase MUST be sequential because we discover imports on-the-fly
func (p *Pipeline) discoverModules() error {
	type queueItem struct {
		importPath string
		// Track where this import was requested (for error reporting)
		requestedFrom     string // file path
		requestedLocation *source.Location
	}

	queue := []queueItem{{importPath: p.ctx.EntryModule}}
	discovered := make(map[string]bool)

	if p.debug {
		colors.BLUE.Printf("  Entry point: %s\n", p.ctx.EntryModule)
	}

	moduleCount := 0

	for len(queue) > 0 {
		item := queue[0]
		queue = queue[1:]

		if discovered[item.importPath] {
			continue
		}
		discovered[item.importPath] = true

		// Check if module already exists (e.g., entry module with code)
		existingModule, exists := p.ctx.GetModule(item.importPath)
		var content string
		var filePath string
		var modType context_v2.ModuleType

		if exists && existingModule.Content != "" {
			// Module already loaded (in-memory code)
			content = existingModule.Content
			filePath = existingModule.FilePath
			modType = existingModule.Type
		} else {
			// Resolve import path to file path
			var err error
			filePath, modType, err = p.ctx.ImportPathToFilePath(item.importPath)
			if err != nil {
				// Report error with the location where the import was requested
				p.ctx.Diagnostics.Add(
					diagnostics.NewError(err.Error()).
						WithPrimaryLabel(item.requestedFrom, item.requestedLocation, ""),
				)
				continue
			}

			// Read source file
			contentBytes, err := os.ReadFile(filePath)
			if err != nil {
				p.ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("cannot read file %s: %v", filePath, err)).
						WithPrimaryLabel(item.requestedFrom, item.requestedLocation, ""),
				)
				continue
			}
			content = string(contentBytes)
		}

		// Quick scan to extract imports and parse AST (done once during discovery)
		imports, astModule, _ := p.quickScanImports(filePath, content, item.importPath)

		// Add source content to diagnostics cache for error reporting
		p.ctx.Diagnostics.AddSourceContent(filePath, content)

		// Create or update module with parsed AST
		if !exists {
			module := &context_v2.Module{
				FilePath: filePath,
				Type:     modType,
				Phase:    context_v2.PhaseNotStarted,
				Symbols:  context_v2.NewSymbolTable(p.ctx.Universe),
				Content:  content,
				AST:      astModule, // Store AST to avoid re-parsing in Phase 2
			}
			// Properly advance through phases to maintain invariants
			module.Phase = context_v2.PhaseLexed  // Tokens were generated
			module.Phase = context_v2.PhaseParsed // AST was built
			p.ctx.AddModule(item.importPath, module)
			moduleCount++
		} else if existingModule.AST == nil {
			// Update existing module with AST if it didn't have one
			existingModule.AST = astModule
			existingModule.Phase = context_v2.PhaseLexed
			existingModule.Phase = context_v2.PhaseParsed
		}

		if p.debug {
			colors.PURPLE.Printf("  [%d] %s (%s)\n", moduleCount, item.importPath, modType)
		}

		// Queue imports and build dependency graph
		for _, imp := range imports {
			// Add dependency (checks for cycles)
			if err := p.ctx.AddDependency(item.importPath, imp.path); err != nil {
				p.ctx.ReportError(err.Error(), imp.location)
				continue
			}

			// Queue for discovery
			if !discovered[imp.path] {
				queue = append(queue, queueItem{
					importPath:        imp.path,
					requestedFrom:     filePath,
					requestedLocation: imp.location,
				})
			}
		}
	}

	if p.debug {
		colors.BLUE.Printf("  Discovered %d module(s)\n", moduleCount)
	}

	return nil
}

// importInfo holds import path and its source location
type importInfo struct {
	path     string
	location *source.Location
}

// quickScanImports does a quick scan to extract import paths with their locations
// Does full lex+parse during discovery, then stores the AST so Phase 2 can skip it
// This ensures each file is parsed exactly once
//
// TODO: Discovery currently advances Phase directly to Parsed (via Lexed).
// When phase validation is enforced, ensure CanProcessPhase checks are satisfied.
func (p *Pipeline) quickScanImports(filePath, content string, importPath string) ([]importInfo, *ast.Module, *diagnostics.DiagnosticBag) {
	tokenizer := lexer.New(filePath, content)
	tokens := tokenizer.Tokenize(false)

	diag := diagnostics.NewDiagnosticBag(filePath)
	astModule := parser.Parse(tokens, filePath, diag)

	p.mergeDiagnostics(diag)

	imports := p.extractImports(astModule)
	return imports, astModule, diag
}

// extractImports extracts import paths with locations from an AST module
func (p *Pipeline) extractImports(astModule *ast.Module) []importInfo {
	var imports []importInfo

	for _, node := range astModule.Nodes {
		if impStmt, ok := node.(*ast.ImportStmt); ok {
			// Import path is a BasicLit (string literal)
			if impStmt.Path != nil {
				importPath := impStmt.Path.Value
				// Remove quotes from string literal
				importPath = strings.Trim(importPath, "\"")
				// Normalize import path semantically (handles whitespace, multiple slashes, etc.)
				importPath = p.ctx.NormalizeImportPath(importPath)
				if importPath != "" {
					imports = append(imports, importInfo{
						path:     importPath,
						location: &impStmt.Location,
					})
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

	// Nil-safe AST node count
	nodes := 0
	if module.AST != nil {
		nodes = len(module.AST.Nodes)
	}
	fmt.Printf("AST Nodes: %d\n", nodes)

	fmt.Println()
}
