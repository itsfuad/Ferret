package pipeline

import (
	"fmt"
	"os"
	"runtime"
	"sort"
	"strings"
	"sync"

	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
	"compiler/internal/semantics/collector"
	"compiler/internal/semantics/resolver"
	"compiler/internal/semantics/typechecker"
	"compiler/internal/source"
)

// moduleState tracks the 3 states: Unseen -> Parsing -> Done
type moduleState int

const (
	stateUnseen moduleState = iota
	stateParsing
	stateDone
)

// Pipeline coordinates the compilation process
type Pipeline struct {
	ctx    *context_v2.CompilerContext
	mu     sync.Mutex             // protects states map
	states map[string]moduleState // tracks module states: Unseen -> Parsing -> Done
	wg     sync.WaitGroup         // tracks all parsing tasks
	sem    chan struct{}          // semaphore for bounded parallelism
}

// New creates a new compilation pipeline
func New(ctx *context_v2.CompilerContext) *Pipeline {
	// Bound parallelism to number of CPU cores
	numWorkers := runtime.NumCPU()
	return &Pipeline{
		ctx:    ctx,
		states: make(map[string]moduleState),
		sem:    make(chan struct{}, numWorkers),
	}
}

// Run executes the full compilation pipeline
func (p *Pipeline) Run() error {
	if p.ctx.Debug {
		colors.CYAN.Printf("\n[Phase 1] Lex + Parse\n")
	}

	// Start with entry module and recursively process imports
	p.processModule(p.ctx.EntryModule, "", nil)

	// Wait for all parsing tasks to complete
	p.wg.Wait()

	if p.ctx.HasErrors() {
		return fmt.Errorf("compilation failed with errors")
	}

	// Phase 2: Symbol Collection
	if p.ctx.Debug {
		colors.CYAN.Printf("\n[Phase 2] Symbol Collection\n")
	}
	if err := p.runCollectorPhase(); err != nil {
		return err
	}

	// Phase 3: Resolution
	if p.ctx.Debug {
		colors.CYAN.Printf("\n[Phase 3] Resolution\n")
	}
	if err := p.runResolverPhase(); err != nil {
		return err
	}

	// Phase 4: Type Checking
	if p.ctx.Debug {
		colors.CYAN.Printf("\n[Phase 4] Type Checking\n")
	}
	if err := p.runTypeCheckerPhase(); err != nil {
		return err
	}

	// Phase 5: Code Generation (future)

	if p.ctx.Debug {
		colors.GREEN.Printf("\n✓ Compilation successful! (%d modules)\n", p.ctx.ModuleCount())
	}

	return nil
}

// processModule processes a single module: lex, parse, and recursively handle imports
func (p *Pipeline) processModule(importPath, requestedFrom string, requestedLocation *source.Location) {
	// Check module state (3-state: Unseen -> Parsing -> Done)
	p.mu.Lock()
	state := p.states[importPath]
	if state == stateParsing || state == stateDone {
		p.mu.Unlock()
		return // Already being processed or done
	}

	// Mark as Parsing
	p.states[importPath] = stateParsing
	p.mu.Unlock()

	// Register this task
	p.wg.Add(1)

	// Acquire semaphore (bounded parallelism)
	p.sem <- struct{}{}

	go func() {
		defer func() {
			<-p.sem     // Release semaphore
			p.wg.Done() // Mark task done

			// Mark module as Done
			p.mu.Lock()
			p.states[importPath] = stateDone
			p.mu.Unlock()
		}()

		p.parseModule(importPath, requestedFrom, requestedLocation)
	}()
}

// parseModule does the actual parsing work
func (p *Pipeline) parseModule(importPath, requestedFrom string, requestedLocation *source.Location) {
	// Get or create module
	module, exists := p.ctx.GetModule(importPath)
	if !exists {
		module = &context_v2.Module{
			FilePath:   "",
			ImportPath: importPath,
			Type:       context_v2.ModuleLocal,
			Phase:      context_v2.PhaseNotStarted,
			Symbols:    context_v2.NewSymbolTable(p.ctx.Universe),
			Content:    "",
			AST:        nil,
		}
		p.ctx.AddModule(importPath, module)
	}

	// Resolve import path to file path
	var content string
	var filePath string
	var modType context_v2.ModuleType

	if module.Content != "" {
		// Module already has content (in-memory code)
		content = module.Content
		filePath = module.FilePath
		modType = module.Type
	} else {
		// Resolve and read file
		var err error
		filePath, modType, err = p.ctx.ImportPathToFilePath(importPath)
		if err != nil {
			// Use current filePath for entry module errors (when requestedFrom is empty)
			errorFile := requestedFrom
			if errorFile == "" {
				errorFile = filePath
			}
			p.ctx.Diagnostics.Add(
				diagnostics.NewError(err.Error()).
					WithPrimaryLabel(errorFile, requestedLocation, ""),
			)
			return // Fail but mark as done
		}

		contentBytes, err := os.ReadFile(filePath)
		if err != nil {
			errMsg := fmt.Sprintf("cannot read file %s: %v", filePath, err)
			// Use current filePath for entry module errors (when requestedFrom is empty)
			errorFile := requestedFrom
			if errorFile == "" {
				errorFile = filePath
			}
			p.ctx.Diagnostics.Add(
				diagnostics.NewError(errMsg).
					WithPrimaryLabel(errorFile, requestedLocation, ""),
			)
			return // Fail but mark as done
		}
		content = string(contentBytes)

		// Update module info with lock
		module.Mu.Lock()
		module.FilePath = filePath
		module.Type = modType
		module.Content = content
		module.Mu.Unlock()
	}

	// Add source content for diagnostics
	p.ctx.Diagnostics.AddSourceContent(filePath, content)

	// Lex
	tokenizer := lexer.New(filePath, content)
	tokens := tokenizer.Tokenize(false)

	// Report lexer errors as diagnostics
	for _, lexErr := range tokenizer.Errors {
		p.ctx.Diagnostics.Add(
			diagnostics.NewError(lexErr.Error()),
		)
	}

	if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseLexed) {
		p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseLexed", importPath), nil)
		return // Fail but mark as done
	}

	// Parse
	diag := diagnostics.NewDiagnosticBag(filePath)
	astModule := parser.Parse(tokens, filePath, diag)
	module.Mu.Lock()
	module.AST = astModule
	module.Mu.Unlock()

	// Merge diagnostics
	for _, d := range diag.Diagnostics() {
		p.ctx.Diagnostics.Add(d)
	}

	if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseParsed) {
		p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseParsed", importPath), nil)
		return // Fail but mark as done
	}

	if p.ctx.Debug {
		colors.PURPLE.Printf("  ✓ %s\n", importPath)
	}

	// Extract top-level imports (stop at first non-import statement)
	imports := p.extractTopLevelImports(astModule)

	// Add dependencies and check for cycles
	for _, imp := range imports {
		if err := p.ctx.AddDependency(importPath, imp.path); err != nil {
			p.ctx.ReportError(err.Error(), imp.location)
			continue
		}
	}

	// Schedule imports for processing (they will run in parallel via worker pool)
	for _, imp := range imports {
		p.processModule(imp.path, filePath, imp.location)
	}
}

// importInfo holds import path and its source location
type importInfo struct {
	path     string
	location *source.Location
}

// extractTopLevelImports extracts only top-level imports (stops at first non-import)
func (p *Pipeline) extractTopLevelImports(astModule *ast.Module) []importInfo {
	var imports []importInfo

	for _, node := range astModule.Nodes {
		impStmt, ok := node.(*ast.ImportStmt)
		if !ok {
			// First non-import statement, stop scanning
			break
		}

		if impStmt.Path != nil {
			importPath := impStmt.Path.Value
			// Remove quotes from string literal
			importPath = strings.Trim(importPath, "\"")
			// Normalize import path
			importPath = p.ctx.NormalizeImportPath(importPath)
			if importPath != "" {
				imports = append(imports, importInfo{
					path:     importPath,
					location: &impStmt.Location,
				})
			}
		}
	}

	return imports
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

// runCollectorPhase runs symbol collection on all parsed modules
func (p *Pipeline) runCollectorPhase() error {
	moduleNames := p.ctx.GetModuleNames()

	for _, importPath := range moduleNames {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least parsed
		if p.ctx.GetModulePhase(importPath) < context_v2.PhaseParsed {
			continue
		}

		// Run collector
		collector.CollectModule(p.ctx, module)

		// Advance to Collected phase
		if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseCollected) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseCollected", importPath), nil)
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	// Don't stop on collection errors - continue to type checking to find more errors
	// if p.ctx.HasErrors() {
	// 	return fmt.Errorf("symbol collection failed with errors")
	// }

	return nil
}

// runResolverPhase runs name resolution on all collected modules
func (p *Pipeline) runResolverPhase() error {
	moduleNames := p.ctx.GetModuleNames()

	for _, importPath := range moduleNames {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least collected
		if p.ctx.GetModulePhase(importPath) < context_v2.PhaseCollected {
			continue
		}

		// Run resolver
		resolver.ResolveModule(p.ctx, module)

		// Advance to Resolved phase
		if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseResolved) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseResolved", importPath), nil)
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	// Don't stop on resolution errors - continue to type checking to find more errors
	// if p.ctx.HasErrors() {
	// 	return fmt.Errorf("resolution failed with errors")
	// }

	return nil
}

// runTypeCheckerPhase runs type checking on all resolved modules
func (p *Pipeline) runTypeCheckerPhase() error {
	moduleNames := p.ctx.GetModuleNames()

	for _, importPath := range moduleNames {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least resolved
		if p.ctx.GetModulePhase(importPath) < context_v2.PhaseResolved {
			continue
		}

		// Run type checker
		typechecker.CheckModule(p.ctx, module)

		// Advance to TypeChecked phase
		if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseTypeChecked) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseTypeChecked", importPath), nil)
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	if p.ctx.HasErrors() {
		return fmt.Errorf("type checking failed with errors")
	}

	return nil
}
