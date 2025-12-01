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
	"compiler/internal/semantics/collector"
	"compiler/internal/semantics/resolver"
	"compiler/internal/semantics/typechecker"
	"compiler/internal/source"
	"compiler/internal/table"
)

// Pipeline coordinates the compilation process
type Pipeline struct {
	ctx *context_v2.CompilerContext

	// seen ensures each module is scheduled exactly once
	seen sync.Map // map[string]struct{}

	// wg tracks all parsing tasks
	wg sync.WaitGroup
}

// New creates a new compilation pipeline
func New(ctx *context_v2.CompilerContext) *Pipeline {
	return &Pipeline{
		ctx: ctx,
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

	// Compute topological order once after parsing
	p.ctx.ComputeTopologicalOrder()

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

// processModule schedules parsing for a module exactly once (thread-safe)
func (p *Pipeline) processModule(importPath, requestedFrom string, requestedLocation *source.Location) {
	// Fast "do-once" gate. If already scheduled/parsed, return.
	if _, loaded := p.seen.LoadOrStore(importPath, struct{}{}); loaded {
		return
	}

	p.wg.Add(1)

	go func() {
		defer func() {
			p.wg.Done() // mark task done
		}()

		p.parseModule(importPath, requestedFrom, requestedLocation)
	}()
}

// parseModule does the actual parsing work and schedules imports
func (p *Pipeline) parseModule(importPath, requestedFrom string, requestedLocation *source.Location) {
	// Get or create module
	module, exists := p.ctx.GetModule(importPath)
	if !exists {
		module = &context_v2.Module{
			FilePath:   "",
			ImportPath: importPath,
			Type:       context_v2.ModuleLocal,
			Phase:      context_v2.PhaseNotStarted,
			Scope:      table.NewSymbolTable(p.ctx.Universe),
			Content:    "",
			AST:        nil,
		}
		p.ctx.AddModule(importPath, module)
		if p.ctx.Debug {
			fmt.Printf("module '%s' parsed and added", importPath)
		}
	}

	// Resolve import path to file path
	var content string
	var filePath string

	if module.Content != "" {
		// Module already has content (in-memory code)
		content = module.Content
		filePath = module.FilePath
	} else {
		// Resolve and read file
		var err error
		var modType context_v2.ModuleType

		filePath, modType, err = p.ctx.ImportPathToFilePath(importPath)
		if err != nil {
			errorFile := requestedFrom
			if errorFile == "" {
				errorFile = filePath
			}
			p.ctx.Diagnostics.Add(
				diagnostics.NewError(err.Error()).
					WithPrimaryLabel(errorFile, requestedLocation, ""),
			)
			return
		}

		contentBytes, err := os.ReadFile(filePath)
		if err != nil {
			errMsg := fmt.Sprintf("cannot read file %s: %v", filePath, err)
			errorFile := requestedFrom
			if errorFile == "" {
				errorFile = filePath
			}
			p.ctx.Diagnostics.Add(
				diagnostics.NewError(errMsg).
					WithPrimaryLabel(errorFile, requestedLocation, ""),
			)
			return
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
	tokenizer := lexer.New(filePath, content, p.ctx.Diagnostics)
	tokens := tokenizer.Tokenize(false)

	if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseLexed) {
		p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseLexed", importPath), nil)
		return
	}

	// Parse
	astModule := parser.Parse(tokens, filePath, p.ctx.Diagnostics)

	module.Mu.Lock()
	module.AST = astModule
	module.Mu.Unlock()

	if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseParsed) {
		p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseParsed", importPath), nil)
		return
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

	// Schedule imports for processing (they will run in parallel via sem)
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
			break
		}

		if impStmt.Path != nil {
			importPath := strings.Trim(impStmt.Path.Value, "\"")
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

	nodes := 0
	if module.AST != nil {
		nodes = len(module.AST.Nodes)
	}
	fmt.Printf("AST Nodes: %d\n", nodes)

	fmt.Println()
}

// runCollectorPhase runs symbol collection on all parsed modules
func (p *Pipeline) runCollectorPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least parsed
		if p.ctx.GetModulePhase(importPath) < context_v2.PhaseParsed {
			continue
		}

		collector.CollectModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseCollected) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseCollected", importPath), nil)
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	return nil
}

// runResolverPhase runs name resolution on all collected modules
func (p *Pipeline) runResolverPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least collected
		if p.ctx.GetModulePhase(importPath) < context_v2.PhaseCollected {
			continue
		}

		resolver.ResolveModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, context_v2.PhaseResolved) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseResolved", importPath), nil)
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	return nil
}

// runTypeCheckerPhase runs type checking on all resolved modules
func (p *Pipeline) runTypeCheckerPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least resolved
		if p.ctx.GetModulePhase(importPath) < context_v2.PhaseResolved {
			continue
		}

		typechecker.CheckModule(p.ctx, module)

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
