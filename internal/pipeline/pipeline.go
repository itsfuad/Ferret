package pipeline

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"sync"

	"compiler/colors"
	"compiler/internal/codegen"
	"compiler/internal/codegen/cgen"
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/frontend/parser"
	"compiler/internal/phase"
	"compiler/internal/semantics/cfganalyzer"
	"compiler/internal/semantics/collector"
	"compiler/internal/semantics/resolver"
	"compiler/internal/semantics/table"
	"compiler/internal/semantics/typechecker"
	"compiler/internal/source"
	"compiler/internal/utils/fs"
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

		colors.PrintAllColors()

		colors.CYAN.Printf("\n[Phase 1] Lex + Parse\n")
	}

	// Start with entry module and recursively process imports
	p.processModule(p.ctx.EntryModule, nil)

	// Wait for all parsing tasks to complete
	p.wg.Wait()

	// Compute topological order once after parsing
	// Do this even if there are errors, so tests can inspect what was discovered
	p.ctx.ComputeTopologicalOrder()

	// Continue to later phases even if there are parse errors
	// This allows us to collect more diagnostics from successfully parsed modules

	// Phase 2: Symbol Collection
	// The collector handles all sub-phases internally:
	//   - Collect declarations (types, functions, methods, variables, constants)
	//   - Collect function bodies
	//   - Collect method bodies
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

	// Phase 4a: Type Check Method Signatures (attach methods to types)
	if p.ctx.Debug {
		colors.CYAN.Printf("\n[Phase 4a] Type Check Method Signatures\n")
	}
	if err := p.runMethodSignatureTypeCheckPhase(); err != nil {
		return err
	}

	// Phase 4b: Type Check Everything Else
	if p.ctx.Debug {
		colors.CYAN.Printf("\n[Phase 4b] Type Check Bodies\n")
	}
	if err := p.runTypeCheckerPhase(); err != nil {
		return err
	}

	// Phase 5: Control Flow Analysis
	if p.ctx.Debug {
		colors.CYAN.Printf("\n[Phase 5] Control Flow Analysis\n")
	}
	if err := p.runCFGAnalysisPhase(); err != nil {
		return err
	}

	// Phase 6: Code Generation
	if p.ctx.Debug {
		colors.CYAN.Printf("\n[Phase 6] Code Generation\n")
	}
	if err := p.runCodegenPhase(); err != nil {
		return err
	}

	// Final error check
	if p.ctx.HasErrors() {
		return fmt.Errorf("compilation failed with errors")
	}

	if p.ctx.Debug {
		colors.GREEN.Printf("\n✓ Compilation successful! (%d modules)\n", p.ctx.ModuleCount())
	}

	return nil
}

// processModule schedules parsing for a module exactly once (thread-safe)
func (p *Pipeline) processModule(importPath string, requestedLocation *source.Location) {
	// Fast "do-once" gate. If already scheduled/parsed, return.
	if _, loaded := p.seen.LoadOrStore(importPath, struct{}{}); loaded {
		return
	}

	p.wg.Add(1)

	go func() {
		defer func() {
			p.wg.Done() // mark task done
		}()

		p.parseModule(importPath, requestedLocation)
	}()
}

// parseModule does the actual parsing work and schedules imports
func (p *Pipeline) parseModule(importPath string, requestedLocation *source.Location) {
	// Get or create module
	module, exists := p.ctx.GetModule(importPath)
	if !exists {
		modScope := table.NewSymbolTable(p.ctx.Universe)
		module = &context_v2.Module{
			FilePath:     "",
			ImportPath:   importPath,
			Type:         context_v2.ModuleLocal,
			Phase:        phase.PhaseNotStarted,
			ModuleScope:  modScope,
			CurrentScope: modScope,
			Content:      "",
			AST:          nil,
		}
		p.ctx.AddModule(importPath, module)
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
			// Check if it's a native module (already registered)
			if p.ctx.HasModule(importPath) {
				mod, _ := p.ctx.GetModule(importPath)
				if mod.Type == context_v2.ModuleBuiltin && mod.AST == nil {
					// Native module - skip file reading, it's already registered
					// Mark it as parsed (native modules don't need parsing)
					if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseParsed) {
						p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseParsed", importPath), nil)
					}
					return
				}
			}
			p.ctx.Diagnostics.Add(
				diagnostics.NewError(err.Error()).
					WithPrimaryLabel(requestedLocation, ""),
			)
			return
		}

		// Skip file reading for native modules
		if strings.HasPrefix(filePath, "native://") {
			// Native module - already registered, create empty AST and mark as parsed
			module.Mu.Lock()
			if module.AST == nil {
				// Create minimal AST for native module
				module.AST = &ast.Module{
					Nodes: []ast.Node{},
				}
			}
			module.Mu.Unlock()

			// Mark as lexed and parsed
			p.ctx.SetModulePhase(importPath, phase.PhaseLexed)
			if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseParsed) {
				// If advance fails, try direct set (native modules may not follow normal flow)
				p.ctx.SetModulePhase(importPath, phase.PhaseParsed)
			}
			return
		}

		contentBytes, err := os.ReadFile(filePath)
		if err != nil {
			errMsg := fmt.Sprintf("cannot read file %s: %v", filePath, err)
			p.ctx.Diagnostics.Add(
				diagnostics.NewError(errMsg).
					WithPrimaryLabel(requestedLocation, ""),
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

	if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseLexed) {
		p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseLexed", importPath), nil)
		return
	}

	// Parse
	astModule := parser.Parse(tokens, filePath, p.ctx.Diagnostics)

	module.Mu.Lock()
	module.AST = astModule
	module.Mu.Unlock()

	if astModule != nil {
		astModule.SaveAST()
	}

	if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseParsed) {
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
		p.processModule(imp.path, imp.location)
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
			importPath = fs.NormalizeImportPath(importPath)
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

// runCollectorPhase runs symbol collection on all parsed modules.
// The collector handles all sub-phases internally (declarations, function bodies, method bodies).
func (p *Pipeline) runCollectorPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least parsed
		if p.ctx.GetModulePhase(importPath) < phase.PhaseParsed {
			continue
		}

		// Skip collection for native modules (symbols already registered)
		if module.Type == context_v2.ModuleBuiltin && module.AST != nil && len(module.AST.Nodes) == 0 {
			// Native module - symbols already in ModuleScope, just advance phase
			if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseCollected) {
				p.ctx.SetModulePhase(importPath, phase.PhaseCollected)
			}
			if p.ctx.Debug {
				colors.PURPLE.Printf("  ✓ %s (native)\n", importPath)
			}
			continue
		}

		// CollectModule handles all collection phases internally
		collector.CollectModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseCollected) {
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
		if p.ctx.GetModulePhase(importPath) < phase.PhaseCollected {
			continue
		}

		// Skip resolution for native modules (symbols already resolved)
		if module.Type == context_v2.ModuleBuiltin && module.AST != nil && len(module.AST.Nodes) == 0 {
			if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseResolved) {
				p.ctx.SetModulePhase(importPath, phase.PhaseResolved)
			}
			if p.ctx.Debug {
				colors.PURPLE.Printf("  ✓ %s (native)\n", importPath)
			}
			continue
		}

		resolver.ResolveModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseResolved) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseResolved", importPath), nil)
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	return nil
}

// runMethodSignatureTypeCheckPhase type checks only method signatures (attaches methods to types)
func (p *Pipeline) runMethodSignatureTypeCheckPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least resolved
		if p.ctx.GetModulePhase(importPath) < phase.PhaseResolved {
			continue
		}

		typechecker.TypeCheckMethodSignatures(p.ctx, module)

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
		if p.ctx.GetModulePhase(importPath) < phase.PhaseResolved {
			continue
		}

		// Skip type checking for native modules (types already validated during registration)
		if module.Type == context_v2.ModuleBuiltin && module.AST != nil && len(module.AST.Nodes) == 0 {
			if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseTypeChecked) {
				p.ctx.SetModulePhase(importPath, phase.PhaseTypeChecked)
			}
			if p.ctx.Debug {
				colors.PURPLE.Printf("  ✓ %s (native)\n", importPath)
			}
			continue
		}

		typechecker.CheckModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseTypeChecked) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseTypeChecked", importPath), nil)
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	// Don't return error here - let Run() handle final error check
	// This allows all modules to be type checked even if some have errors
	return nil
}

// runCFGAnalysisPhase runs control flow analysis on all type-checked modules
func (p *Pipeline) runCFGAnalysisPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Check if module is at least type-checked
		if p.ctx.GetModulePhase(importPath) < phase.PhaseTypeChecked {
			continue
		}

		cfganalyzer.AnalyzeModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseCFGAnalyzed) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseCFGAnalyzed", importPath), nil)
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\\n", importPath)
		}
	}

	// Don't return error here - let Run() handle final error check
	// This allows all modules to be analyzed even if some have errors
	return nil
}

// runCodegenPhase runs code generation on all type-checked modules
func (p *Pipeline) runCodegenPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		// Only generate code for entry module (main module)
		// Skip builtin modules and other imported modules
		if importPath != p.ctx.EntryModule {
			continue
		}

		// Check if module is at least type-checked
		if p.ctx.GetModulePhase(importPath) < phase.PhaseTypeChecked {
			continue
		}

		// Generate C code
		cFilePath := p.ctx.Config.OutputPath + ".c"
		if err := p.generateCode(module, cFilePath); err != nil {
			p.ctx.ReportError(fmt.Sprintf("code generation failed: %v", err), nil)
			continue
		}

		// Build executable
		execPath := p.ctx.Config.OutputPath
		// Only add .exe if user didn't specify custom output path and we're on Windows
		// (If user specified -o, they control the exact filename)
		if runtime.GOOS == "windows" && !strings.HasSuffix(execPath, ".exe") {
			// Check if this looks like a user-specified path (not the default pattern)
			// Default pattern is: <projectRoot>/bin/<projectName>
			defaultPattern := filepath.Join(p.ctx.Config.ProjectRoot, "bin", p.ctx.Config.ProjectName)
			if execPath == defaultPattern {
				execPath += ".exe"
			}
		}

		if err := p.buildExecutable(cFilePath, execPath); err != nil {
			p.ctx.ReportError(fmt.Sprintf("build failed: %v", err), nil)
			continue
		}

		if p.ctx.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	return nil
}

// generateCode generates C code for a module
func (p *Pipeline) generateCode(mod *context_v2.Module, outputPath string) error {
	return cgen.GenerateModule(p.ctx, mod, outputPath)
}

// buildExecutable compiles the C code into an executable
func (p *Pipeline) buildExecutable(cFilePath, execPath string) error {
	// Determine runtime path - use project root (most reliable)
	runtimePath := filepath.Join(p.ctx.Config.ProjectRoot, "runtime")

	// If not found, try relative to C file as fallback
	if _, err := os.Stat(runtimePath); os.IsNotExist(err) {
		cFileDir := filepath.Dir(cFilePath)
		runtimePath = filepath.Join(cFileDir, "..", "..", "runtime")
	}

	// Make it absolute
	if absRuntime, err := filepath.Abs(runtimePath); err == nil {
		runtimePath = absRuntime
	}

	opts := codegen.DefaultBuildOptions()
	// Use project root for runtime path (most reliable)
	opts.RuntimePath = filepath.Join(p.ctx.Config.ProjectRoot, "runtime")
	opts.OutputPath = execPath
	opts.Debug = p.ctx.Debug

	return codegen.BuildExecutable(p.ctx, cFilePath, opts)
}
