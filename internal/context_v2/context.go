// Package context_v2 provides the central compilation context for the Ferret compiler.
//
// ARCHITECTURE:
// This implements a per-module phase tracking system where each module (source file)
// progresses through compilation phases independently. This design:
// - Handles multi-file imports correctly (same file imported multiple times)
// - Enables incremental compilation
// - Prevents redundant parsing/analysis
// - Matches production compiler architectures (Rustc, TypeScript, Zig)
//
// DESIGN PRINCIPLES:
// 1. Import paths are semantic identifiers (not file system paths)
// 2. Each module tracks its own compilation phase
// 3. Cycle detection prevents circular imports
// 4. Module types (Local, Builtin, Remote) enable flexible resolution
package context_v2

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/source"
	"compiler/internal/table"
	"compiler/internal/types"
	"compiler/internal/utils/fs"
)

// ModulePhase tracks the compilation phase of an individual module
//
// Phase progression must be sequential and respect dependencies:
// - NotStarted -> Lexed -> Parsed (currently implemented)
// - Parsed -> Collected -> Resolved -> TypeChecked -> CodeGen (future)
//
// Phase transitions are validated using AdvanceModulePhase() which checks
// that prerequisites are satisfied via the phasePrerequisites map.
//
// Phase prerequisites (enforced by tests in pipeline_test.go):
// - A module at PhaseResolved requires all its imports at least PhaseParsed
// - A module at PhaseTypeChecked requires all its imports at least PhaseResolved
type ModulePhase int

const (
	PhaseNotStarted  ModulePhase = iota // Module discovered but not processed
	PhaseLexed                          // Tokens generated
	PhaseParsed                         // AST built
	PhaseCollected                      // Symbols collected (first pass)
	PhaseResolved                       // Imports and symbols resolved
	PhaseTypeChecked                    // Type checking complete
	PhaseCodeGen                        // Code generation complete
)

// phasePrerequisites maps each phase to its required predecessor phase
// This explicit mapping is safer than arithmetic and allows for non-linear phase progressions
var phasePrerequisites = map[ModulePhase]ModulePhase{
	PhaseLexed:       PhaseNotStarted,
	PhaseParsed:      PhaseLexed,
	PhaseCollected:   PhaseParsed,
	PhaseResolved:    PhaseCollected,
	PhaseTypeChecked: PhaseResolved,
	PhaseCodeGen:     PhaseTypeChecked,
}

func (p ModulePhase) String() string {
	switch p {
	case PhaseNotStarted:
		return "NotStarted"
	case PhaseLexed:
		return "Lexed"
	case PhaseParsed:
		return "Parsed"
	case PhaseCollected:
		return "Collected"
	case PhaseResolved:
		return "Resolved"
	case PhaseTypeChecked:
		return "TypeChecked"
	case PhaseCodeGen:
		return "CodeGen"
	default:
		return "Unknown"
	}
}

// ModuleType categorizes how a module is resolved
type ModuleType int

const (
	ModuleLocal    ModuleType = iota // Local project module
	ModuleBuiltin                    // Standard library module
	ModuleRemote                     // Remote dependency (github.com/...)
	ModuleNeighbor                   // Neighbor project module
	ModuleUnknown                    // Unknown/unresolved
)

func (mt ModuleType) String() string {
	switch mt {
	case ModuleLocal:
		return "Local"
	case ModuleBuiltin:
		return "Builtin"
	case ModuleRemote:
		return "Remote"
	case ModuleNeighbor:
		return "Neighbor"
	default:
		return "Unknown"
	}
}

// Module represents a single compiled module with its semantic information
type Module struct {
	// Core data
	ImportPath string      // Logical import path (e.g., "myproject/utils/math")
	FilePath   string      // Physical file path
	Type       ModuleType  // How this module was resolved
	AST        *ast.Module // Parsed syntax tree

	// Compilation state
	Phase ModulePhase // Current compilation phase

	// Semantic data
	Scope   *table.SymbolTable // Module-level symbols
	Imports []*Import          // Resolved imports
	//ExprTypes map[ast.Expression]types.SemType // Type of each expression (filled during type checking)

	// Source metadata
	Content string // Raw source code (for diagnostics)

	// Concurrency control
	Mu sync.Mutex // Protects field updates during parallel parsing
}

// Import represents a resolved import statement
type Import struct {
	Path     string           // Import path as written in source
	Alias    string           // Optional alias (e.g., "import math as m")
	Location *source.Location // Source location for diagnostics
	IsUsed   bool
}

// CompilerContext is the central compilation state manager
type CompilerContext struct {
	// Module registry: import path -> Module
	// This is the authoritative source for all compiled modules
	Modules map[string]*Module
	mu      sync.RWMutex // protects Modules and DepGraph during parallel parse

	// Sorted module keys in topological order
	sortedModules []string

	// Entry point
	EntryPoint  string // Full path to entry file
	EntryModule string // Import path of entry module

	// Universe scope: built-in types and functions
	Universe *table.SymbolTable

	// Diagnostics: centralized error collection
	Diagnostics *diagnostics.DiagnosticBag

	// Dependency graph: import path -> list of imported paths
	// Used for cycle detection and build ordering
	DepGraph map[string][]string

	// Configuration
	Config *Config

	// Debug mode
	Debug bool
}

// Config holds compiler configuration
type Config struct {
	// Project information
	ProjectName string // Name of the project
	ProjectRoot string // Root directory of the project

	// Build configuration
	OutputPath string // Where to write compiled output
	Extension  string // Source file extension (default: ".fer")

	// Module resolution
	BuiltinModulesPath string            // Path to standard library
	BuiltinModules     map[string]string // name -> path mapping

	// Remote modules (future)
	RemoteCachePath string // Cache directory for remote dependencies (.ferret)
}

// New creates a new compiler context
func New(config *Config, debug bool) *CompilerContext {
	if config == nil {
		config = &Config{
			Extension: ".fer",
		}
	}

	// Set up remote cache path if not specified
	if config.RemoteCachePath == "" && config.ProjectRoot != "" {
		config.RemoteCachePath = filepath.Join(config.ProjectRoot, ".ferret")
		os.MkdirAll(config.RemoteCachePath, 0755)
	}

	// Create universe scope with built-in types
	universe := table.NewSymbolTable(nil)
	registerBuiltins(universe)

	ctx := &CompilerContext{
		Modules:       make(map[string]*Module),
		sortedModules: []string{},
		Universe:      universe,
		Diagnostics:   diagnostics.NewDiagnosticBag(""),
		DepGraph:      make(map[string][]string),
		Config:        config,
		Debug:         debug,
	}

	// Auto-load builtin modules if path is provided
	if config.BuiltinModulesPath != "" {
		ctx.loadBuiltinModules()
	}

	return ctx
}

// loadBuiltinModules discovers and registers all builtin modules from the builtin path
func (ctx *CompilerContext) loadBuiltinModules() {
	if ctx.Config.BuiltinModules == nil {
		ctx.Config.BuiltinModules = make(map[string]string)
	}

	builtinPath := ctx.Config.BuiltinModulesPath

	// Check if builtin path exists
	if !fs.IsDir(builtinPath) {
		// Silently skip if builtin path doesn't exist (for testing/minimal setups)
		return
	}

	// Scan for .fer files in builtin directory
	entries, err := os.ReadDir(builtinPath)
	if err != nil {
		// Non-critical error - just skip builtin loading
		return
	}

	for _, entry := range entries {
		if entry.IsDir() {
			// For directories, register as module package
			dirPath := filepath.Join(builtinPath, entry.Name())
			ctx.Config.BuiltinModules[entry.Name()] = dirPath
		} else if strings.HasSuffix(entry.Name(), ctx.Config.Extension) {
			// For individual .fer files, register as standalone module
			moduleName := strings.TrimSuffix(entry.Name(), ctx.Config.Extension)
			ctx.Config.BuiltinModules[moduleName] = builtinPath
		}
	}
}

// registerBuiltins populates the universe scope with built-in types
func registerBuiltins(universe *table.SymbolTable) {
	builtinTypes := []types.SemType{
		types.TypeI8, types.TypeI16, types.TypeI32, types.TypeI64,
		types.TypeU8, types.TypeU16, types.TypeU32, types.TypeU64,
		types.TypeF32, types.TypeF64,
		types.TypeString, types.TypeBool, types.TypeVoid,
	}

	for _, typ := range builtinTypes {
		universe.Declare(typ.String(), &table.Symbol{
			Name:     typ.String(),
			Kind:     table.SymbolType,
			Type:     typ,
			Exported: true,
		})
	}

	// Built-in constants
	universe.Declare("true", &table.Symbol{
		Name:     "true",
		Kind:     table.SymbolConstant,
		Type:     types.TypeBool,
		Exported: true,
	})
	universe.Declare("false", &table.Symbol{
		Name:     "false",
		Kind:     table.SymbolConstant,
		Type:     types.TypeBool,
		Exported: true,
	})
}

// SetEntryPoint sets the entry point for compilation
func (ctx *CompilerContext) SetEntryPoint(filePath string) error {
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return fmt.Errorf("failed to resolve entry point: %w", err)
	}

	if !fs.IsValidFile(absPath) {
		return fmt.Errorf("entry point does not exist: %s", absPath)
	}

	ctx.EntryPoint = filepath.ToSlash(absPath)

	// Derive import path from file path
	ctx.EntryModule = ctx.FilePathToImportPath(absPath)

	return nil
}

// SetEntryPointWithCode sets the entry point with in-memory code (for WASM/playground)
func (ctx *CompilerContext) SetEntryPointWithCode(code, moduleName string) error {
	// For in-memory compilation, use a virtual path
	virtualPath := filepath.Join(ctx.Config.ProjectRoot, moduleName+ctx.Config.Extension)
	ctx.EntryPoint = filepath.ToSlash(virtualPath)

	// Derive import path consistently with file mode
	ctx.EntryModule = ctx.FilePathToImportPath(virtualPath)

	// Add source content to diagnostic bag's cache so it can display source lines
	ctx.Diagnostics.AddSourceContent(virtualPath, code)

	// Create the entry module directly with the provided code
	module := &Module{
		FilePath: virtualPath,
		Type:     ModuleLocal,
		Phase:    PhaseNotStarted,
		Scope:    table.NewSymbolTable(ctx.Universe),
		Content:  code,
	}

	ctx.AddModule(ctx.EntryModule, module)
	return nil
}

// FilePathToImportPath converts a file path to a logical import path
func (ctx *CompilerContext) FilePathToImportPath(filePath string) string {
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return ""
	}

	absPath = filepath.ToSlash(absPath)
	projectRoot := filepath.ToSlash(ctx.Config.ProjectRoot)

	// Get relative path from project root
	relPath, err := filepath.Rel(projectRoot, absPath)
	if err != nil {
		// If not under project root, use filename
		return strings.TrimSuffix(filepath.Base(filePath), ctx.Config.Extension)
	}

	relPath = filepath.ToSlash(relPath)
	// Remove extension
	relPath = strings.TrimSuffix(relPath, ctx.Config.Extension)

	// Build import path: projectname/path/to/module
	if ctx.Config.ProjectName != "" {
		return ctx.Config.ProjectName + "/" + relPath
	}
	return relPath
}

// NormalizeImportPath normalizes an import path for semantic use
//
// Import paths are semantic identifiers, not file system paths. This function
// ensures consistent representation by:
// - Trimming leading/trailing whitespace
// - Trimming leading/trailing slashes
// - Collapsing multiple consecutive slashes into one
// - Using forward slashes only (no backslashes)
//
// Examples:
//   - " myproject/utils " -> "myproject/utils"
//   - "myproject//utils"  -> "myproject/utils"
//   - "/myproject/utils/" -> "myproject/utils"
//   - "myproject\utils"   -> "myproject/utils"
func (ctx *CompilerContext) NormalizeImportPath(importPath string) string {
	// Trim whitespace
	importPath = strings.TrimSpace(importPath)

	// Replace backslashes with forward slashes (handle Windows-style paths in source)
	importPath = strings.ReplaceAll(importPath, "\\", "/")

	// Collapse multiple slashes into one
	for strings.Contains(importPath, "//") {
		importPath = strings.ReplaceAll(importPath, "//", "/")
	}

	// Trim leading/trailing slashes
	importPath = strings.Trim(importPath, "/")

	return importPath
}

// ImportPathToFilePath converts an import path to a file path
func (ctx *CompilerContext) ImportPathToFilePath(importPath string) (string, ModuleType, error) {
	// Normalize import path to ensure consistent lookup
	importPath = ctx.NormalizeImportPath(importPath)

	// Determine module type
	packageName := fs.FirstPart(importPath)
	cleanPath := strings.TrimPrefix(importPath, packageName+"/")

	// Check if it's a local module
	if packageName == ctx.Config.ProjectName {
		filePath := filepath.Join(ctx.Config.ProjectRoot, cleanPath+ctx.Config.Extension)
		if fs.IsValidFile(filePath) {
			return filepath.ToSlash(filePath), ModuleLocal, nil
		}
		return "", ModuleUnknown, fmt.Errorf("module not found: %s", importPath)
	}

	// Check if it's a builtin module
	if builtinPath, ok := ctx.Config.BuiltinModules[packageName]; ok {
		filePath := filepath.Join(builtinPath, cleanPath+ctx.Config.Extension)
		if fs.IsValidFile(filePath) {
			return filepath.ToSlash(filePath), ModuleBuiltin, nil
		}
		return "", ModuleUnknown, fmt.Errorf("builtin module not found: %s", importPath)
	}

	// Check if it's a remote module (future implementation)
	if ctx.isRemoteImport(importPath) {
		// TODO: Implement remote module resolution
		return "", ModuleRemote, fmt.Errorf("remote imports not yet implemented: %s", importPath)
	}

	return "", ModuleUnknown, fmt.Errorf("cannot resolve import: %s", importPath)
}

// isRemoteImport checks if an import path is a remote module
func (ctx *CompilerContext) isRemoteImport(importPath string) bool {
	return strings.HasPrefix(importPath, "github.com/") ||
		strings.HasPrefix(importPath, "gitlab.com/") ||
		strings.HasPrefix(importPath, "bitbucket.org/")
}

// AddModule registers a module in the context
func (ctx *CompilerContext) AddModule(importPath string, module *Module) {
	if module == nil {
		panic(fmt.Sprintf("cannot add nil module for %q", importPath))
	}

	ctx.mu.Lock()
	defer ctx.mu.Unlock()

	// Don't overwrite existing modules
	if _, exists := ctx.Modules[importPath]; exists {
		return
	}

	module.ImportPath = importPath
	ctx.Modules[importPath] = module
}

// GetModule retrieves a module by import path
func (ctx *CompilerContext) GetModule(importPath string) (*Module, bool) {
	ctx.mu.RLock()
	defer ctx.mu.RUnlock()
	module, exists := ctx.Modules[importPath]
	return module, exists
}

// HasModule checks if a module exists in the context
func (ctx *CompilerContext) HasModule(importPath string) bool {
	ctx.mu.RLock()
	defer ctx.mu.RUnlock()
	_, exists := ctx.Modules[importPath]
	return exists
}

// GetModulePhase returns the current phase of a module
func (ctx *CompilerContext) GetModulePhase(importPath string) ModulePhase {
	ctx.mu.RLock()
	defer ctx.mu.RUnlock()
	if module, exists := ctx.Modules[importPath]; exists {
		return module.Phase
	}
	return PhaseNotStarted
}

// SetModulePhase updates the phase of a module
func (ctx *CompilerContext) SetModulePhase(importPath string, phase ModulePhase) {
	ctx.mu.Lock()
	defer ctx.mu.Unlock()
	if module, exists := ctx.Modules[importPath]; exists {
		module.Mu.Lock()
		module.Phase = phase
		module.Mu.Unlock()
	}
}

// AdvanceModulePhase advances a module to the next phase with validation
// Returns false if the phase transition is invalid (prerequisites not met)
func (ctx *CompilerContext) AdvanceModulePhase(importPath string, targetPhase ModulePhase) bool {
	if !ctx.CanProcessPhase(importPath, targetPhase) {
		return false
	}
	ctx.SetModulePhase(importPath, targetPhase)
	return true
}

// CanProcessPhase checks if a module is ready for a specific phase
// Uses explicit prerequisite map for safe phase transitions
func (ctx *CompilerContext) CanProcessPhase(importPath string, requiredPhase ModulePhase) bool {
	currentPhase := ctx.GetModulePhase(importPath)
	prerequisite, exists := phasePrerequisites[requiredPhase]
	if !exists {
		// Unknown phase - cannot process
		return false
	}
	return currentPhase == prerequisite
}

// IsModuleParsed checks if a module has been parsed (at least)
func (ctx *CompilerContext) IsModuleParsed(importPath string) bool {
	return ctx.GetModulePhase(importPath) >= PhaseParsed
}

// AddDependency registers an import relationship
// Returns error if adding this dependency would create a cycle
func (ctx *CompilerContext) AddDependency(importer, imported string) error {
	// Normalize paths
	importer = filepath.ToSlash(importer)
	imported = filepath.ToSlash(imported)

	ctx.mu.Lock()
	defer ctx.mu.Unlock()

	// Check for cycle before adding
	if cycle := ctx.findCycle(imported, importer); cycle != nil {
		return fmt.Errorf("circular import detected: %s", formatCycle(cycle))
	}

	// Check if dependency already exists (deduplicate)
	for _, existing := range ctx.DepGraph[importer] {
		if existing == imported {
			return nil // Already added, skip
		}
	}

	// Add dependency
	ctx.DepGraph[importer] = append(ctx.DepGraph[importer], imported)
	return nil
}

// findCycle uses DFS to detect if adding edge (from -> to) creates a cycle
func (ctx *CompilerContext) findCycle(from, to string) []string {
	visited := make(map[string]bool)
	path := []string{}

	if ctx.hasCyclePath(from, to, visited, &path) {
		// Construct cycle path
		cycle := append([]string{to}, path...)
		cycle = append(cycle, to)
		return cycle
	}
	return nil
}

// hasCyclePath performs DFS to find path from start to target
func (ctx *CompilerContext) hasCyclePath(start, target string, visited map[string]bool, path *[]string) bool {
	if start == target {
		return true
	}

	if visited[start] {
		return false
	}

	visited[start] = true
	*path = append(*path, start)

	for _, dep := range ctx.DepGraph[start] {
		dep = filepath.ToSlash(dep)
		if ctx.hasCyclePath(dep, target, visited, path) {
			return true
		}
	}

	// Backtrack
	*path = (*path)[:len(*path)-1]
	return false
}

// formatCycle formats a cycle path for error messages
func formatCycle(cycle []string) string {
	parts := make([]string, len(cycle))
	for i, path := range cycle {
		parts[i] = filepath.Base(path)
	}
	return strings.Join(parts, " -> ")
}

// HasErrors returns true if any errors have been reported
func (ctx *CompilerContext) HasErrors() bool {
	return ctx.Diagnostics.HasErrors()
}

// ReportError adds an error diagnostic
func (ctx *CompilerContext) ReportError(message string, location *source.Location) {
	diag := &diagnostics.Diagnostic{
		Severity: diagnostics.Error,
		Message:  message,
		Labels: []diagnostics.Label{
			{Location: location, Message: "", Style: diagnostics.Primary},
		},
	}
	ctx.Diagnostics.Add(diag)
}

// EmitDiagnostics outputs all collected diagnostics
func (ctx *CompilerContext) EmitDiagnostics() {
	ctx.Diagnostics.EmitAll()
}

// ModuleCount returns the number of modules in the context
func (ctx *CompilerContext) ModuleCount() int {
	return len(ctx.Modules)
}

// GetModuleNames returns all module import paths
func (ctx *CompilerContext) GetModuleNames() []string {
	return ctx.sortedModules
}

// ComputeTopologicalOrder computes and stores the topological order of modules
func (ctx *CompilerContext) ComputeTopologicalOrder() {
	ctx.mu.Lock()
	defer ctx.mu.Unlock()

	inDegree := make(map[string]int)

	for modulePath := range ctx.Modules {
		if _, exists := inDegree[modulePath]; !exists {
			inDegree[modulePath] = 0
		}
	}

	for importer, deps := range ctx.DepGraph {
		for range deps {
			inDegree[importer]++
		}
	}

	var queue []string
	for module := range ctx.Modules {
		if inDegree[module] == 0 {
			queue = append(queue, module)
		}
	}
	sort.Strings(queue)

	var sorted []string
	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		sorted = append(sorted, current)

		var next []string
		for importer, deps := range ctx.DepGraph {
			for _, dep := range deps {
				if dep == current {
					inDegree[importer]--
					if inDegree[importer] == 0 {
						next = append(next, importer)
					}
				}
			}
		}
		sort.Strings(next)
		queue = append(queue, next...)
	}

	ctx.sortedModules = sorted
}
