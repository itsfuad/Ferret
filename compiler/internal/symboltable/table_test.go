package symboltable

import (
	"ferret/compiler/internal/analyzer"
	"ferret/compiler/internal/source"
	"ferret/compiler/types"
	"testing"
)

const testFilePath = "test.fer"

// TestNewSymbolTable tests the creation of new symbol tables and their hierarchy.
func TestNewSymbolTable(t *testing.T) {
	// Test creating a root symbol table
	root := NewSymbolTable(nil, GLOBAL_SCOPE, testFilePath)
	if root == nil || root.Kind != GLOBAL_SCOPE {
		t.Errorf("NewSymbolTable returned nil or wrong kind: got %v", root)
	}
	if root.parent != nil {
		t.Error("Root symbol table should have nil parent")
	}
	if root.Filepath != testFilePath {
		t.Errorf("Expected filepath '%v', got %v", testFilePath, root.Filepath)
	}

	// Test creating a child symbol table
	child := NewSymbolTable(root, LOCAL_SCOPE, testFilePath)
	if child == nil || child.Kind != LOCAL_SCOPE {
		t.Errorf("NewSymbolTable returned nil or wrong kind: got %v", child)
	}
	if child.parent != root {
		t.Error("Child symbol table should have root as parent")
	}
	if len(root.children) != 1 {
		t.Error("Root should have one child")
	}
}

// TestDefineAndResolve tests symbol definition and resolution functionality.
func TestDefineAndResolve(t *testing.T) {
	table := NewSymbolTable(nil, GLOBAL_SCOPE, testFilePath)

	// Test defining a symbol
	startPos := &source.Position{Line: 1, Column: 1}
	endPos := &source.Position{Line: 1, Column: 10}
	location := source.NewLocation(startPos, endPos)
	symbol := &Symbol{
		Name:       "testVar",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  true,
		FilePath:   testFilePath,
		Location:   location,
		SymbolType: &analyzer.BoolType{
			TypeName: types.BOOL,
		},
	}

	if !table.Define(symbol) {
		t.Error("Failed to define symbol")
	}

	// Test resolving the symbol
	resolved, exists := table.Resolve("testVar")
	if !exists {
		t.Error("Failed to resolve defined symbol")
	}
	if resolved != symbol {
		t.Error("Resolved symbol doesn't match defined symbol")
	}

	// Test resolving non-existent symbol
	_, exists = table.Resolve("nonexistent")
	if exists {
		t.Error("Should not resolve non-existent symbol")
	}
}

// TestScopeHierarchy tests the scope inheritance and visibility rules.
func TestScopeHierarchy(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE, testFilePath)
	local := NewSymbolTable(global, LOCAL_SCOPE, testFilePath)

	// Define symbol in global scope
	globalStartPos := &source.Position{Line: 1, Column: 1}
	globalEndPos := &source.Position{Line: 1, Column: 10}
	globalLocation := source.NewLocation(globalStartPos, globalEndPos)
	globalSymbol := &Symbol{
		Name:       "globalVar",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  true,
		FilePath:   testFilePath,
		Location:   globalLocation,
		SymbolType: &analyzer.BoolType{
			TypeName: types.BOOL,
		},
	}
	global.Define(globalSymbol)

	// Test resolving global symbol from local scope
	resolved, exists := local.Resolve("globalVar")
	if !exists {
		t.Error("Failed to resolve global symbol from local scope")
	}
	if resolved != globalSymbol {
		t.Error("Resolved symbol doesn't match global symbol")
	}

	// Test local scope resolution
	localStartPos := &source.Position{Line: 2, Column: 1}
	localEndPos := &source.Position{Line: 2, Column: 10}
	localLocation := source.NewLocation(localStartPos, localEndPos)
	localSymbol := &Symbol{
		Name:       "localVar",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  true,
		FilePath:   testFilePath,
		Location:   localLocation,
		SymbolType: &analyzer.BoolType{
			TypeName: types.BOOL,
		},
	}
	local.Define(localSymbol)

	// Test resolving local symbol
	resolved, exists = local.Resolve("localVar")
	if !exists {
		t.Error("Failed to resolve local symbol")
	}
	if resolved != localSymbol {
		t.Error("Resolved symbol doesn't match local symbol")
	}

	// Test that local symbol is not visible in global scope
	_, exists = global.Resolve("localVar")
	if exists {
		t.Error("Local symbol should not be visible in global scope")
	}
}

// TestEnterExitScope tests scope management functionality.
func TestEnterExitScope(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE, testFilePath)

	// Test entering new scope
	local := global.EnterScope(LOCAL_SCOPE, testFilePath)
	if local == nil || local.Kind != LOCAL_SCOPE {
		t.Errorf("EnterScope returned nil or wrong kind: got %v", local)
	}
	if local.parent != global {
		t.Error("New scope should have global as parent")
	}
	if local.Filepath != testFilePath {
		t.Errorf("Expected filepath '%v', got %v", testFilePath, local.Filepath)
	}

	// Test exiting scope
	exited := local.ExitScope()
	if exited != global {
		t.Error("ExitScope should return parent scope")
	}

	// Test exiting global scope
	exited = global.ExitScope()
	if exited != global {
		t.Error("Exiting global scope should return itself")
	}
}

// TestResolveInModule tests module-level symbol resolution.
func TestResolveInModule(t *testing.T) {
	table := NewSymbolTable(nil, GLOBAL_SCOPE, testFilePath)

	// Define a module-level symbol
	startPos := &source.Position{Line: 1, Column: 1}
	endPos := &source.Position{Line: 1, Column: 10}
	location := source.NewLocation(startPos, endPos)
	symbol := &Symbol{
		Name:       "ModuleVar",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  true,
		FilePath:   testFilePath,
		Location:   location,
		Module:     "testModule",
		SymbolType: &analyzer.BoolType{
			TypeName: types.BOOL,
		},
	}
	table.Define(symbol)

	// Test resolving module symbol
	resolved, exists := table.ResolveInModule("testModule", "ModuleVar")
	if !exists {
		t.Error("Failed to resolve module symbol")
	}
	if resolved != symbol {
		t.Error("Resolved symbol doesn't match module symbol")
	}

	// Test resolving non-existent module symbol
	_, exists = table.ResolveInModule("testModule", "NonExistent")
	if exists {
		t.Error("Should not resolve non-existent module symbol")
	}

	// Test resolving with wrong module
	_, exists = table.ResolveInModule("wrongModule", "ModuleVar")
	if exists {
		t.Error("Should not resolve symbol from wrong module")
	}
}

// TestCompilerGlobalSymbols tests the built-in compiler global symbols.
func TestCompilerGlobalSymbols(t *testing.T) {
	// Test that compiler global symbols are accessible
	trueSymbol, exists := compilerGlobalSymbols["true"]
	if !exists {
		t.Error("Compiler global symbol 'true' not found")
	}
	if trueSymbol.SymbolKind != VARIABLE_SYMBOL {
		t.Error("Compiler global symbol 'true' should be a variable")
	}
	if trueSymbol.IsMutable {
		t.Error("Compiler global symbol 'true' should not be mutable")
	}
	if trueSymbol.SymbolType == nil {
		t.Error("Compiler global symbol 'true' should have a type")
	}

	falseSymbol, exists := compilerGlobalSymbols["false"]
	if !exists {
		t.Error("Compiler global symbol 'false' not found")
	}
	if falseSymbol.SymbolKind != VARIABLE_SYMBOL {
		t.Error("Compiler global symbol 'false' should be a variable")
	}
	if falseSymbol.IsMutable {
		t.Error("Compiler global symbol 'false' should not be mutable")
	}
	if falseSymbol.SymbolType == nil {
		t.Error("Compiler global symbol 'false' should have a type")
	}
}

// TestClear tests the symbol table clearing functionality.
func TestClear(t *testing.T) {
	table := NewSymbolTable(nil, GLOBAL_SCOPE, testFilePath)

	// Define some symbols
	startPos1 := &source.Position{Line: 1, Column: 1}
	endPos1 := &source.Position{Line: 1, Column: 10}
	location1 := source.NewLocation(startPos1, endPos1)

	startPos2 := &source.Position{Line: 2, Column: 1}
	endPos2 := &source.Position{Line: 2, Column: 10}
	location2 := source.NewLocation(startPos2, endPos2)

	symbol1 := &Symbol{
		Name:       "var1",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  true,
		FilePath:   testFilePath,
		Location:   location1,
		SymbolType: &analyzer.BoolType{
			TypeName: types.BOOL,
		},
	}
	symbol2 := &Symbol{
		Name:       "var2",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  true,
		FilePath:   testFilePath,
		Location:   location2,
		SymbolType: &analyzer.BoolType{
			TypeName: types.BOOL,
		},
	}

	table.Define(symbol1)
	table.Define(symbol2)

	// Verify symbols exist
	if _, exists := table.Resolve("var1"); !exists {
		t.Error("Symbol var1 should exist before clear")
	}
	if _, exists := table.Resolve("var2"); !exists {
		t.Error("Symbol var2 should exist before clear")
	}

	// Clear the table
	table.Clear()

	// Verify symbols are gone
	if _, exists := table.Resolve("var1"); exists {
		t.Error("Symbol var1 should not exist after clear")
	}
	if _, exists := table.Resolve("var2"); exists {
		t.Error("Symbol var2 should not exist after clear")
	}
}

// TestResolveLocal tests local-only symbol resolution.
func TestResolveLocal(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE, testFilePath)
	local := NewSymbolTable(global, LOCAL_SCOPE, testFilePath)

	// Define symbol in global scope
	globalStartPos := &source.Position{Line: 1, Column: 1}
	globalEndPos := &source.Position{Line: 1, Column: 10}
	globalLocation := source.NewLocation(globalStartPos, globalEndPos)
	globalSymbol := &Symbol{
		Name:       "globalVar",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  true,
		FilePath:   testFilePath,
		Location:   globalLocation,
		SymbolType: &analyzer.BoolType{
			TypeName: types.BOOL,
		},
	}
	global.Define(globalSymbol)

	// Test that global symbol is not visible in local scope when using ResolveLocal
	_, exists := local.ResolveLocal("globalVar")
	if exists {
		t.Error("Global symbol should not be visible in local scope when using ResolveLocal")
	}

	// Define symbol in local scope
	localStartPos := &source.Position{Line: 2, Column: 1}
	localEndPos := &source.Position{Line: 2, Column: 10}
	localLocation := source.NewLocation(localStartPos, localEndPos)
	localSymbol := &Symbol{
		Name:       "localVar",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  true,
		FilePath:   testFilePath,
		Location:   localLocation,
		SymbolType: &analyzer.BoolType{
			TypeName: types.BOOL,
		},
	}
	local.Define(localSymbol)

	// Test that local symbol is visible in local scope
	resolved, exists := local.ResolveLocal("localVar")
	if !exists {
		t.Error("Local symbol should be visible in local scope")
	}
	if resolved != localSymbol {
		t.Error("Resolved symbol doesn't match local symbol")
	}
}
