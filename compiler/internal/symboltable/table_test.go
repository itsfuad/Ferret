package symboltable

import (
	"ferret/compiler/types"
	"testing"
)

func TestNewSymbolTable(t *testing.T) {
	// Test creating global scope
	global := NewSymbolTable(nil, GLOBAL_SCOPE)
	if global.parent != nil {
		t.Error("Global scope should not have a parent")
	}
	if global.kind != GLOBAL_SCOPE {
		t.Error("Expected GLOBAL_SCOPE kind")
	}

	// Test creating child scope
	local := NewSymbolTable(global, LOCAL_SCOPE)
	if local.parent != global {
		t.Error("Local scope should have global scope as parent")
	}
	if local.kind != LOCAL_SCOPE {
		t.Error("Expected LOCAL_SCOPE kind")
	}
	if len(global.children) != 1 || global.children[0] != local {
		t.Error("Parent scope should track child scope")
	}
}

func TestSymbolDefinition(t *testing.T) {
	st := NewSymbolTable(nil, GLOBAL_SCOPE)

	// Test defining a new symbol
	sym := &Symbol{
		Name:       "x",
		SymbolKind: VARIABLE_SYMBOL,
		Type:       types.INT32,
		IsMutable:  true,
	}

	if !st.Define(sym) {
		t.Error("Failed to define new symbol")
	}

	// Test defining duplicate symbol
	if st.Define(sym) {
		t.Error("Should not allow duplicate symbol definition")
	}

	// Test defining compiler global symbol
	symTrue := &Symbol{
		Name:       "true",
		SymbolKind: CONST_SYMBOL,
		Type:       types.BOOL,
	}
	if st.Define(symTrue) {
		t.Error("Should not allow redefining compiler global symbol")
	}
}

func TestSymbolResolution(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE)
	local := NewSymbolTable(global, LOCAL_SCOPE)

	// Define symbol in global scope
	globalSym := &Symbol{
		Name:       "globalVar",
		SymbolKind: VARIABLE_SYMBOL,
		Type:       types.INT32,
	}
	global.Define(globalSym)

	// Define symbol in local scope
	localSym := &Symbol{
		Name:       "localVar",
		SymbolKind: VARIABLE_SYMBOL,
		Type:       types.STRING,
	}
	local.Define(localSym)

	// Test resolving from local scope
	if sym, exists := local.Resolve("globalVar"); !exists || sym != globalSym {
		t.Error("Failed to resolve global symbol from local scope")
	}
	if sym, exists := local.Resolve("localVar"); !exists || sym != localSym {
		t.Error("Failed to resolve local symbol")
	}

	// Test resolving compiler globals
	if sym, exists := local.Resolve("true"); !exists || sym.Type != types.BOOL {
		t.Error("Failed to resolve compiler global symbol")
	}

	// Test resolving non-existent symbol
	if _, exists := local.Resolve("nonexistent"); exists {
		t.Error("Should not resolve non-existent symbol")
	}
}

func TestResolveLocal(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE)
	local := NewSymbolTable(global, LOCAL_SCOPE)

	globalSym := &Symbol{
		Name:       "var",
		SymbolKind: VARIABLE_SYMBOL,
		Type:       types.INT32,
	}
	global.Define(globalSym)

	// Should not find global symbol in local scope
	if _, exists := local.ResolveLocal("var"); exists {
		t.Error("ResolveLocal should not find symbol in parent scope")
	}

	if sym, exists := local.Resolve("var"); !exists || sym != globalSym {
		t.Error("ResolveLocal failed to find symbol in current scope")
	}
}

func TestScopeOperations(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE)

	// Test entering new scope
	block := global.EnterScope(BLOCK_SCOPE)
	if block.parent != global || block.kind != BLOCK_SCOPE {
		t.Error("EnterScope failed to create proper scope")
	}

	// Test exiting scope
	parent := block.ExitScope()
	if parent != global {
		t.Error("ExitScope failed to return to parent scope")
	}

	// Test exiting global scope (should return self)
	if global.ExitScope() != global {
		t.Error("Exiting global scope should return self")
	}
}

func TestClearSymbols(t *testing.T) {
	st := NewSymbolTable(nil, GLOBAL_SCOPE)

	sym := &Symbol{
		Name:       "x",
		SymbolKind: VARIABLE_SYMBOL,
		Type:       types.INT32,
	}
	st.Define(sym)

	st.Clear()
	if len(st.GetSymbols()) != 0 {
		t.Error("Clear failed to remove all symbols")
	}
}

func TestGetCompilerGlobalSymbols(t *testing.T) {
	globals := GetCompilerGlobalSymbols()

	// Check true and false constants
	if trueSymbol, exists := globals["true"]; !exists || trueSymbol.Type != types.BOOL {
		t.Error("Missing or invalid true constant in compiler globals")
	}
	if falseSymbol, exists := globals["false"]; !exists || falseSymbol.Type != types.BOOL {
		t.Error("Missing or invalid false constant in compiler globals")
	}
}
