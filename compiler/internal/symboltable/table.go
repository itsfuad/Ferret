package symboltable

import (
	"ferret/compiler/internal/analyzer"
	"ferret/compiler/internal/utils"
	"ferret/compiler/types"
)

type SCOPE_KIND int

const (
	GLOBAL_SCOPE SCOPE_KIND = iota
	LOCAL_SCOPE
	BLOCK_SCOPE
	FUNCTION_SCOPE
	STRUCT_SCOPE
	INTERFACE_SCOPE
	MODULE_SCOPE
)

// SYMBOL_KIND represents the kind of symbol (variable, function, type, etc.)
type SYMBOL_KIND int

const (
	VARIABLE_SYMBOL SYMBOL_KIND = iota
	FUNCTION_SYMBOL
	TYPE_SYMBOL
	STRUCT_FIELD_SYMBOL
	PARAMETER_SYMBOL
	CONST_SYMBOL
	MODULE_SYMBOL
)

var compilerGlobalSymbols = map[string]*Symbol{
	"true": {
		Name:       "true",
		SymbolKind: CONST_SYMBOL,
		Type:       types.BOOL,
		IsMutable:  false,
		Value: &analyzer.BoolType{
			TypeName: types.BOOL,
			NodeType: analyzer.TYPE_NODE,
			IsLValue: false,
			IsRef:    false,
			IsConst:  true,
		},
	},
	"false": {
		Name:       "false",
		SymbolKind: CONST_SYMBOL,
		Type:       types.BOOL,
		IsMutable:  false,
		Value: &analyzer.BoolType{
			TypeName: types.BOOL,
			NodeType: analyzer.TYPE_NODE,
			IsLValue: false,
			IsRef:    false,
			IsConst:  true,
		},
	},
}

// Symbol represents a single symbol in the program
type Symbol struct {
	Name       string                // The name of the symbol
	SymbolKind SYMBOL_KIND           // The kind of symbol (variable, function, etc.)
	Type       types.TYPE_NAME       // The type of the symbol
	IsMutable  bool                  // Whether the symbol can be modified
	Location   SymbolLocation        // Source location information
	Value      analyzer.AnalyzerNode // For constants and compile-time known values
	Module     string                // The module this symbol belongs to (if any)
}

// SymbolLocation tracks the source location of a symbol
type SymbolLocation struct {
	File   string // Source file path
	Line   int    // Line number (1-based)
	Column int    // Column number (1-based)
}

// SymbolTable represents a scope and its symbols
type SymbolTable struct {
	symbols  map[string]*Symbol
	parent   *SymbolTable
	children []*SymbolTable
	kind     SCOPE_KIND
}

// NewSymbolTable creates a new symbol table with the given parent and scope kind
func NewSymbolTable(parent *SymbolTable, kind SCOPE_KIND) *SymbolTable {
	st := &SymbolTable{
		symbols:  make(map[string]*Symbol),
		parent:   parent,
		children: make([]*SymbolTable, 0),
		kind:     kind,
	}
	if parent != nil {
		parent.children = append(parent.children, st)
	}
	return st
}

// Define adds a new symbol to the current scope
func (st *SymbolTable) Define(symbol *Symbol) bool {
	if compilerGlobalSymbols[symbol.Name] != nil {
		return false // Symbol already defined as a compiler global symbol
	}
	if _, exists := st.symbols[symbol.Name]; exists {
		return false // Symbol already defined in current scope
	}
	st.symbols[symbol.Name] = symbol
	return true
}

// Resolve looks up a symbol by name in the current scope and parent scopes
func (st *SymbolTable) Resolve(name string) (*Symbol, bool) {

	if compilerGlobalSymbols[name] != nil {
		return compilerGlobalSymbols[name], true
	}

	// Check current scope
	sym, exists := st.symbols[name]
	if exists {
		return sym, exists
	}

	// Check parent scopes
	if st.parent != nil {
		return st.parent.Resolve(name)
	}

	return nil, exists
}

// ResolveLocal looks up a symbol only in the current scope
func (st *SymbolTable) ResolveLocal(name string) (*Symbol, bool) {
	if compilerGlobalSymbols[name] != nil {
		return compilerGlobalSymbols[name], true
	}
	sym, exists := st.symbols[name]
	return sym, exists
}

// Parent returns the parent scope
func (st *SymbolTable) Parent() *SymbolTable {
	return st.parent
}

// Children returns the child scopes
func (st *SymbolTable) Children() []*SymbolTable {
	return st.children
}

// ScopeKind returns the kind of this scope
func (st *SymbolTable) ScopeKind() SCOPE_KIND {
	return st.kind
}

// GetSymbols returns all symbols in the current scope
func (st *SymbolTable) GetSymbols() map[string]*Symbol {
	return st.symbols
}

// GetCompilerGlobalSymbols returns the compiler global symbols
func GetCompilerGlobalSymbols() map[string]*Symbol {
	return compilerGlobalSymbols
}

// Clear removes all symbols from the current scope
func (st *SymbolTable) Clear() {
	st.symbols = make(map[string]*Symbol)
}

// EnterScope creates and returns a new child scope
func (st *SymbolTable) EnterScope(kind SCOPE_KIND) *SymbolTable {
	return NewSymbolTable(st, kind)
}

// ExitScope returns to the parent scope
func (st *SymbolTable) ExitScope() *SymbolTable {
	if st.parent == nil {
		return st // Can't exit global scope
	}
	return st.parent
}

// ResolveInModule looks up a symbol by name in a specific module
func (st *SymbolTable) ResolveInModule(module, name string) (*Symbol, bool) {
	// Check current scope
	for _, sym := range st.symbols {
		if sym.Module == module && sym.Name == name && utils.IsCapitalized(sym.Name) {
			return sym, true
		}
	}

	// Check parent scopes
	if st.parent != nil {
		return st.parent.ResolveInModule(module, name)
	}

	return nil, false
}
