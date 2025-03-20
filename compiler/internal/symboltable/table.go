package symboltable

import (
	"ferret/compiler/internal/types"
)

type ScopeKind int

const (
	GLOBAL_SCOPE ScopeKind = iota
	LOCAL_SCOPE
	BLOCK_SCOPE
	FUNCTION_SCOPE
	STRUCT_SCOPE
	INTERFACE_SCOPE
	MODULE_SCOPE
)

// SymbolKind represents the kind of symbol (variable, function, type, etc.)
type SymbolKind int

const (
	VARIABLE_SYMBOL SymbolKind = iota
	FUNCTION_SYMBOL
	TYPE_SYMBOL
	STRUCT_FIELD_SYMBOL
	PARAMETER_SYMBOL
	CONST_SYMBOL
)

var compilerGlobalSymbols = map[string]*Symbol{
	"true": {
		Name:       "true",
		SymbolKind: CONST_SYMBOL,
		Type:       types.BOOL,
		IsMutable:  false,
		Value:      true,
	},
	"false": {
		Name:       "false",
		SymbolKind: CONST_SYMBOL,
		Type:       types.BOOL,
		IsMutable:  false,
		Value:      false,
	},
}

// Symbol represents a single symbol in the program
type Symbol struct {
	Name       string          // The name of the symbol
	SymbolKind SymbolKind      // The kind of symbol (variable, function, etc.)
	Type       types.TYPE_NAME // The type of the symbol
	Scope      ScopeKind       // The scope in which this symbol is defined
	IsExported bool            // Whether the symbol is exported (public)
	IsMutable  bool            // Whether the symbol can be modified
	Location   SymbolLocation  // Source location information
	Value      any             // For constants and compile-time known values
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
	kind     ScopeKind
}

// NewSymbolTable creates a new symbol table with the given parent and scope kind
func NewSymbolTable(parent *SymbolTable, kind ScopeKind) *SymbolTable {
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
func (st *SymbolTable) ScopeKind() ScopeKind {
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
func (st *SymbolTable) EnterScope(kind ScopeKind) *SymbolTable {
	return NewSymbolTable(st, kind)
}

// ExitScope returns to the parent scope
func (st *SymbolTable) ExitScope() *SymbolTable {
	if st.parent == nil {
		return st // Can't exit global scope
	}
	return st.parent
}
