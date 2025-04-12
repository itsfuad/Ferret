package symboltable

import (
	"ferret/compiler/internal/source"
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

func (s SCOPE_KIND) String() string {
	return []string{"global", "local", "block", "function", "struct", "interface", "module"}[s]
}

// SYMBOL_KIND represents the kind of symbol (variable, function, type, etc.)
type SYMBOL_KIND int

const (
	VARIABLE_SYMBOL SYMBOL_KIND = iota
	TYPE_SYMBOL
	MODULE_SYMBOL
)

var compilerGlobalSymbols = map[string]*Symbol{
	"true": {
		Name:       "true",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  false,
		SymbolType: &BoolType{
			TypeName: types.BOOL,
		},
	},
	"false": {
		Name:       "false",
		SymbolKind: VARIABLE_SYMBOL,
		IsMutable:  false,
		SymbolType: &BoolType{
			TypeName: types.BOOL,
		},
	},
}

// Symbol represents a single symbol in the program
type Symbol struct {
	Name       string
	SymbolKind SYMBOL_KIND
	IsMutable  bool
	FilePath   string
	Location   *source.Location
	SymbolType AnalyzerNode
	Module     string
}

// SymbolTable represents a scope and its symbols
type SymbolTable struct {
	Symbols  map[string]*Symbol
	parent   *SymbolTable
	children []*SymbolTable
	Kind     SCOPE_KIND
	Filepath string
}

// NewSymbolTable creates a new symbol table with the given parent and scope kind
func NewSymbolTable(parent *SymbolTable, kind SCOPE_KIND, filepath string) *SymbolTable {
	st := &SymbolTable{
		Symbols:  make(map[string]*Symbol),
		parent:   parent,
		children: make([]*SymbolTable, 0),
		Kind:     kind,
		Filepath: filepath,
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
	if _, exists := st.Symbols[symbol.Name]; exists {
		return false // Symbol already defined in current scope
	}
	st.Symbols[symbol.Name] = symbol
	return true
}

func (st *SymbolTable) UpdateType(name string, symbolType AnalyzerNode) bool {
	//must exist
	if _, exists := st.Symbols[name]; !exists {
		return false
	}
	// must be mutable
	if !st.Symbols[name].IsMutable {
		return false
	}
	st.Symbols[name].SymbolType = symbolType
	return true
}

// Resolve looks up a symbol by name in the current scope and parent scopes
func (st *SymbolTable) Resolve(name string) (*Symbol, bool) {

	if compilerGlobalSymbols[name] != nil {
		return compilerGlobalSymbols[name], true
	}

	// Check current scope
	sym, exists := st.Symbols[name]
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
	sym, exists := st.Symbols[name]
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
	return st.Kind
}

// GetSymbols returns all symbols in the current scope
func (st *SymbolTable) GetSymbols() map[string]*Symbol {
	return st.Symbols
}

// GetCompilerGlobalSymbols returns the compiler global symbols
func GetCompilerGlobalSymbols() map[string]*Symbol {
	return compilerGlobalSymbols
}

// Clear removes all symbols from the current scope
func (st *SymbolTable) Clear() {
	st.Symbols = make(map[string]*Symbol)
}

// EnterScope creates and returns a new child scope
func (st *SymbolTable) EnterScope(kind SCOPE_KIND) *SymbolTable {
	return NewSymbolTable(st, kind, st.Filepath)
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
	for _, sym := range st.Symbols {
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
