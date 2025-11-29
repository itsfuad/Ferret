package semantics

import (
	"compiler/internal/source"
	"compiler/internal/types"
	"fmt"
)

// SymbolKind represents the kind of symbol (variable, function, type, etc.)
type SymbolKind int

const (
	SymbolVariable SymbolKind = iota
	SymbolConstant
	SymbolFunction
	SymbolType
	SymbolParameter
)

func (sk SymbolKind) String() string {
	switch sk {
	case SymbolVariable:
		return "variable"
	case SymbolConstant:
		return "constant"
	case SymbolFunction:
		return "function"
	case SymbolType:
		return "type"
	case SymbolParameter:
		return "parameter"
	default:
		return "unknown"
	}
}

// Symbol represents a declared name in the program (variable, function, type, etc.)
type Symbol struct {
	Name     string
	Kind     SymbolKind
	Type     types.SemType   // Semantic type of the symbol (TypeUnknown initially)
	Location source.Location // Where the symbol is declared
}

// SymbolTable represents a lexical scope with a chain to parent scopes
type SymbolTable struct {
	Parent  *SymbolTable
	Symbols map[string]*Symbol
}

// NewSymbolTable creates a new symbol table with the given parent
func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		Parent:  parent,
		Symbols: make(map[string]*Symbol),
	}
}

// Insert adds a symbol to the current scope
// Returns an error if the symbol already exists in this scope
func (st *SymbolTable) Insert(sym *Symbol) error {
	if _, exists := st.Symbols[sym.Name]; exists {
		return fmt.Errorf("symbol '%s' already declared in this scope", sym.Name)
	}
	st.Symbols[sym.Name] = sym
	return nil
}

// Lookup searches for a symbol by name, walking up the scope chain
func (st *SymbolTable) Lookup(name string) (*Symbol, bool) {
	if sym, ok := st.Symbols[name]; ok {
		return sym, true
	}
	if st.Parent != nil {
		return st.Parent.Lookup(name)
	}
	return nil, false
}

// LookupLocal searches for a symbol only in the current scope (no parent lookup)
func (st *SymbolTable) LookupLocal(name string) (*Symbol, bool) {
	sym, ok := st.Symbols[name]
	return sym, ok
}

// Import represents an import statement with resolution info
type Import struct {
	Path         string          // Original import path from source
	Alias        string          // Optional alias (empty if none)
	Location     source.Location // Where the import appears
	ResolvedPath string          // Normalized/resolved path (filled by resolver)
	Module       interface{}     // Pointer to resolved Module (filled by resolver, using interface{} to avoid circular dep)
	Symbols      *SymbolTable    // Imported module's symbol table (filled by resolver)
}
