package table

import (
	"compiler/internal/semantics/symbols"
	"fmt"
)

// SymbolTable holds symbols declared in a module or scope
// For now this is a placeholder - will be expanded with proper symbol resolution
type SymbolTable struct {
	parent  *SymbolTable
	symbols map[string]*symbols.Symbol
}

func (st SymbolTable) Scope() {}

// NewSymbolTable creates a new symbol table with optional parent scope
func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		parent:  parent,
		symbols: make(map[string]*symbols.Symbol),
	}
}

// Declare adds a symbol to the table
func (st *SymbolTable) Declare(name string, symbol *symbols.Symbol) error {
	if _, exists := st.symbols[name]; exists {
		return fmt.Errorf("symbol '%s' already declared", name)
	}
	st.symbols[name] = symbol
	return nil
}

// Lookup finds a symbol in this scope or parent scopes
func (st *SymbolTable) Lookup(name string) (*symbols.Symbol, bool) {
	if sym, ok := st.symbols[name]; ok {
		return sym, true
	}
	if st.parent != nil {
		return st.parent.Lookup(name)
	}
	return nil, false
}

func (st *SymbolTable) GetSymbol(name string) (*symbols.Symbol, bool) {
	if sym, ok := st.symbols[name]; ok {
		return sym, true
	}

	return nil, false
}
