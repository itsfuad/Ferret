package table

import (
	"compiler/internal/frontend/ast"
	"compiler/internal/types"
	"fmt"
)

// SymbolTable holds symbols declared in a module or scope
// For now this is a placeholder - will be expanded with proper symbol resolution
type SymbolTable struct {
	parent  *SymbolTable
	symbols map[string]*Symbol
}

// Symbol represents a declared entity (variable, function, type, etc.)
type Symbol struct {
	Name     string
	Kind     SymbolKind
	Type     types.SemType // Semantic type of the symbol
	Exported bool          // Whether symbol is accessible from other modules
	Decl     ast.Node      // AST node that declared this symbol
}

// SymbolKind categorizes symbols
type SymbolKind int

const (
	SymbolVariable SymbolKind = iota
	SymbolConstant
	SymbolFunction
	SymbolType
	SymbolParameter
)

// NewSymbolTable creates a new symbol table with optional parent scope
func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		parent:  parent,
		symbols: make(map[string]*Symbol),
	}
}

// Declare adds a symbol to the table
func (st *SymbolTable) Declare(name string, symbol *Symbol) error {
	if _, exists := st.symbols[name]; exists {
		return fmt.Errorf("symbol '%s' already declared", name)
	}
	st.symbols[name] = symbol
	return nil
}

// Lookup finds a symbol in this scope or parent scopes
func (st *SymbolTable) Lookup(name string) (*Symbol, bool) {
	if sym, ok := st.symbols[name]; ok {
		return sym, true
	}
	if st.parent != nil {
		return st.parent.Lookup(name)
	}
	return nil, false
}

func (st *SymbolTable) GetSymbol(name string) (*Symbol, bool) {
	if sym, ok := st.symbols[name]; ok {
		return sym, true
	}

	return nil, false
}
