package symbols

import (
	"compiler/internal/frontend/ast"
	"compiler/internal/types"
)

// Forward declaration to avoid import cycle
type SymbolTable interface {
	Declare(name string, symbol *Symbol) error
	Lookup(name string) (*Symbol, bool)
	GetSymbol(name string) (*Symbol, bool)
}

// Symbol represents a declared entity (variable, function, type, etc.)
type Symbol struct {
	Name     string
	Kind     SymbolKind
	Type     types.SemType // Semantic type of the symbol
	Exported bool          // Whether symbol is accessible from other modules
	Decl     ast.Node      // AST node that declared this symbol
	Scope    SymbolTable   // For types: holds methods and associated functions
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
