package symbols

import (
	"compiler/internal/frontend/ast"
	"compiler/internal/types"
)

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