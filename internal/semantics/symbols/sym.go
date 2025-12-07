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

// ConstValue forward declaration to avoid import cycle
// Actual implementation is in internal/semantics/consteval
type ConstValue interface {
	IsConstant() bool
	String() string
}

// Symbol represents a declared entity (variable, function, type, etc.)
// Scopes are stored in AST nodes (FuncDecl.Scope, Block.Scope, etc.), not here.
type Symbol struct {
	Name          string
	Kind          SymbolKind
	Type          types.SemType          // Semantic type of the symbol
	Exported      bool                   // Whether symbol is accessible from other modules
	Decl          ast.Node               // AST node that declared this symbol
	DeclaredScope SymbolTable            // Scope where this symbol was declared
	Methods       map[string]*MethodInfo // Methods attached to this named type (only for SymbolType)
	ConstValue    ConstValue             // Compile-time known value (for constants and const variables)
}

// MethodInfo stores information about a method attached to a named type
type MethodInfo struct {
	Name     string
	FuncType *types.FunctionType
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
