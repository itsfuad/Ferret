package ast

import (
	"compiler/internal/source"
)

// Node is the base interface for all AST nodes
type Node interface {
	INode()
	Loc() *source.Location
}

// Expression represents any node that produces a value
type Expression interface {
	Node
	Expr()
}

// TypeNode represents a type in the AST (for use in declarations, annotations, etc.)
// This is separate from Expression to maintain clean separation between values and types
type TypeNode interface {
	Node
	TypeExpr()
}

// Statement represents any node that performs an action
type Statement interface {
	Node
	Stmt()
}

// Decl represents a declaration (var, const, type, func)
type Decl interface {
	Node
	Decl()
}

// BlockConstruct represents block-level constructs (functions, loops, conditionals)
type BlockConstruct interface {
	Node
	Block()
}
