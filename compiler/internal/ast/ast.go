package ast

import "ferret/compiler/internal/lexer"

type NODE_TYPE int

type Node interface {
	INode()
	StartPos() lexer.Position
	EndPos() lexer.Position
}

type Location struct {
	Start lexer.Position
	End   lexer.Position
}

const (
	BINARY_EXPR NODE_TYPE = iota
	UNARY_EXPR
	LITERAL_EXPR
	GROUPING_EXPR
	IDENTIFIER_EXPR
	VARIABLE_DECL
	FUNCTION_DECL
)

// Expression represents any node that produces a value
type Expression interface {
	Node
	Expr()
}

// Statement represents any node that doesn't produce a value
type Statement interface {
	Node
	Stmt()
}