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

type LValue interface {
	Node
	LValue()
}

type ExpressionList []Expression

func (el ExpressionList) StartPos() lexer.Position {
	return el[0].StartPos()
}

func (el ExpressionList) EndPos() lexer.Position {
	return el[len(el)-1].EndPos()
}

func (el ExpressionList) INode() {} // INode is a marker interface for all nodes

// Statement represents any node that doesn't produce a value
type Statement interface {
	Node
	Stmt()
}
