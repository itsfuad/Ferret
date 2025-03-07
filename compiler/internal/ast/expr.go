package ast

import "ferret/compiler/internal/lexer"

// Basic expression nodes
type BinaryExpr struct {
	Left     Expression
	Operator lexer.Token
	Right    Expression
	Location
}

func (b *BinaryExpr) INode() {} // INode is a marker interface for all nodes
func (b *BinaryExpr) Expr() {} // Expr is a marker interface for all expressions
func (b *BinaryExpr) StartPos() lexer.Position { return b.Start }
func (b *BinaryExpr) EndPos() lexer.Position   { return b.End }

type UnaryExpr struct {
	Operator lexer.Token
	Operand    Expression
	Location
}

func (u *UnaryExpr) INode() {} // INode is a marker interface for all nodes
func (u *UnaryExpr) Expr() {} // Expr is a marker interface for all expressions
func (u *UnaryExpr) StartPos() lexer.Position { return u.Start }
func (u *UnaryExpr) EndPos() lexer.Position   { return u.End }

type IdentifierExpr struct {
	Name lexer.Token
	Location
}

func (i *IdentifierExpr) INode() {} // INode is a marker interface for all nodes
func (i *IdentifierExpr) Expr() {} // Expr is a marker interface for all expressions
func (i *IdentifierExpr) StartPos() lexer.Position { return i.Start }
func (i *IdentifierExpr) EndPos() lexer.Position   { return i.End }
