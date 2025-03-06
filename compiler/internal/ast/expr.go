package ast

import "ferret/compiler/internal/lexer"

// Basic expression nodes
type BinaryExpr struct {
	Location
	Left     Expression
	Operator lexer.Token
	Right    Expression
}

func (b *BinaryExpr) INode()                   {} // INode is a marker interface for all nodes
func (b *BinaryExpr) isExpr()                  {} // isExpr is a marker interface for all expressions
func (b *BinaryExpr) StartPos() lexer.Position { return b.Start }
func (b *BinaryExpr) EndPos() lexer.Position   { return b.End }

type UnaryExpr struct {
	Location
	Operator lexer.Token
	Right    Expression
}

func (u *UnaryExpr) INode()                   {} // INode is a marker interface for all nodes
func (u *UnaryExpr) isExpr()                  {} // isExpr is a marker interface for all expressions
func (u *UnaryExpr) StartPos() lexer.Position { return u.Start }
func (u *UnaryExpr) EndPos() lexer.Position   { return u.End }

type LiteralExpr struct {
	Location
	Value lexer.Token
}

func (l *LiteralExpr) INode()                   {} // INode is a marker interface for all nodes
func (l *LiteralExpr) isExpr()                  {} // isExpr is a marker interface for all expressions
func (l *LiteralExpr) StartPos() lexer.Position { return l.Start }
func (l *LiteralExpr) EndPos() lexer.Position   { return l.End }

type IdentifierExpr struct {
	Location
	Name lexer.Token
}

func (i *IdentifierExpr) INode()                   {} // INode is a marker interface for all nodes
func (i *IdentifierExpr) isExpr()                  {} // isExpr is a marker interface for all expressions
func (i *IdentifierExpr) StartPos() lexer.Position { return i.Start }
func (i *IdentifierExpr) EndPos() lexer.Position   { return i.End }
