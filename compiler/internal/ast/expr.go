package ast

import "ferret/compiler/internal/lexer"

// Basic expression nodes
type BinaryExpr struct {
	Left     Expression
	Operator lexer.Token
	Right    Expression
	Location
}

func (b *BinaryExpr) INode()                    {} // INode is a marker interface for all nodes
func (b *BinaryExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (b *BinaryExpr) StartPos() *lexer.Position { return b.Start }
func (b *BinaryExpr) EndPos() *lexer.Position   { return b.End }

type UnaryExpr struct {
	Operator lexer.Token
	Operand  Expression
	Location
}

func (u *UnaryExpr) INode()                    {} // INode is a marker interface for all nodes
func (u *UnaryExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (u *UnaryExpr) StartPos() *lexer.Position { return u.Start }
func (u *UnaryExpr) EndPos() *lexer.Position   { return u.End }

type PrefixExpr struct {
	Operator lexer.Token // The operator token (++, --)
	Operand  Expression
	Location
}

func (p *PrefixExpr) INode()                    {} // INode is a marker interface for all nodes
func (p *PrefixExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (p *PrefixExpr) StartPos() *lexer.Position { return p.Start }
func (p *PrefixExpr) EndPos() *lexer.Position   { return p.End }

type PostfixExpr struct {
	Operand  Expression
	Operator lexer.Token // The operator token (++, --)
	Location
}

func (p *PostfixExpr) INode()                    {} // INode is a marker interface for all nodes
func (p *PostfixExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (p *PostfixExpr) StartPos() *lexer.Position { return p.Start }
func (p *PostfixExpr) EndPos() *lexer.Position   { return p.End }

type IdentifierExpr struct {
	Name string
	Location
}

func (i *IdentifierExpr) INode()                    {} // INode is a marker interface for all nodes
func (i *IdentifierExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (i *IdentifierExpr) LValue()                   {} // LValue is a marker interface for all lvalues
func (i *IdentifierExpr) StartPos() *lexer.Position { return i.Start }
func (i *IdentifierExpr) EndPos() *lexer.Position   { return i.End }

type ArrayLiteralExpr struct {
	Elements []Expression
	Location
}

func (a *ArrayLiteralExpr) INode()                    {} // INode is a marker interface for all nodes
func (a *ArrayLiteralExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (a *ArrayLiteralExpr) StartPos() *lexer.Position { return a.Start }
func (a *ArrayLiteralExpr) EndPos() *lexer.Position   { return a.End }

// StructFieldInit represents a field initialization in a struct literal
type StructFieldInit struct {
	Field IdentifierExpr
	Value Expression
	Location
}

// StructLiteralExpr represents a struct literal expression like Point{x: 10, y: 20}
type StructLiteralExpr struct {
	TypeName    IdentifierExpr
	Fields      []StructFieldInit
	IsAnonymous bool
	Location
}

func (s *StructLiteralExpr) INode()                    {} // INode is a marker interface for all nodes
func (s *StructLiteralExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (s *StructLiteralExpr) StartPos() *lexer.Position { return s.Start }
func (s *StructLiteralExpr) EndPos() *lexer.Position   { return s.End }

// FunctionDeclExpr represents both named and anonymous function declarations
type FunctionDeclExpr struct {
	Identifier  IdentifierExpr
	Params      []IdentifierExpr // Parameter names
	ParamTypes  []DataType       // Parameter types
	ReturnType  DataType         // Return type (can be nil for void functions)
	Body        *BlockStmt       // Function body
	IsAnonymous bool             // Whether this is an anonymous function
	Location
}

func (f *FunctionDeclExpr) INode()                    {} // INode is a marker interface for all nodes
func (f *FunctionDeclExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (f *FunctionDeclExpr) StartPos() *lexer.Position { return f.Start }
func (f *FunctionDeclExpr) EndPos() *lexer.Position   { return f.End }
