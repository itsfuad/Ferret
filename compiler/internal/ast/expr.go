package ast

import (
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/source"
)

// Basic expression nodes
type BinaryExpr struct {
	Left     Expression
	Operator lexer.Token
	Right    Expression
	source.Location
}

func (b *BinaryExpr) INode()                     {} // INode is a marker interface for all nodes
func (b *BinaryExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (b *BinaryExpr) StartPos() *source.Position { return b.Start }
func (b *BinaryExpr) EndPos() *source.Position   { return b.End }

type UnaryExpr struct {
	Operator lexer.Token
	Operand  Expression
	source.Location
}

func (u *UnaryExpr) INode()                     {} // INode is a marker interface for all nodes
func (u *UnaryExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (u *UnaryExpr) StartPos() *source.Position { return u.Start }
func (u *UnaryExpr) EndPos() *source.Position   { return u.End }

type PrefixExpr struct {
	Operator lexer.Token // The operator token (++, --)
	Operand  Expression
	source.Location
}

func (p *PrefixExpr) INode()                     {} // INode is a marker interface for all nodes
func (p *PrefixExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (p *PrefixExpr) StartPos() *source.Position { return p.Start }
func (p *PrefixExpr) EndPos() *source.Position   { return p.End }

type PostfixExpr struct {
	Operand  Expression
	Operator lexer.Token // The operator token (++, --)
	source.Location
}

func (p *PostfixExpr) INode()                     {} // INode is a marker interface for all nodes
func (p *PostfixExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (p *PostfixExpr) StartPos() *source.Position { return p.Start }
func (p *PostfixExpr) EndPos() *source.Position   { return p.End }

type IdentifierExpr struct {
	Name string
	source.Location
}

func (i *IdentifierExpr) INode()                     {} // INode is a marker interface for all nodes
func (i *IdentifierExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (i *IdentifierExpr) LValue()                    {} // LValue is a marker interface for all lvalues
func (i *IdentifierExpr) StartPos() *source.Position { return i.Start }
func (i *IdentifierExpr) EndPos() *source.Position   { return i.End }

type ArrayLiteralExpr struct {
	Elements []Expression
	source.Location
}

func (a *ArrayLiteralExpr) INode()                     {} // INode is a marker interface for all nodes
func (a *ArrayLiteralExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (a *ArrayLiteralExpr) StartPos() *source.Position { return a.Start }
func (a *ArrayLiteralExpr) EndPos() *source.Position   { return a.End }

// StructLiteralExpr represents a struct literal expression like Point{x: 10, y: 20}
type StructLiteralExpr struct {
	TypeName    IdentifierExpr
	Fields      []StructField
	IsAnonymous bool
	source.Location
}

func (s *StructLiteralExpr) INode()                     {} // INode is a marker interface for all nodes
func (s *StructLiteralExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (s *StructLiteralExpr) StartPos() *source.Position { return s.Start }
func (s *StructLiteralExpr) EndPos() *source.Position   { return s.End }

type FunctionLiteral struct {
	Params     []Parameter
	ReturnType []DataType
	Body       *BlockConstruct
	source.Location
}

func (f *FunctionLiteral) INode()                     {} // INode is a marker interface for all nodes
func (f *FunctionLiteral) Expr()                      {} // Expr is a marker interface for all expressions
func (f *FunctionLiteral) StartPos() *source.Position { return f.Start }
func (f *FunctionLiteral) EndPos() *source.Position   { return f.End }

// FunctionCallExpr represents a function call expression
type FunctionCallExpr struct {
	Caller    Expression   // The function being called (can be an identifier or other expression)
	Arguments []Expression // The arguments passed to the function
	source.Location
}

func (f *FunctionCallExpr) INode()                     {} // INode is a marker interface for all nodes
func (f *FunctionCallExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (f *FunctionCallExpr) StartPos() *source.Position { return f.Start }
func (f *FunctionCallExpr) EndPos() *source.Position   { return f.End }

// FieldAccessExpr represents a field access expression like struct.field
type FieldAccessExpr struct {
	Object Expression      // The struct being accessed
	Field  *IdentifierExpr // The field being accessed
	source.Location
}

func (f *FieldAccessExpr) INode()                     {} // INode is a marker interface for all nodes
func (f *FieldAccessExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (f *FieldAccessExpr) LValue()                    {} // LValue is a marker interface for all lvalues
func (f *FieldAccessExpr) StartPos() *source.Position { return f.Start }
func (f *FieldAccessExpr) EndPos() *source.Position   { return f.End }

// ScopeResolutionExpr represents a scope resolution expression like fmt::Println
type ScopeResolutionExpr struct {
	Module     *IdentifierExpr
	Identifier *IdentifierExpr
	source.Location
}

func (s *ScopeResolutionExpr) INode()                     {} // INode is a marker interface for all nodes
func (s *ScopeResolutionExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (s *ScopeResolutionExpr) StartPos() *source.Position { return s.Start }
func (s *ScopeResolutionExpr) EndPos() *source.Position   { return s.End }
