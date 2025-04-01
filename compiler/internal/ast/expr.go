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

func (b *BinaryExpr) INode()                {} // INode is a marker interface for all nodes
func (b *BinaryExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (b *BinaryExpr) Loc() *source.Location { return &b.Location }

type UnaryExpr struct {
	Operator lexer.Token
	Operand  Expression
	source.Location
}

func (u *UnaryExpr) INode()                {} // INode is a marker interface for all nodes
func (u *UnaryExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (u *UnaryExpr) Loc() *source.Location { return &u.Location }

type PrefixExpr struct {
	Operator lexer.Token // The operator token (++, --)
	Operand  Expression
	source.Location
}

func (p *PrefixExpr) INode()                {} // INode is a marker interface for all nodes
func (p *PrefixExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (p *PrefixExpr) Loc() *source.Location { return &p.Location }

type PostfixExpr struct {
	Operand  Expression
	Operator lexer.Token // The operator token (++, --)
	source.Location
}

func (p *PostfixExpr) INode()                {} // INode is a marker interface for all nodes
func (p *PostfixExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (p *PostfixExpr) Loc() *source.Location { return &p.Location }

type IdentifierExpr struct {
	Name string
	source.Location
}

func (i *IdentifierExpr) INode()                {} // INode is a marker interface for all nodes
func (i *IdentifierExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (i *IdentifierExpr) LValue()               {} // LValue is a marker interface for all lvalues
func (i *IdentifierExpr) Loc() *source.Location { return &i.Location }

type ArrayLiteralExpr struct {
	Elements []Expression
	source.Location
}

func (a *ArrayLiteralExpr) INode()                {} // INode is a marker interface for all nodes
func (a *ArrayLiteralExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (a *ArrayLiteralExpr) Loc() *source.Location { return &a.Location }

// StructLiteralExpr represents a struct literal expression like Point{x: 10, y: 20}
type StructLiteralExpr struct {
	TypeName    IdentifierExpr
	Fields      []StructField
	IsAnonymous bool
	source.Location
}

func (s *StructLiteralExpr) INode()                {} // INode is a marker interface for all nodes
func (s *StructLiteralExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (s *StructLiteralExpr) Loc() *source.Location { return &s.Location }

type FunctionLiteral struct {
	Params     []Parameter
	ReturnType []DataType
	Body       *BlockConstruct
	source.Location
}

func (f *FunctionLiteral) INode()                {} // INode is a marker interface for all nodes
func (f *FunctionLiteral) Expr()                 {} // Expr is a marker interface for all expressions
func (f *FunctionLiteral) Loc() *source.Location { return &f.Location }

// FunctionCallExpr represents a function call expression
type FunctionCallExpr struct {
	Caller    Expression   // The function being called (can be an identifier or other expression)
	Arguments []Expression // The arguments passed to the function
	source.Location
}

func (f *FunctionCallExpr) INode()                {} // INode is a marker interface for all nodes
func (f *FunctionCallExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (f *FunctionCallExpr) Loc() *source.Location { return &f.Location }

// FieldAccessExpr represents a field access expression like struct.field
type FieldAccessExpr struct {
	Object Expression      // The struct being accessed
	Field  *IdentifierExpr // The field being accessed
	source.Location
}

func (f *FieldAccessExpr) INode()                {} // INode is a marker interface for all nodes
func (f *FieldAccessExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (f *FieldAccessExpr) LValue()               {} // LValue is a marker interface for all lvalues
func (f *FieldAccessExpr) Loc() *source.Location { return &f.Location }

// ScopeResolutionExpr represents a scope resolution expression like fmt::Println
type ScopeResolutionExpr struct {
	Module     *IdentifierExpr
	Identifier *IdentifierExpr
	source.Location
}

func (s *ScopeResolutionExpr) INode()                {} // INode is a marker interface for all nodes
func (s *ScopeResolutionExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (s *ScopeResolutionExpr) Loc() *source.Location { return &s.Location }
