package ast

import (
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
)

type IntLiteral struct {
	Value int64  // Actual numeric value
	Raw   string // Original source text
	Base  int    // Number base (10, 16, 8, or 2)
	Location
}

func (i *IntLiteral) INode()                    {} // INode is a marker interface for all nodes
func (i *IntLiteral) Expr()                     {} // Expr is a marker interface for all expressions
func (i *IntLiteral) StartPos() *lexer.Position { return i.Start }
func (i *IntLiteral) EndPos() *lexer.Position   { return i.End }

type FloatLiteral struct {
	Value float64 // Actual numeric value
	Raw   string  // Original source text
	Location
}

func (f *FloatLiteral) INode()                    {} // INode is a marker interface for all nodes
func (f *FloatLiteral) Expr()                     {} // Expr is a marker interface for all expressions
func (f *FloatLiteral) StartPos() *lexer.Position { return f.Start }
func (f *FloatLiteral) EndPos() *lexer.Position   { return f.End }

type StringLiteral struct {
	Value string
	Location
}

func (s *StringLiteral) INode()                    {} // INode is a marker interface for all nodes
func (s *StringLiteral) Expr()                     {} // Expr is a marker interface for all expressions
func (s *StringLiteral) StartPos() *lexer.Position { return s.Start }
func (s *StringLiteral) EndPos() *lexer.Position   { return s.End }

type BoolLiteral struct {
	Value bool
	Location
}

func (b *BoolLiteral) INode()                    {} // INode is a marker interface for all nodes
func (b *BoolLiteral) Expr()                     {} // Expr is a marker interface for all expressions
func (b *BoolLiteral) StartPos() *lexer.Position { return b.Start }
func (b *BoolLiteral) EndPos() *lexer.Position   { return b.End }

type IndexableExpr struct {
	Indexable Expression // The expression being indexed (array, map, etc.)
	Index     Expression // The index expression
	Location
}

func (i *IndexableExpr) INode()                    {} // INode is a marker interface for all nodes
func (i *IndexableExpr) Expr()                     {} // Expr is a marker interface for all expressions
func (i *IndexableExpr) StartPos() *lexer.Position { return i.Start }
func (i *IndexableExpr) EndPos() *lexer.Position   { return i.End }

// ObjectField represents a field in an object type or literal
type ObjectField struct {
	Name  string
	Type  DataType   // nil for object literals
	Value Expression // nil for object types
	Location
}

// ObjectType represents an object type definition
type ObjectType struct {
	Fields   []ObjectField
	TypeName types.TYPE_NAME
	Location
}

func (o *ObjectType) INode()                    {} // INode is a marker interface for all nodes
func (o *ObjectType) Type() types.TYPE_NAME     { return o.TypeName }
func (o *ObjectType) StartPos() *lexer.Position { return o.Start }
func (o *ObjectType) EndPos() *lexer.Position   { return o.End }
