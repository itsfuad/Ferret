package ast

import "ferret/compiler/internal/source"

type IntLiteral struct {
	Value int64  // Actual numeric value
	Raw   string // Original source text
	Base  int    // Number base (10, 16, 8, or 2)
	source.Location
}

func (i *IntLiteral) INode()                     {} // INode is a marker interface for all nodes
func (i *IntLiteral) Expr()                      {} // Expr is a marker interface for all expressions
func (i *IntLiteral) StartPos() *source.Position { return i.Start }
func (i *IntLiteral) EndPos() *source.Position   { return i.End }

type FloatLiteral struct {
	Value float64 // Actual numeric value
	Raw   string  // Original source text
	source.Location
}

func (f *FloatLiteral) INode()                     {} // INode is a marker interface for all nodes
func (f *FloatLiteral) Expr()                      {} // Expr is a marker interface for all expressions
func (f *FloatLiteral) StartPos() *source.Position { return f.Start }
func (f *FloatLiteral) EndPos() *source.Position   { return f.End }

type StringLiteral struct {
	Value string
	source.Location
}

func (s *StringLiteral) INode()                     {} // INode is a marker interface for all nodes
func (s *StringLiteral) Expr()                      {} // Expr is a marker interface for all expressions
func (s *StringLiteral) StartPos() *source.Position { return s.Start }
func (s *StringLiteral) EndPos() *source.Position   { return s.End }

type BoolLiteral struct {
	Value bool
	source.Location
}

func (b *BoolLiteral) INode()                     {} // INode is a marker interface for all nodes
func (b *BoolLiteral) Expr()                      {} // Expr is a marker interface for all expressions
func (b *BoolLiteral) StartPos() *source.Position { return b.Start }
func (b *BoolLiteral) EndPos() *source.Position   { return b.End }

type IndexableExpr struct {
	Indexable Expression // The expression being indexed (array, map, etc.)
	Index     Expression // The index expression
	source.Location
}

func (i *IndexableExpr) INode()                     {} // INode is a marker interface for all nodes
func (i *IndexableExpr) Expr()                      {} // Expr is a marker interface for all expressions
func (i *IndexableExpr) StartPos() *source.Position { return i.Start }
func (i *IndexableExpr) EndPos() *source.Position   { return i.End }
