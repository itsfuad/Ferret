package ast

import "ferret/compiler/internal/source"

type IntLiteral struct {
	Value int64  // Actual numeric value
	Raw   string // Original source text
	Base  int    // Number base (10, 16, 8, or 2)
	source.Location
}

func (i *IntLiteral) INode()                {} // INode is a marker interface for all nodes
func (i *IntLiteral) Expr()                 {} // Expr is a marker interface for all expressions
func (i *IntLiteral) Loc() *source.Location { return &i.Location }

type FloatLiteral struct {
	Value float64 // Actual numeric value
	Raw   string  // Original source text
	source.Location
}

func (f *FloatLiteral) INode()                {} // INode is a marker interface for all nodes
func (f *FloatLiteral) Expr()                 {} // Expr is a marker interface for all expressions
func (f *FloatLiteral) Loc() *source.Location { return &f.Location }

type StringLiteral struct {
	Value string
	source.Location
}

func (s *StringLiteral) INode()                {} // INode is a marker interface for all nodes
func (s *StringLiteral) Expr()                 {} // Expr is a marker interface for all expressions
func (s *StringLiteral) Loc() *source.Location { return &s.Location }

type BoolLiteral struct {
	Value bool
	source.Location
}

func (b *BoolLiteral) INode()                {} // INode is a marker interface for all nodes
func (b *BoolLiteral) Expr()                 {} // Expr is a marker interface for all expressions
func (b *BoolLiteral) Loc() *source.Location { return &b.Location }

type IndexableExpr struct {
	Indexable Expression // The expression being indexed (array, map, etc.)
	Index     Expression // The index expression
	source.Location
}

func (i *IndexableExpr) INode()                {} // INode is a marker interface for all nodes
func (i *IndexableExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (i *IndexableExpr) Loc() *source.Location { return &i.Location }
