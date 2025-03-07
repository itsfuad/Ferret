package ast

import "ferret/compiler/internal/lexer"

type IntLiteral struct {
	Value string
	Location
}

func (i *IntLiteral) INode()                   {} // INode is a marker interface for all nodes
func (i *IntLiteral) Expr()                    {} // Expr is a marker interface for all expressions
func (i *IntLiteral) StartPos() lexer.Position { return i.Start }
func (i *IntLiteral) EndPos() lexer.Position   { return i.End }

type FloatLiteral struct {
	Value string
	Location
}

func (f *FloatLiteral) INode()                   {} // INode is a marker interface for all nodes
func (f *FloatLiteral) Expr()                    {} // Expr is a marker interface for all expressions
func (f *FloatLiteral) StartPos() lexer.Position { return f.Start }
func (f *FloatLiteral) EndPos() lexer.Position   { return f.End }

type StringLiteral struct {
	Value string
	Location
}

func (s *StringLiteral) INode()                   {} // INode is a marker interface for all nodes
func (s *StringLiteral) Expr()                    {} // Expr is a marker interface for all expressions
func (s *StringLiteral) StartPos() lexer.Position { return s.Start }
func (s *StringLiteral) EndPos() lexer.Position   { return s.End }
