package ast

import "ferret/compiler/internal/lexer"

// BlockStmt represents a block of statements
type Block struct {
	Nodes []Node
	Location
}

func (b *Block) INode()                    {} // INode is a marker interface for all nodes
func (b *Block) Block()                    {} // Block is a marker method for statements
func (b *Block) StartPos() *lexer.Position { return b.Start }
func (b *Block) EndPos() *lexer.Position   { return b.End }

// FunctionDeclExpr represents both named and anonymous function declarations
type FunctionDeclExpr struct {
	Identifier IdentifierExpr
	Function   *FunctionLiteral // Function literal
	Location
}

func (f *FunctionDeclExpr) INode()                    {} // INode is a marker interface for all nodes
func (f *FunctionDeclExpr) Block()                    {} // Block is a marker interface for all expressions
func (f *FunctionDeclExpr) StartPos() *lexer.Position { return f.Start }
func (f *FunctionDeclExpr) EndPos() *lexer.Position   { return f.End }
