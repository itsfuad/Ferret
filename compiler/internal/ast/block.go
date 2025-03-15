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

// FunctionDecl represents both named and anonymous function declarations
type FunctionDecl struct {
	Identifier IdentifierExpr
	Function   *FunctionLiteral // Function literal
	Location
}

func (f *FunctionDecl) INode()                    {} // INode is a marker interface for all nodes
func (f *FunctionDecl) Block()                    {} // Block is a marker interface for all expressions
func (f *FunctionDecl) StartPos() *lexer.Position { return f.Start }
func (f *FunctionDecl) EndPos() *lexer.Position   { return f.End }

// IfStmt represents an if statement with optional else and else-if branches
type IfStmt struct {
	Condition   Expression
	Body        *BlockConstruct
	Alternative Node
	Location
}

func (i *IfStmt) INode()                    {} // INode is a marker interface for all nodes
func (i *IfStmt) Block()                    {} // Block is a marker interface for all statements
func (i *IfStmt) StartPos() *lexer.Position { return i.Start }
func (i *IfStmt) EndPos() *lexer.Position   { return i.End }

// MethodDecl represents a method declaration
type MethodDecl struct {
	Method   IdentifierExpr
	Receiver *Parameter // Receiver parameter: e.g. in `fn (t *T) M(n int)`, `t` is the receiver
	IsRRef   bool       // Whether the receiver is a reference
	Function *FunctionLiteral
	Location
}

func (m *MethodDecl) INode()                    {} // INode is a marker interface for all nodes
func (m *MethodDecl) Block()                    {} // Block is a marker interface for all statements
func (m *MethodDecl) StartPos() *lexer.Position { return m.Start }
func (m *MethodDecl) EndPos() *lexer.Position   { return m.End }
