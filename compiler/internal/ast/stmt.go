package ast

import "ferret/compiler/internal/lexer"

// Statement nodes
type VarDeclStmt struct {
	Variables    []*VariableToDeclare
	Initializers []Expression
	IsConst      bool
	Location
}

func (v *VarDeclStmt) INode()                    {} // INode is a marker interface for all nodes
func (v *VarDeclStmt) Stmt()                     {} // Stmt is a marker interface for all statements
func (v *VarDeclStmt) StartPos() *lexer.Position { return v.Start }
func (v *VarDeclStmt) EndPos() *lexer.Position   { return v.End }

type VariableToDeclare struct {
	Identifier   *IdentifierExpr
	ExplicitType DataType
}

type AssignmentStmt struct {
	Left  ExpressionList
	Right ExpressionList
	Location
}

func (a *AssignmentStmt) INode()                    {} // INode is a marker interface for all nodes
func (a *AssignmentStmt) Stmt()                     {} // Stmt is a marker interface for all statements
func (a *AssignmentStmt) StartPos() *lexer.Position { return a.Start }
func (a *AssignmentStmt) EndPos() *lexer.Position   { return a.End }

// TypeDeclStmt represents a type declaration statement
type TypeDeclStmt struct {
	Alias    *IdentifierExpr // The name of the type
	BaseType DataType        // The underlying type
	Location
}

func (t *TypeDeclStmt) INode()                    {} // INode is a marker interface for all nodes
func (t *TypeDeclStmt) Stmt()                     {} // Stmt is a marker interface for all statements
func (t *TypeDeclStmt) StartPos() *lexer.Position { return t.Start }
func (t *TypeDeclStmt) EndPos() *lexer.Position   { return t.End }

// ReturnStmt represents a return statement
type ReturnStmt struct {
	Values ExpressionList
	Location
}

func (r *ReturnStmt) INode()                    {} // INode is a marker interface for all nodes
func (r *ReturnStmt) Stmt()                     {} // Stmt is a marker method for statements
func (r *ReturnStmt) StartPos() *lexer.Position { return r.Start }
func (r *ReturnStmt) EndPos() *lexer.Position   { return r.End }

// IfStmt represents an if statement with optional else and else-if branches
type IfStmt struct {
	Condition Expression
	Body      *BlockConstruct
	Alternative Node
	Location
}

func (i *IfStmt) INode()                    {} // INode is a marker interface for all nodes
func (i *IfStmt) Stmt()                     {} // Stmt is a marker interface for all statements
func (i *IfStmt) StartPos() *lexer.Position { return i.Start }
func (i *IfStmt) EndPos() *lexer.Position   { return i.End }