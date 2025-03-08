package ast

import "ferret/compiler/internal/lexer"

// Statement nodes
type VarDeclStmt struct {
	Variables []*VariableDecl
	Location
}

func (v *VarDeclStmt) INode()                   {} // INode is a marker interface for all nodes
func (v *VarDeclStmt) Stmt()                    {} // Stmt is a marker interface for all statements
func (v *VarDeclStmt) StartPos() lexer.Position { return v.Start }
func (v *VarDeclStmt) EndPos() lexer.Position   { return v.End }

type VariableDecl struct {
	IsConst     bool
	Identifier  *IdentifierExpr
	Type        DataType
	Initializer Expression
	Location
}

func (v *VariableDecl) INode()                   {} // INode is a marker interface for all nodes
func (v *VariableDecl) Expr()                    {} // Expr is a marker interface for all expressions
func (v *VariableDecl) StartPos() lexer.Position { return v.Start }
func (v *VariableDecl) EndPos() lexer.Position   { return v.End }

type AssignmentStmt struct {
	Left  ExpressionList
	Right ExpressionList
	Location
}

func (a *AssignmentStmt) INode()                   {} // INode is a marker interface for all nodes
func (a *AssignmentStmt) Stmt()                    {} // Stmt is a marker interface for all statements
func (a *AssignmentStmt) StartPos() lexer.Position { return a.Start }
func (a *AssignmentStmt) EndPos() lexer.Position   { return a.End }
