package ast

import "ferret/compiler/internal/lexer"

// Statement nodes
type VarDeclStmt struct {
	Variables []VariableDecl
	Location
}

func (v *VarDeclStmt) INode() {} // INode is a marker interface for all nodes
func (v *VarDeclStmt) Stmt() {} // Stmt is a marker interface for all statements
func (v *VarDeclStmt) StartPos() lexer.Position { return v.Start }
func (v *VarDeclStmt) EndPos() lexer.Position   { return v.End }

type VariableDecl struct {
	Identifier IdentifierExpr
	Type DataType
	Initializer Expression
	Location
}

func (v *VariableDecl) INode() {} // INode is a marker interface for all nodes
func (v *VariableDecl) Expr() {} // Expr is a marker interface for all expressions
func (v *VariableDecl) StartPos() lexer.Position { return v.Start }
func (v *VariableDecl) EndPos() lexer.Position   { return v.End }
