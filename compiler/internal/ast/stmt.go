package ast

import "ferret/compiler/internal/lexer"

// Statement nodes
type VarDeclStmt struct {
	Location
	Name        lexer.Token
	Type        lexer.Token // Can be nil for type inference
	Initializer Expression
}

func (v *VarDeclStmt) INode()                   {} // INode is a marker interface for all nodes
func (v *VarDeclStmt) isStmt()                  {} // isStmt is a marker interface for all statements
func (v *VarDeclStmt) StartPos() lexer.Position { return v.Start }
func (v *VarDeclStmt) EndPos() lexer.Position   { return v.End }

type ExpressionStmt struct {
	Location
	Expression Expression
}

func (e *ExpressionStmt) INode()                   {} // INode is a marker interface for all nodes
func (e *ExpressionStmt) isStmt()                  {} // isStmt is a marker interface for all statements
func (e *ExpressionStmt) StartPos() lexer.Position { return e.Start }
func (e *ExpressionStmt) EndPos() lexer.Position   { return e.End }
