package ast

import "ferret/compiler/internal/source"

// Statement nodes
type VarDeclStmt struct {
	Variables    []*VariableToDeclare
	Initializers []Expression
	IsConst      bool
	source.Location
}

func (v *VarDeclStmt) INode()                     {} // INode is a marker interface for all nodes
func (v *VarDeclStmt) Stmt()                      {} // Stmt is a marker interface for all statements
func (v *VarDeclStmt) StartPos() *source.Position { return v.Start }
func (v *VarDeclStmt) EndPos() *source.Position   { return v.End }

type VariableToDeclare struct {
	Identifier   *IdentifierExpr
	ExplicitType DataType
}

type AssignmentStmt struct {
	Left  ExpressionList
	Right ExpressionList
	source.Location
}

func (a *AssignmentStmt) INode()                     {} // INode is a marker interface for all nodes
func (a *AssignmentStmt) Stmt()                      {} // Stmt is a marker interface for all statements
func (a *AssignmentStmt) StartPos() *source.Position { return a.Start }
func (a *AssignmentStmt) EndPos() *source.Position   { return a.End }

// TypeDeclStmt represents a type declaration statement
type TypeDeclStmt struct {
	Alias    *IdentifierExpr // The name of the type
	BaseType DataType        // The underlying type
	source.Location
}

func (t *TypeDeclStmt) INode()                     {} // INode is a marker interface for all nodes
func (t *TypeDeclStmt) Stmt()                      {} // Stmt is a marker interface for all statements
func (t *TypeDeclStmt) StartPos() *source.Position { return t.Start }
func (t *TypeDeclStmt) EndPos() *source.Position   { return t.End }

// ReturnStmt represents a return statement
type ReturnStmt struct {
	Values ExpressionList
	source.Location
}

func (r *ReturnStmt) INode()                     {} // INode is a marker interface for all nodes
func (r *ReturnStmt) Stmt()                      {} // Stmt is a marker method for statements
func (r *ReturnStmt) StartPos() *source.Position { return r.Start }
func (r *ReturnStmt) EndPos() *source.Position   { return r.End }

// PackageDeclStmt represents a package declaration
type PackageDeclStmt struct {
	Package *IdentifierExpr
	source.Location
}

func (p *PackageDeclStmt) INode()                     {} // INode is a marker interface for all nodes
func (p *PackageDeclStmt) Stmt()                      {} // Stmt is a marker interface for all statements
func (p *PackageDeclStmt) StartPos() *source.Position { return p.Start }
func (p *PackageDeclStmt) EndPos() *source.Position   { return p.End }

// ImportStmt represents an import statement
type ImportStmt struct {
	Import *StringLiteral
	source.Location
}

func (i *ImportStmt) INode()                     {} // INode is a marker interface for all nodes
func (i *ImportStmt) Stmt()                      {} // Stmt is a marker interface for all statements
func (i *ImportStmt) StartPos() *source.Position { return i.Start }
func (i *ImportStmt) EndPos() *source.Position   { return i.End }
