package ast

import "ferret/compiler/internal/source"

// Statement nodes
type VarDeclStmt struct {
	Variables    []*VariableToDeclare
	Initializers []Expression
	IsConst      bool
	source.Location
}

func (v *VarDeclStmt) INode() Node           { return v }
func (v *VarDeclStmt) Stmt()                 {} // Stmt is a marker interface for all statements
func (v *VarDeclStmt) Loc() *source.Location { return &v.Location }

type VariableToDeclare struct {
	Identifier   *IdentifierExpr
	ExplicitType DataType
}

type AssignmentStmt struct {
	Left  ExpressionList
	Right ExpressionList
	source.Location
}

func (a *AssignmentStmt) INode() Node           { return a }
func (a *AssignmentStmt) Stmt()                 {} // Stmt is a marker interface for all statements
func (a *AssignmentStmt) Loc() *source.Location { return &a.Location }

// TypeDeclStmt represents a type declaration statement
type TypeDeclStmt struct {
	Alias    *IdentifierExpr // The name of the type
	BaseType DataType        // The underlying type
	source.Location
}

func (t *TypeDeclStmt) INode() Node           { return t }
func (t *TypeDeclStmt) Stmt()                 {} // Stmt is a marker interface for all statements
func (t *TypeDeclStmt) Loc() *source.Location { return &t.Location }

// ReturnStmt represents a return statement
type ReturnStmt struct {
	Values ExpressionList
	source.Location
}

func (r *ReturnStmt) INode() Node           { return r }
func (r *ReturnStmt) Stmt()                 {} // Stmt is a marker method for statements
func (r *ReturnStmt) Loc() *source.Location { return &r.Location }

// PackageDeclStmt represents a package declaration
type PackageDeclStmt struct {
	Package *IdentifierExpr
	source.Location
}

func (p *PackageDeclStmt) INode() Node           { return p }
func (p *PackageDeclStmt) Stmt()                 {} // Stmt is a marker interface for all statements
func (p *PackageDeclStmt) Loc() *source.Location { return &p.Location }

// ImportStmt represents an import statement
type ImportStmt struct {
	ImportPath *StringLiteral
	ModuleName string
	source.Location
}

func (i *ImportStmt) INode() Node           { return i }
func (i *ImportStmt) Stmt()                 {} // Stmt is a marker interface for all statements
func (i *ImportStmt) Loc() *source.Location { return &i.Location }
