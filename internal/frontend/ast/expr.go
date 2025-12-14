package ast

import (
	"compiler/internal/source"
	"compiler/internal/tokens"
	"compiler/internal/types"
)

// BinaryExpr represents a binary expression
type BinaryExpr struct {
	X    Expression    // left operand
	Op   tokens.Token  // operator
	Y    Expression    // right operand
	Type types.SemType // type information (populated during semantic analysis)
	source.Location
}

func (b *BinaryExpr) INode()                {} // Implements Node interface
func (b *BinaryExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (b *BinaryExpr) Loc() *source.Location { return &b.Location }

// UnaryExpr represents a unary expression
type UnaryExpr struct {
	Op   tokens.Token  // operator
	X    Expression    // operand
	Type types.SemType // type information (populated during semantic analysis)
	source.Location
}

func (u *UnaryExpr) INode()                {} // Implements Node interface
func (u *UnaryExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (u *UnaryExpr) Loc() *source.Location { return &u.Location }

// PrefixExpr represents a prefix increment/decrement expression (++x, --x)
type PrefixExpr struct {
	Op   tokens.Token  // operator (++, --)
	X    Expression    // operand
	Type types.SemType // type information (populated during semantic analysis)
	source.Location
}

func (p *PrefixExpr) INode()                {} // Implements Node interface
func (p *PrefixExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (p *PrefixExpr) Loc() *source.Location { return &p.Location }

// PostfixExpr represents a postfix increment/decrement expression (x++, x--)
type PostfixExpr struct {
	X    Expression    // operand
	Op   tokens.Token  // operator (++, --)
	Type types.SemType // type information (populated during semantic analysis)
	source.Location
}

func (p *PostfixExpr) INode()                {} // Implements Node interface
func (p *PostfixExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (p *PostfixExpr) Loc() *source.Location { return &p.Location }

// IdentifierExpr represents an identifier
type IdentifierExpr struct {
	Name string
	Type types.SemType // type information (populated during semantic analysis, from symbol table)
	source.Location
}

func (i *IdentifierExpr) INode()                {} // Implements Node interface
func (i *IdentifierExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (i *IdentifierExpr) TypeExpr()             {} // Can also be used as a type name
func (i *IdentifierExpr) Loc() *source.Location { return &i.Location }

// CatchClause represents the catch part of error handling
// Syntax: catch [ident] [block] [fallback]
// Examples:
//   - catch 0                    -> just fallback
//   - catch err { ... } 0        -> handler + fallback
//   - catch err { return 0; }    -> handler only (must handle control flow)
type CatchClause struct {
	ErrIdent *IdentifierExpr // Optional: error variable name (e.g., 'err')
	Handler  *Block          // Optional: error handler block { ... }
	Fallback Expression      // Optional: fallback/default value
	source.Location
}

func (c *CatchClause) INode()                {} // Implements Node interface
func (c *CatchClause) Loc() *source.Location { return &c.Location }

// CallExpr represents a function call expression
type CallExpr struct {
	Fun   Expression    // function expression
	Args  []Expression  // function arguments
	Catch *CatchClause  // Optional: error handling (nil if no catch)
	Type  types.SemType // return type (populated during semantic analysis)
	source.Location
}

func (c *CallExpr) INode()                {} // Implements Node interface
func (c *CallExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (c *CallExpr) Loc() *source.Location { return &c.Location }

// SelectorExpr represents a field access expression (x.field)
type SelectorExpr struct {
	X     Expression      // expression
	Field *IdentifierExpr // field selector
	Type  types.SemType   // type of the selected field (populated during semantic analysis)
	source.Location
}

func (s *SelectorExpr) INode()                {} // Implements Node interface
func (s *SelectorExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (s *SelectorExpr) Loc() *source.Location { return &s.Location }

// RangeExpr represents a range expression: start..end or start..=end or start..end:incr
// Generates an array from start to end with optional increment
// Inclusive controls whether end is included: .. (false) or ..= (true)
type RangeExpr struct {
	Start     Expression    // start value (e.g., 0)
	End       Expression    // end value (e.g., 10)
	Incr      Expression    // increment (optional, default 1)
	Inclusive bool          // true for ..=, false for ..
	Type      types.SemType // array type (populated during semantic analysis)
	source.Location
}

func (r *RangeExpr) INode()                {} // Implements Node interface
func (r *RangeExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (r *RangeExpr) Loc() *source.Location { return &r.Location }

// IndexExpr represents an index expression (array[index])
type IndexExpr struct {
	X     Expression    // expression
	Index Expression    // index
	Type  types.SemType // element type (populated during semantic analysis)
	source.Location
}

func (i *IndexExpr) INode()                {} // Implements Node interface
func (i *IndexExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (i *IndexExpr) Loc() *source.Location { return &i.Location }

// CastExpr represents a type cast expression (value as TargetType)
type CastExpr struct {
	X        Expression    // value being cast
	Type     TypeNode      // target type (AST node)
	TypeInfo types.SemType // resolved target type (populated during semantic analysis)
	source.Location
}

func (c *CastExpr) INode()                {} // Implements Node interface
func (c *CastExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (c *CastExpr) Loc() *source.Location { return &c.Location }

// CoalescingExpr represents the coalescing operator (a ?? b)
// If a is none/falsy, returns b
type CoalescingExpr struct {
	Cond    Expression    // condition (left side)
	Default Expression    // default value (right side)
	Type    types.SemType // result type (populated during semantic analysis)
	source.Location
}

func (e *CoalescingExpr) INode()                {} // Implements Node interface
func (e *CoalescingExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (e *CoalescingExpr) Loc() *source.Location { return &e.Location }

// ForkExpr represents a fork expression for coroutines
type ForkExpr struct {
	Call Expression    // the function call to fork
	Type types.SemType // return type (populated during semantic analysis)
	source.Location
}

func (s *ForkExpr) INode()                {} // Implements Node interface
func (s *ForkExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (s *ForkExpr) Loc() *source.Location { return &s.Location }

// ScopeResolutionExpr represents scope resolution (module::symbol or Enum::Variant)
type ScopeResolutionExpr struct {
	X        Expression      // left side (module or enum name)
	Selector *IdentifierExpr // symbol being accessed
	Type     types.SemType   // type of the resolved symbol (populated during semantic analysis)
	source.Location
}

func (s *ScopeResolutionExpr) INode()                {} // Implements Node interface
func (s *ScopeResolutionExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (s *ScopeResolutionExpr) TypeExpr()             {} // Can also be used as a type name (for module::Type)
func (s *ScopeResolutionExpr) Loc() *source.Location { return &s.Location }

// ParenExpr represents a parenthesized expression
type ParenExpr struct {
	X    Expression    // expression within parentheses
	Type types.SemType // type information (same as inner expression, populated during semantic analysis)
	source.Location
}

func (p *ParenExpr) INode()                {} // Implements Node interface
func (p *ParenExpr) Expr()                 {} // Expr is a marker interface for all expressions
func (p *ParenExpr) Loc() *source.Location { return &p.Location }
