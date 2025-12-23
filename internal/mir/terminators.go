package mir

import "compiler/internal/source"

// Term is the base interface for MIR terminators.
type Term interface {
	mirTerm()
	Loc() *source.Location
}

// Return exits the current function.
type Return struct {
	Value    ValueID
	HasValue bool
	Location source.Location
}

func (r *Return) mirTerm()              {}
func (r *Return) Loc() *source.Location { return &r.Location }

// Br jumps unconditionally to another block.
type Br struct {
	Target   BlockID
	Location source.Location
}

func (b *Br) mirTerm()              {}
func (b *Br) Loc() *source.Location { return &b.Location }

// CondBr jumps based on a boolean condition.
type CondBr struct {
	Cond     ValueID
	Then     BlockID
	Else     BlockID
	Location source.Location
}

func (c *CondBr) mirTerm()              {}
func (c *CondBr) Loc() *source.Location { return &c.Location }

// Switch selects a target based on an integer-like condition.
type Switch struct {
	Cond     ValueID
	Cases    []SwitchCase
	Default  BlockID
	Location source.Location
}

// SwitchCase maps a constant to a target block.
type SwitchCase struct {
	Value  string
	Target BlockID
}

func (s *Switch) mirTerm()              {}
func (s *Switch) Loc() *source.Location { return &s.Location }

// Unreachable marks an invalid control-flow path.
type Unreachable struct {
	Location source.Location
}

func (u *Unreachable) mirTerm()              {}
func (u *Unreachable) Loc() *source.Location { return &u.Location }
