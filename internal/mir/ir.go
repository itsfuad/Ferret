package mir

import (
	"compiler/internal/source"
	"compiler/internal/types"
)

// ValueID identifies a SSA value within a function.
type ValueID uint32

// BlockID identifies a basic block within a function.
type BlockID uint32

const (
	InvalidValue ValueID = 0
	InvalidBlock BlockID = 0
)

// Module is the MIR root for a single source module.
type Module struct {
	ImportPath string
	Functions  []*Function
	Location   source.Location
}

// Function is a typed, SSA-based MIR function.
type Function struct {
	Name     string
	Receiver *Param
	Params   []Param
	Return   types.SemType
	Blocks   []*Block
	Location source.Location
}

// Param describes a function parameter value.
type Param struct {
	ID       ValueID
	Name     string
	Type     types.SemType
	Location source.Location
}

// Block is a basic block with a list of instructions and a terminator.
type Block struct {
	ID       BlockID
	Name     string
	Instrs   []Instr
	Term     Term
	Location source.Location
}
