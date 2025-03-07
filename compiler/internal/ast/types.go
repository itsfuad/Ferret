package ast

import (
	//Standard packages
	"fmt"

	//ferret packages
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
)

type DataType interface {
	Type() types.COMMON_TYPE
	StartPos() lexer.Position
	EndPos() lexer.Position
}

// Integer type
type IntType struct {
	BitSize int
	Unsigned bool
	Start   lexer.Position
	End     lexer.Position
}
func (t *IntType) Type() types.COMMON_TYPE {
	prefix := ""
	if t.Unsigned {
		prefix = "u"
	}
	return types.COMMON_TYPE(fmt.Sprintf("%si%d", prefix, t.BitSize))
}
func (t *IntType) StartPos() lexer.Position {
	return t.Start
}
func (t *IntType) EndPos() lexer.Position {
	return t.End
}

// Float type
type FloatType struct {
	BitSize int
	Start   lexer.Position
	End     lexer.Position
}
func (t *FloatType) Type() types.COMMON_TYPE {
	return types.COMMON_TYPE(fmt.Sprintf("f%d", t.BitSize))
}
func (t *FloatType) StartPos() lexer.Position {
	return t.Start
}
func (t *FloatType) EndPos() lexer.Position {
	return t.End
}

// String type
type StringType struct {
	Start lexer.Position
	End   lexer.Position
}
func (t *StringType) Type() types.COMMON_TYPE {
	return types.STRING
}
func (t *StringType) StartPos() lexer.Position {
	return t.Start
}
func (t *StringType) EndPos() lexer.Position {
	return t.End
}

// Byte type
type ByteType struct {
	Start lexer.Position
	End   lexer.Position
}
func (t *ByteType) Type() types.COMMON_TYPE {
	return types.BYTE
}
func (t *ByteType) StartPos() lexer.Position {
	return t.Start
}
func (t *ByteType) EndPos() lexer.Position {
	return t.End
}

// Boolean type
type BoolType struct {
	Start lexer.Position
	End   lexer.Position
}
func (t *BoolType) Type() types.COMMON_TYPE {
	return types.BOOL
}
func (t *BoolType) StartPos() lexer.Position {
	return t.Start
}
func (t *BoolType) EndPos() lexer.Position {
	return t.End
}

// Later, we will add more types like structs, arrays, maps, etc.
