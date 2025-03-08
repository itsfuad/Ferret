package ast

import (
	//ferret packages
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
)

type DataType interface {
	Type() types.TYPE_NAME
	StartPos() lexer.Position
	EndPos() lexer.Position
}

// Integer type
type IntType struct {
	BitSize  int
	Unsigned bool
	TypeName types.TYPE_NAME
	Start    lexer.Position
	End      lexer.Position
}

func (t *IntType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *IntType) StartPos() lexer.Position {
	return t.Start
}
func (t *IntType) EndPos() lexer.Position {
	return t.End
}

// Float type
type FloatType struct {
	BitSize  int
	TypeName types.TYPE_NAME
	Start    lexer.Position
	End      lexer.Position
}

func (t *FloatType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *FloatType) StartPos() lexer.Position {
	return t.Start
}
func (t *FloatType) EndPos() lexer.Position {
	return t.End
}

// String type
type StringType struct {
	TypeName types.TYPE_NAME
	Start    lexer.Position
	End      lexer.Position
}

func (t *StringType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *StringType) StartPos() lexer.Position {
	return t.Start
}
func (t *StringType) EndPos() lexer.Position {
	return t.End
}

// Byte type
type ByteType struct {
	TypeName types.TYPE_NAME
	Start    lexer.Position
	End      lexer.Position
}

func (t *ByteType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *ByteType) StartPos() lexer.Position {
	return t.Start
}
func (t *ByteType) EndPos() lexer.Position {
	return t.End
}

// Boolean type
type BoolType struct {
	TypeName types.TYPE_NAME
	Start    lexer.Position
	End      lexer.Position
}

func (t *BoolType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *BoolType) StartPos() lexer.Position {
	return t.Start
}
func (t *BoolType) EndPos() lexer.Position {
	return t.End
}

// Array type
type ArrayType struct {
	ElementType DataType
	TypeName    types.TYPE_NAME
	Start       lexer.Position
	End         lexer.Position
}

func (t *ArrayType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *ArrayType) StartPos() lexer.Position {
	return t.Start
}
func (t *ArrayType) EndPos() lexer.Position {
	return t.End
}

// Later, we will add more types like structs, arrays, maps, etc.
