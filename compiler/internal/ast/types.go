package ast

import (
	//ferret packages
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
)

type DataType interface {
	Node
	Type() types.TYPE_NAME
	StartPos() *lexer.Position
	EndPos() *lexer.Position
}

// User defined type
type UserDefinedType struct {
	TypeName types.TYPE_NAME
	Location
}

func (t *UserDefinedType) INode() {} // INode is a marker interface for all nodes
func (t *UserDefinedType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *UserDefinedType) StartPos() *lexer.Position {
	return t.Location.Start
}
func (t *UserDefinedType) EndPos() *lexer.Position {
	return t.Location.End
}

// Integer type
type IntType struct {
	BitSize  uint8
	Unsigned bool
	TypeName types.TYPE_NAME
	Location
}

func (t *IntType) INode() {} // INode is a marker interface for all nodes
func (t *IntType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *IntType) StartPos() *lexer.Position {
	return t.Location.Start
}
func (t *IntType) EndPos() *lexer.Position {
	return t.Location.End
}

// Float type
type FloatType struct {
	BitSize  uint8
	TypeName types.TYPE_NAME
	Location
}

func (t *FloatType) INode() {} // INode is a marker interface for all nodes
func (t *FloatType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *FloatType) StartPos() *lexer.Position {
	return t.Location.Start
}
func (t *FloatType) EndPos() *lexer.Position {
	return t.Location.End
}

// String type
type StringType struct {
	TypeName types.TYPE_NAME
	Location
}

func (t *StringType) INode() {} // INode is a marker interface for all nodes
func (t *StringType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *StringType) StartPos() *lexer.Position {
	return t.Location.Start
}
func (t *StringType) EndPos() *lexer.Position {
	return t.Location.End
}

// Byte type
type ByteType struct {
	TypeName types.TYPE_NAME
	Location
}

func (t *ByteType) INode() {} // INode is a marker interface for all nodes
func (t *ByteType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *ByteType) StartPos() *lexer.Position {
	return t.Location.Start
}
func (t *ByteType) EndPos() *lexer.Position {
	return t.Location.End
}

// Boolean type
type BoolType struct {
	TypeName types.TYPE_NAME
	Location
}

func (t *BoolType) INode() {} // INode is a marker interface for all nodes
func (t *BoolType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *BoolType) StartPos() *lexer.Position {
	return t.Location.Start
}
func (t *BoolType) EndPos() *lexer.Position {
	return t.Location.End
}

// Array type
type ArrayType struct {
	ElementType DataType
	TypeName    types.TYPE_NAME
	Location
}

func (t *ArrayType) INode() {} // INode is a marker interface for all nodes
func (t *ArrayType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *ArrayType) StartPos() *lexer.Position {
	return t.Location.Start
}
func (t *ArrayType) EndPos() *lexer.Position {
	return t.Location.End
}

// Parameter represents a function or method parameter
type Parameter struct {
	Identifier *IdentifierExpr
	Type       DataType
}

// MethodDecl represents a method declaration
type MethodDecl struct {
	Name     string
	Receiver *Parameter // Receiver parameter: e.g. in `fn (t *T) M(n int)`, `t` is the receiver
	IsRRef   bool       // Whether the receiver is a reference
	Function *FunctionLiteral
	Location
}

// StructType represents a struct type definition
type StructType struct {
	Fields   []ObjectField
	Methods  []MethodDecl
	TypeName types.TYPE_NAME
	Location
}

func (s *StructType) INode()                    {} // INode is a marker interface for all nodes
func (s *StructType) Type() types.TYPE_NAME     { return s.TypeName }
func (s *StructType) StartPos() *lexer.Position { return s.Start }
func (s *StructType) EndPos() *lexer.Position   { return s.End }

type FunctionType struct {
	Parameters  []DataType
	ReturnTypes []DataType
	TypeName    types.TYPE_NAME
	Location
}

func (f *FunctionType) INode()                    {} // INode is a marker interface for all nodes
func (f *FunctionType) Type() types.TYPE_NAME     { return f.TypeName }
func (f *FunctionType) StartPos() *lexer.Position { return f.Location.Start }
func (f *FunctionType) EndPos() *lexer.Position   { return f.Location.End }
