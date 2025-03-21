package ast

import (
	"ferret/compiler/internal/source"
	"ferret/compiler/internal/types"
)

type DataType interface {
	Node
	Type() types.TYPE_NAME
	StartPos() *source.Position
	EndPos() *source.Position
}

// User defined type
type UserDefinedType struct {
	TypeName types.TYPE_NAME
	source.Location
}

func (t *UserDefinedType) INode() {} // INode is a marker interface for all nodes
func (t *UserDefinedType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *UserDefinedType) StartPos() *source.Position {
	return t.Location.Start
}
func (t *UserDefinedType) EndPos() *source.Position {
	return t.Location.End
}

// Integer type
type IntType struct {
	BitSize  uint8
	Unsigned bool
	TypeName types.TYPE_NAME
	source.Location
}

func (t *IntType) INode() {} // INode is a marker interface for all nodes
func (t *IntType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *IntType) StartPos() *source.Position {
	return t.Location.Start
}
func (t *IntType) EndPos() *source.Position {
	return t.Location.End
}

// Float type
type FloatType struct {
	BitSize  uint8
	TypeName types.TYPE_NAME
	source.Location
}

func (t *FloatType) INode() {} // INode is a marker interface for all nodes
func (t *FloatType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *FloatType) StartPos() *source.Position {
	return t.Location.Start
}
func (t *FloatType) EndPos() *source.Position {
	return t.Location.End
}

// String type
type StringType struct {
	TypeName types.TYPE_NAME
	source.Location
}

func (t *StringType) INode() {} // INode is a marker interface for all nodes
func (t *StringType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *StringType) StartPos() *source.Position {
	return t.Location.Start
}
func (t *StringType) EndPos() *source.Position {
	return t.Location.End
}

// Byte type
type ByteType struct {
	TypeName types.TYPE_NAME
	source.Location
}

func (t *ByteType) INode() {} // INode is a marker interface for all nodes
func (t *ByteType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *ByteType) StartPos() *source.Position {
	return t.Location.Start
}
func (t *ByteType) EndPos() *source.Position {
	return t.Location.End
}

// Boolean type
type BoolType struct {
	TypeName types.TYPE_NAME
	source.Location
}

func (t *BoolType) INode() {} // INode is a marker interface for all nodes
func (t *BoolType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *BoolType) StartPos() *source.Position {
	return t.Location.Start
}
func (t *BoolType) EndPos() *source.Position {
	return t.Location.End
}

// Array type
type ArrayType struct {
	ElementType DataType
	TypeName    types.TYPE_NAME
	source.Location
}

func (t *ArrayType) INode() {} // INode is a marker interface for all nodes
func (t *ArrayType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *ArrayType) StartPos() *source.Position {
	return t.Location.Start
}
func (t *ArrayType) EndPos() *source.Position {
	return t.Location.End
}

// Parameter represents a function or method parameter
type Parameter struct {
	Identifier *IdentifierExpr
	Type       DataType
}

type StructField struct {
	Field IdentifierExpr
	Type  DataType   // nil for literal
	Value Expression // nil for type
	source.Location
}

// StructType represents a struct type definition
type StructType struct {
	Fields   []StructField
	TypeName types.TYPE_NAME
	source.Location
}

func (s *StructType) INode()                     {} // INode is a marker interface for all nodes
func (s *StructType) Type() types.TYPE_NAME      { return s.TypeName }
func (s *StructType) StartPos() *source.Position { return s.Start }
func (s *StructType) EndPos() *source.Position   { return s.End }

type InterfaceMethod struct {
	Name       IdentifierExpr
	Params     []Parameter
	ReturnType []DataType
	source.Location
}

// InterfaceType represents an interface type definition
type InterfaceType struct {
	Methods  []InterfaceMethod
	TypeName types.TYPE_NAME
	source.Location
}

func (i *InterfaceType) INode()                     {} // INode is a marker interface for all nodes
func (i *InterfaceType) Type() types.TYPE_NAME      { return i.TypeName }
func (i *InterfaceType) StartPos() *source.Position { return i.Start }
func (i *InterfaceType) EndPos() *source.Position   { return i.End }

type FunctionType struct {
	Parameters  []DataType
	ReturnTypes []DataType
	TypeName    types.TYPE_NAME
	source.Location
}

func (f *FunctionType) INode()                     {} // INode is a marker interface for all nodes
func (f *FunctionType) Type() types.TYPE_NAME      { return f.TypeName }
func (f *FunctionType) StartPos() *source.Position { return f.Location.Start }
func (f *FunctionType) EndPos() *source.Position   { return f.Location.End }
