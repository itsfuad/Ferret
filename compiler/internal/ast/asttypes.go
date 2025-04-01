package ast

import (
	"ferret/compiler/internal/source"
	"ferret/compiler/types"
)

type DataType interface {
	Node
	Type() types.TYPE_NAME
	Loc() *source.Location
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
func (t *UserDefinedType) Loc() *source.Location {
	return &t.Location
}

// Integer type
type IntType struct {
	BitSize    uint8
	IsUnsigned bool
	TypeName   types.TYPE_NAME
	source.Location
}

func (t *IntType) INode() {} // INode is a marker interface for all nodes
func (t *IntType) Type() types.TYPE_NAME {
	return t.TypeName
}
func (t *IntType) Loc() *source.Location {
	return &t.Location
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
func (t *FloatType) Loc() *source.Location {
	return &t.Location
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
func (t *StringType) Loc() *source.Location {
	return &t.Location
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
func (t *ByteType) Loc() *source.Location {
	return &t.Location
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
func (t *BoolType) Loc() *source.Location {
	return &t.Location
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
func (t *ArrayType) Loc() *source.Location {
	return &t.Location
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

func (s *StructType) INode()                {} // INode is a marker interface for all nodes
func (s *StructType) Type() types.TYPE_NAME { return s.TypeName }
func (s *StructType) Loc() *source.Location { return &s.Location }

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

func (i *InterfaceType) INode()                {} // INode is a marker interface for all nodes
func (i *InterfaceType) Type() types.TYPE_NAME { return i.TypeName }
func (i *InterfaceType) Loc() *source.Location { return &i.Location }

type FunctionType struct {
	Parameters  []DataType
	ReturnTypes []DataType
	TypeName    types.TYPE_NAME
	source.Location
}

func (f *FunctionType) INode()                {} // INode is a marker interface for all nodes
func (f *FunctionType) Type() types.TYPE_NAME { return f.TypeName }
func (f *FunctionType) Loc() *source.Location { return &f.Location }
