package analyzer

import (
	"ferret/compiler/types"
)

type AnalyzerNode interface {
	ANode() types.TYPE_NAME
}

type IntegerType struct {
	TypeName   types.TYPE_NAME
	IsUnsigned bool
	BitSize    uint8
}

func (t *IntegerType) ANode() types.TYPE_NAME { return t.TypeName }

type FloatType struct {
	TypeName types.TYPE_NAME
	BitSize  uint8
}

func (t *FloatType) ANode() types.TYPE_NAME { return t.TypeName }

type StringType struct {
	TypeName types.TYPE_NAME
}

func (t *StringType) ANode() types.TYPE_NAME { return t.TypeName }

type BoolType struct {
	TypeName types.TYPE_NAME
}

func (t *BoolType) ANode() types.TYPE_NAME { return t.TypeName }

type ByteType struct {
	TypeName types.TYPE_NAME
}

func (t *ByteType) ANode() types.TYPE_NAME { return t.TypeName }

type ArrayType struct {
	TypeName    types.TYPE_NAME
	ElementType AnalyzerNode
	Size        uint32
}

func (t *ArrayType) ANode() types.TYPE_NAME { return t.TypeName }

type FunctionType struct {
	TypeName   types.TYPE_NAME
	Parameters []AnalyzerNode
	ReturnType AnalyzerNode
}

func (t *FunctionType) ANode() types.TYPE_NAME { return t.TypeName }

type StructType struct {
	TypeName types.TYPE_NAME
	Fields   []AnalyzerNode
}

func (t *StructType) ANode() types.TYPE_NAME { return t.TypeName }

type InterfaceType struct {
	TypeName types.TYPE_NAME
	Methods  []AnalyzerNode
}

func (t *InterfaceType) ANode() types.TYPE_NAME { return t.TypeName }

type UserDefType struct {
	TypeName       types.TYPE_NAME
	UnderlyingType AnalyzerNode
}

func (t *UserDefType) ANode() types.TYPE_NAME { return t.TypeName }
