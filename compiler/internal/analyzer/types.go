package analyzer

import (
	"ferret/compiler/types"
	"fmt"
)

type AnalyzerNode interface {
	ANode() types.TYPE_NAME
	ToString() string
}

type IntegerType struct {
	TypeName   types.TYPE_NAME
	IsUnsigned bool
	BitSize    uint8
}

func (t *IntegerType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *IntegerType) ToString() string       { return string(t.TypeName) }
func (t *IntegerType) Some() bool             { return true }

type FloatType struct {
	TypeName types.TYPE_NAME
	BitSize  uint8
}

func (t *FloatType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *FloatType) ToString() string       { return string(t.TypeName) }

type StringType struct {
	TypeName types.TYPE_NAME
}

func (t *StringType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *StringType) ToString() string       { return string(t.TypeName) }

type BoolType struct {
	TypeName types.TYPE_NAME
}

func (t *BoolType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *BoolType) ToString() string       { return string(t.TypeName) }

type ByteType struct {
	TypeName types.TYPE_NAME
}

func (t *ByteType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *ByteType) ToString() string       { return string(t.TypeName) }

type ArrayType struct {
	TypeName    types.TYPE_NAME
	ElementType AnalyzerNode
	Size        uint32
}

func (t *ArrayType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *ArrayType) ToString() string       { return fmt.Sprintf("[]%s", t.ElementType.ToString()) }

type FunctionType struct {
	TypeName   types.TYPE_NAME
	Parameters []AnalyzerNode
	ReturnType AnalyzerNode
}

func (t *FunctionType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *FunctionType) ToString() string {
	paramString := ""
	for i, param := range t.Parameters {
		if i > 0 {
			paramString += ", "
		}
		paramString += param.ToString()
	}

	tail := ""
	if t.ReturnType != nil {
		tail = " -> " + t.ReturnType.ToString()
	}
	return fmt.Sprintf("(%s)%s", paramString, tail)
}

type StructField struct {
	Name string
	Type AnalyzerNode
}

type StructType struct {
	TypeName types.TYPE_NAME
	Fields   []StructField
}

func (t *StructType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *StructType) ToString() string {
	fieldString := ""
	for i, field := range t.Fields {
		if i > 0 {
			fieldString += ", "
		}
		fieldString += fmt.Sprintf("%s: %s", field.Name, field.Type.ToString())
	}
	return fmt.Sprintf("%s { %s }", t.TypeName, fieldString)
}

type InterfaceType struct {
	TypeName types.TYPE_NAME
	Methods  []AnalyzerNode
}

func (t *InterfaceType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *InterfaceType) ToString() string {
	return "not implemented"
}

type UserDefType struct {
	TypeName       types.TYPE_NAME
	UnderlyingType AnalyzerNode
}

func (t *UserDefType) ANode() types.TYPE_NAME { return t.TypeName }
func (t *UserDefType) ToString() string {
	return unwrapType(t.UnderlyingType).ToString()
}
