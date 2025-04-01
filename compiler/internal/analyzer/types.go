package analyzer

import (
	"ferret/compiler/types"
)

type NODE_TYPE uint8

const (
	TYPE_NODE NODE_TYPE = iota
	VALUE_NODE
)

type AnalyzerNode interface {
	ANode()
	ANodeType() NODE_TYPE
	IsReference() bool
	IsConstant() bool
}

type IntegerType struct {
	TypeName types.TYPE_NAME
	NodeType NODE_TYPE
	IsSigned bool
	BitSize  uint8
	IsLValue bool
	IsRef    bool
	IsConst  bool
}

func (t *IntegerType) ANode()               {} // implements AnalyzerNode
func (t *IntegerType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *IntegerType) IsReference() bool    { return t.IsRef }
func (t *IntegerType) IsConstant() bool     { return t.IsConst }

type FloatType struct {
	TypeName types.TYPE_NAME
	NodeType NODE_TYPE
	BitSize  uint8
	IsLValue bool
	IsRef    bool
	IsConst  bool
}

func (t *FloatType) ANode()               {} // implements AnalyzerNode
func (t *FloatType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *FloatType) IsReference() bool    { return t.IsRef }
func (t *FloatType) IsConstant() bool     { return t.IsConst }

type StringType struct {
	TypeName types.TYPE_NAME
	NodeType NODE_TYPE
	IsLValue bool
	IsRef    bool
	IsConst  bool
}

func (t *StringType) ANode()               {} // implements AnalyzerNode
func (t *StringType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *StringType) IsReference() bool    { return t.IsRef }
func (t *StringType) IsConstant() bool     { return t.IsConst }

type BoolType struct {
	TypeName types.TYPE_NAME
	NodeType NODE_TYPE
	IsLValue bool
	IsRef    bool
	IsConst  bool
}

func (t *BoolType) ANode()               {} // implements AnalyzerNode
func (t *BoolType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *BoolType) IsReference() bool    { return t.IsRef }
func (t *BoolType) IsConstant() bool     { return t.IsConst }

type ArrayType struct {
	TypeName    types.TYPE_NAME
	ElementType AnalyzerNode
	Size        uint32
	NodeType    NODE_TYPE
	IsLValue    bool
	IsRef       bool
	IsConst     bool
}

func (t *ArrayType) ANode()               {} // implements AnalyzerNode
func (t *ArrayType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *ArrayType) IsReference() bool    { return t.IsRef }
func (t *ArrayType) IsConstant() bool     { return t.IsConst }

type FunctionType struct {
	TypeName   types.TYPE_NAME
	Parameters []AnalyzerNode
	ReturnType AnalyzerNode
	NodeType   NODE_TYPE
	IsLValue   bool
	IsRef      bool
	IsConst    bool
}

func (t *FunctionType) ANode()               {} // implements AnalyzerNode
func (t *FunctionType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *FunctionType) IsReference() bool    { return t.IsRef }
func (t *FunctionType) IsConstant() bool     { return t.IsConst }

type VoidType struct {
	TypeName types.TYPE_NAME
	NodeType NODE_TYPE
	IsLValue bool
	IsRef    bool
	IsConst  bool
}

func (t *VoidType) ANode()               {} // implements AnalyzerNode
func (t *VoidType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *VoidType) IsReference() bool    { return t.IsRef }
func (t *VoidType) IsConstant() bool     { return t.IsConst }

type StructType struct {
	TypeName types.TYPE_NAME
	Fields   []AnalyzerNode
	NodeType NODE_TYPE
	IsLValue bool
	IsRef    bool
	IsConst  bool
}

func (t *StructType) ANode()               {} // implements AnalyzerNode
func (t *StructType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *StructType) IsReference() bool    { return t.IsRef }
func (t *StructType) IsConstant() bool     { return t.IsConst }

type InterfaceType struct {
	TypeName types.TYPE_NAME
	Methods  []AnalyzerNode
	NodeType NODE_TYPE
	IsLValue bool
	IsRef    bool
	IsConst  bool
}

func (t *InterfaceType) ANode()               {} // implements AnalyzerNode
func (t *InterfaceType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *InterfaceType) IsReference() bool    { return t.IsRef }
func (t *InterfaceType) IsConstant() bool     { return t.IsConst }

type UserDefType struct {
	TypeName       types.TYPE_NAME
	UnderlyingType AnalyzerNode
	NodeType       NODE_TYPE
	IsLValue       bool
	IsRef          bool
	IsConst        bool
}

func (t *UserDefType) ANode()               {} // implements AnalyzerNode
func (t *UserDefType) ANodeType() NODE_TYPE { return t.NodeType }
func (t *UserDefType) IsReference() bool    { return t.IsRef }
func (t *UserDefType) IsConstant() bool     { return t.IsConst }
