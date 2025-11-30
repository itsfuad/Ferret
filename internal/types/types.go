package types

import (
	"fmt"
	"strings"
)

// SemType is the semantic representation of types in the Ferret language.
//
// Design principles:
// - Types are immutable after creation
// - SemType equality is structural (deep comparison)
// - All types can be displayed as strings
type SemType interface {
	// String returns a human-readable representation of the type
	String() string

	// Equals checks structural equality with another type
	Equals(other SemType) bool

	// Size returns the size in bytes (for codegen)
	// Returns -1 for types without known size (void, unknown)
	Size() int

	// isType is a marker method to prevent external implementation
	isType()
}

// ============================================================================
// Primitive Types (built-in) likely can be represented as numbers
// ============================================================================

// PrimitiveType represents built-in scalar types (i32, str, bool, etc.)
type PrimitiveType struct {
	name TYPE_NAME
	size int // size in bytes
}

func NewPrimitive(name TYPE_NAME) *PrimitiveType {
	size := getPrimitiveSize(name)
	return &PrimitiveType{name: name, size: size}
}

func (p *PrimitiveType) String() string { return string(p.name) }
func (p *PrimitiveType) Size() int      { return p.size }
func (p *PrimitiveType) isType()        {}
func (p *PrimitiveType) Equals(other SemType) bool {
	if o, ok := other.(*PrimitiveType); ok {
		return p.name == o.name
	}
	return false
}

// GetName returns the primitive type name (for compatibility during migration)
func (p *PrimitiveType) GetName() TYPE_NAME {
	return p.name
}

func getPrimitiveSize(name TYPE_NAME) int {
	switch name {
	case TYPE_I8, TYPE_U8, TYPE_BYTE:
		return 1
	case TYPE_I16, TYPE_U16:
		return 2
	case TYPE_I32, TYPE_U32, TYPE_F32:
		return 4
	case TYPE_I64, TYPE_U64, TYPE_F64:
		return 8
	case TYPE_BOOL:
		return 1
	case TYPE_STRING:
		return 16 // pointer + length
	case TYPE_VOID, TYPE_NONE:
		return 0
	case TYPE_UNKNOWN, TYPE_UNTYPED:
		return -1
	default:
		return -1
	}
}

// ============================================================================
// Array Types
// ============================================================================

// ArrayType represents array types: [N]T or []T (dynamic)
type ArrayType struct {
	Element SemType
	Length  int // -1 for dynamic arrays []T
}

func NewArray(element SemType, length int) *ArrayType {
	return &ArrayType{Element: element, Length: length}
}

func (a *ArrayType) String() string {
	if a.Length < 0 {
		return fmt.Sprintf("[]%s", a.Element.String())
	}
	return fmt.Sprintf("[%d]%s", a.Length, a.Element.String())
}

func (a *ArrayType) Size() int {
	if a.Length < 0 {
		return 16 // slice: pointer + length + capacity
	}
	return a.Element.Size() * a.Length
}

func (a *ArrayType) isType() {}

func (a *ArrayType) Equals(other SemType) bool {
	if o, ok := other.(*ArrayType); ok {
		return a.Length == o.Length && a.Element.Equals(o.Element)
	}
	return false
}

// ============================================================================
// Map Types
// ============================================================================

// MapType represents map types: Map<K, V>
type MapType struct {
	Key   SemType
	Value SemType
}

func NewMap(key, value SemType) *MapType {
	return &MapType{Key: key, Value: value}
}

func (m *MapType) String() string {
	return fmt.Sprintf("Map<%s, %s>", m.Key.String(), m.Value.String())
}

func (m *MapType) Size() int {
	return 8 // pointer to map structure
}

func (m *MapType) isType() {}

func (m *MapType) Equals(other SemType) bool {
	if o, ok := other.(*MapType); ok {
		return m.Key.Equals(o.Key) && m.Value.Equals(o.Value)
	}
	return false
}

// ============================================================================
// Function Types
// ============================================================================

// FunctionType represents function signatures: fn(T1, T2) -> R
type ParamType struct {
	Name string
	Type SemType
}

func (p *ParamType) String() string {
	return fmt.Sprintf("%s: %s", p.Name, p.Type.String())
}

type FunctionType struct {
	Params []ParamType
	Return SemType
}

func NewFunction(params []ParamType, ret SemType) *FunctionType {
	return &FunctionType{Params: params, Return: ret}
}

func (f *FunctionType) String() string {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = p.String()
	}
	return fmt.Sprintf("fn(%s) -> %s", strings.Join(params, ", "), f.Return.String())
}

func (f *FunctionType) Size() int {
	return 8 // function pointer
}

func (f *FunctionType) isType() {}

func (f *FunctionType) Equals(other SemType) bool {
	if ft, ok := other.(*FunctionType); ok {
		// Check return type
		if !f.Return.Equals(ft.Return) {
			return false
		}
		// Check parameter count
		if len(f.Params) != len(ft.Params) {
			return false
		}
		// Check each parameter (both name and type)
		for i := range f.Params {
			if f.Params[i].Name != ft.Params[i].Name {
				return false
			}
			if !f.Params[i].Type.Equals(ft.Params[i].Type) {
				return false
			}
		}
		return true
	}
	return false
}

// ============================================================================
// Optional Types
// ============================================================================

// OptionalType represents nullable types: T?
type OptionalType struct {
	Inner SemType
}

func NewOptional(inner SemType) *OptionalType {
	return &OptionalType{Inner: inner}
}

func (o *OptionalType) String() string {
	return fmt.Sprintf("%s?", o.Inner.String())
}

func (o *OptionalType) Size() int {
	return o.Inner.Size() + 1 // value + presence flag
}

func (o *OptionalType) isType() {}

func (o *OptionalType) Equals(other SemType) bool {
	if ot, ok := other.(*OptionalType); ok {
		return o.Inner.Equals(ot.Inner)
	}
	return false
}

// ============================================================================
// Result/Error Types
// ============================================================================

// ResultType represents result types with error handling: T ! E
type ResultType struct {
	Ok  SemType
	Err SemType
}

func NewResult(ok, err SemType) *ResultType {
	return &ResultType{Ok: ok, Err: err}
}

func (r *ResultType) String() string {
	return fmt.Sprintf("%s ! %s", r.Ok.String(), r.Err.String())
}

func (r *ResultType) Size() int {
	okSize := r.Ok.Size()
	errSize := r.Err.Size()
	if okSize > errSize {
		return okSize + 1 // larger variant + discriminant
	}
	return errSize + 1
}

func (r *ResultType) isType() {}

func (r *ResultType) Equals(other SemType) bool {
	if rt, ok := other.(*ResultType); ok {
		return r.Ok.Equals(rt.Ok) && r.Err.Equals(rt.Err)
	}
	return false
}

// ============================================================================
// Struct Types
// ============================================================================

// StructField represents a field in a struct
type StructField struct {
	Name string
	Type SemType
}

// StructType represents struct types
type StructType struct {
	Name   string // Can be empty for anonymous structs
	Fields []StructField
}

func NewStruct(name string, fields []StructField) *StructType {
	return &StructType{Name: name, Fields: fields}
}

func (s *StructType) String() string {
	if s.Name != "" {
		return s.Name
	}
	// Anonymous struct
	fields := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		fields[i] = fmt.Sprintf(".%s: %s", f.Name, f.Type.String())
	}
	return fmt.Sprintf("struct { %s }", strings.Join(fields, ", "))
}

func (s *StructType) Size() int {
	total := 0
	for _, f := range s.Fields {
		total += f.Type.Size()
	}
	return total
}

func (s *StructType) isType() {}

func (s *StructType) Equals(other SemType) bool {
	if st, ok := other.(*StructType); ok {
		// Named structs must have same name
		if s.Name != "" && st.Name != "" {
			return s.Name == st.Name
		}
		// Anonymous structs must have same structure
		if len(s.Fields) != len(st.Fields) {
			return false
		}
		for i := range s.Fields {
			if s.Fields[i].Name != st.Fields[i].Name {
				return false
			}
			if !s.Fields[i].Type.Equals(st.Fields[i].Type) {
				return false
			}
		}
		return true
	}
	return false
}

// ============================================================================
// Enum Types
// ============================================================================

// EnumVariant represents a variant in an enum
type EnumVariant struct {
	Name string
	Type SemType // nil for unit variants
}

// EnumType represents enum types (sum types/tagged unions)
type EnumType struct {
	Name     string
	Variants []EnumVariant
}

func NewEnum(name string, variants []EnumVariant) *EnumType {
	return &EnumType{Name: name, Variants: variants}
}

func (e *EnumType) String() string {
	return e.Name
}

func (e *EnumType) Size() int {
	// Discriminant + largest variant
	maxSize := 0
	for _, v := range e.Variants {
		if v.Type != nil {
			if s := v.Type.Size(); s > maxSize {
				maxSize = s
			}
		}
	}
	return 4 + maxSize // 4 bytes for discriminant
}

func (e *EnumType) isType() {}

func (e *EnumType) Equals(other SemType) bool {
	if et, ok := other.(*EnumType); ok {
		return e.Name == et.Name
	}
	return false
}

// ============================================================================
// Convenience Functions
// ============================================================================

// Commonly used types (initialized in init())
var (
	TypeI8      SemType
	TypeI16     SemType
	TypeI32     SemType
	TypeI64     SemType
	TypeU8      SemType
	TypeU16     SemType
	TypeU32     SemType
	TypeU64     SemType
	TypeF32     SemType
	TypeF64     SemType
	TypeBool    SemType
	TypeString  SemType
	TypeVoid    SemType
	TypeUnknown SemType
	TypeUntyped SemType
)

func init() {
	TypeI8 = NewPrimitive(TYPE_I8)
	TypeI16 = NewPrimitive(TYPE_I16)
	TypeI32 = NewPrimitive(TYPE_I32)
	TypeI64 = NewPrimitive(TYPE_I64)
	TypeU8 = NewPrimitive(TYPE_U8)
	TypeU16 = NewPrimitive(TYPE_U16)
	TypeU32 = NewPrimitive(TYPE_U32)
	TypeU64 = NewPrimitive(TYPE_U64)
	TypeF32 = NewPrimitive(TYPE_F32)
	TypeF64 = NewPrimitive(TYPE_F64)
	TypeBool = NewPrimitive(TYPE_BOOL)
	TypeString = NewPrimitive(TYPE_STRING)
	TypeVoid = NewPrimitive(TYPE_VOID)
	TypeUnknown = NewPrimitive(TYPE_UNKNOWN)
	TypeUntyped = NewPrimitive(TYPE_UNTYPED)
}

// FromTypeName converts TYPE_NAME to Type (for backward compatibility during migration)
func FromTypeName(name TYPE_NAME) SemType {
	return NewPrimitive(name)
}

// IsPrimitive checks if a type is a primitive type
func IsPrimitive(t SemType) bool {
	_, ok := t.(*PrimitiveType)
	return ok
}

// IsNumeric checks if a type is a numeric type
func IsNumeric(t SemType) bool {
	if p, ok := t.(*PrimitiveType); ok {
		return IsNumericTypeName(p.name)
	}
	return false
}

// IsInteger checks if a type is an integer type
func IsInteger(t SemType) bool {
	if p, ok := t.(*PrimitiveType); ok {
		return IsIntegerTypeName(p.name)
	}
	return false
}

// IsFloat checks if a type is a floating-point type
func IsFloat(t SemType) bool {
	if p, ok := t.(*PrimitiveType); ok {
		return IsFloatTypeName(p.name)
	}
	return false
}

// GetPrimitiveName extracts TYPE_NAME from PrimitiveType (for compatibility)
func GetPrimitiveName(t SemType) (TYPE_NAME, bool) {
	if p, ok := t.(*PrimitiveType); ok {
		return p.name, true
	}
	return TYPE_UNKNOWN, false
}
