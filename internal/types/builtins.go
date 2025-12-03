package types

// TODO: TYPE_NAME is a simple string alias that works for primitive types
// but is insufficient for complex types. Before implementing generics, optionals,
// or complex type features, we need to refactor to a proper Type interface:
//
//   type Type interface {
//       String() string
//       Equals(other Type) bool
//   }
//
//   type PrimitiveType struct { Name string }
//   type ArrayType struct { Element Type }
//   type MapType struct { Key, Value Type }
//   type FunctionType struct { Params []Type; Return Type }
//   type OptionalType struct { Inner Type }     // i32?
//   type ResultType struct { Ok, Err Type }     // i32 ! Error
//   type GenericType struct { Name string; Args []Type }  // Vec<i32>
//
// This will enable proper type checking for:
// - Generic instantiation: Vec<T> -> Vec<i32>
// - Function signatures: fn(i32, str) -> bool
// - Nested structures: Map<str, Array<i32>>
// - Type parameters and constraints

type TYPE_NAME string

const (
	TYPE_I8        TYPE_NAME = "i8"
	TYPE_I16       TYPE_NAME = "i16"
	TYPE_I32       TYPE_NAME = "i32"
	TYPE_I64       TYPE_NAME = "i64"
	TYPE_I128      TYPE_NAME = "i128"
	TYPE_I256      TYPE_NAME = "i256"
	TYPE_U8        TYPE_NAME = "u8"
	TYPE_U16       TYPE_NAME = "u16"
	TYPE_U32       TYPE_NAME = "u32"
	TYPE_U64       TYPE_NAME = "u64"
	TYPE_U128      TYPE_NAME = "u128"
	TYPE_U256      TYPE_NAME = "u256"
	TYPE_F32       TYPE_NAME = "f32"
	TYPE_F64       TYPE_NAME = "f64"
	TYPE_STRING    TYPE_NAME = "str"
	TYPE_BOOL      TYPE_NAME = "bool"
	TYPE_NONE      TYPE_NAME = "none"
	TYPE_VOID      TYPE_NAME = "void"
	TYPE_BYTE      TYPE_NAME = "byte"
	TYPE_FUNC      TYPE_NAME = "fn"
	TYPE_ARRAY     TYPE_NAME = "array"
	TYPE_INTERFACE TYPE_NAME = "interface"
	TYPE_STRUCT    TYPE_NAME = "struct"
	TYPE_ENUM      TYPE_NAME = "enum"
	TYPE_MAP       TYPE_NAME = "map"

	TYPE_UNTYPED TYPE_NAME = "untyped" // For untyped numeric literals before contextual instantiation

	TYPE_UNKNOWN TYPE_NAME = "unknown"
)

func (t TYPE_NAME) String() string {
	return string(t)
}
