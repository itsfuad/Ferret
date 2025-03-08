package types

import "fmt"

type TYPE_NAME string

const (
	INT8      TYPE_NAME = "i8"
	INT16     TYPE_NAME = "i16"
	INT32     TYPE_NAME = "i32"
	INT64     TYPE_NAME = "i64"
	UINT8     TYPE_NAME = "u8"
	UINT16    TYPE_NAME = "u16"
	UINT32    TYPE_NAME = "u32"
	UINT64    TYPE_NAME = "u64"
	FLOAT32   TYPE_NAME = "f32"
	FLOAT64   TYPE_NAME = "f64"
	STRING    TYPE_NAME = "str"
	BYTE      TYPE_NAME = "byte"
	BOOL      TYPE_NAME = "bool"
	FUNCTION  TYPE_NAME = "fn"
	ARRAY     TYPE_NAME = "array"
	INTERFACE TYPE_NAME = "interface"
	VOID      TYPE_NAME = "void"
	OBJECT    TYPE_NAME = "object"
)

func GetBitSize(kind TYPE_NAME) (uint8, error) {
	switch kind {
	case INT8, UINT8, BYTE:
		return 8, nil
	case INT16, UINT16:
		return 16, nil
	case INT32, UINT32, FLOAT32:
		return 32, nil
	case INT64, UINT64, FLOAT64:
		return 64, nil
	default:
		return 0, fmt.Errorf("invalid type: %s", kind)
	}
}

func IsSigned(kind TYPE_NAME) bool {
	switch kind {
	case INT8, INT16, INT32, INT64:
		return true
	default:
		return false
	}
}

func IsUnsigned(kind TYPE_NAME) bool {
	switch kind {
	case UINT8, UINT16, UINT32, UINT64, BYTE:
		return true
	default:
		return false
	}
}
