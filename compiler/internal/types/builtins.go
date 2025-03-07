package types

import "fmt"

type COMMON_TYPE string

const (
	INT8         COMMON_TYPE = "i8"
	INT16        COMMON_TYPE = "i16"
	INT32        COMMON_TYPE = "i32"
	INT64        COMMON_TYPE = "i64"
	UINT8        COMMON_TYPE = "u8"
	UINT16       COMMON_TYPE = "u16"
	UINT32       COMMON_TYPE = "u32"
	UINT64       COMMON_TYPE = "u64"
	FLOAT32      COMMON_TYPE = "f32"
	FLOAT64      COMMON_TYPE = "f64"
	STRING       COMMON_TYPE = "str"
	BYTE         COMMON_TYPE = "byte"
	BOOL         COMMON_TYPE = "bool"
	FUNCTION     COMMON_TYPE = "fn"
	STRUCT       COMMON_TYPE = "struct"
	INTERFACE    COMMON_TYPE = "interface"
	ARRAY        COMMON_TYPE = "array"
	MAP          COMMON_TYPE = "map"
	VOID         COMMON_TYPE = "void"
	USER_DEFINED COMMON_TYPE = "user_defined"
)

func GetBitSize(kind COMMON_TYPE) (uint8, error) {
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

func IsSigned(kind COMMON_TYPE) bool {
	switch kind {
	case INT8, INT16, INT32, INT64:
		return true
	default:
		return false
	}
}

func IsUnsigned(kind COMMON_TYPE) bool {
	switch kind {
	case UINT8, UINT16, UINT32, UINT64, BYTE:
		return true
	default:
		return false
	}
}