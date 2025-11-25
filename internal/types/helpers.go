package types

func GetNumberBitSize(kind TYPE_NAME) uint8 {
	switch kind {
	case TYPE_I8, TYPE_U8, TYPE_BYTE:
		return 8
	case TYPE_I16, TYPE_U16:
		return 16
	case TYPE_I32, TYPE_U32, TYPE_F32:
		return 32
	case TYPE_I64, TYPE_U64, TYPE_F64:
		return 64
	default:
		return 0
	}
}

func IsSigned(kind TYPE_NAME) bool {
	switch kind {
	case TYPE_I8, TYPE_I16, TYPE_I32, TYPE_I64:
		return true
	default:
		return false
	}
}

func IsUnsigned(kind TYPE_NAME) bool {
	switch kind {
	case TYPE_U8, TYPE_U16, TYPE_U32, TYPE_U64, TYPE_BYTE:
		return true
	default:
		return false
	}
}

// IsNumericTypeName checks if a type name is numeric
func IsNumericTypeName(typeName TYPE_NAME) bool {
	return IsIntegerTypeName(typeName) || IsFloatTypeName(typeName)
}

// IsIntegerTypeName checks if a type name is an integer type
func IsIntegerTypeName(typeName TYPE_NAME) bool {
	switch typeName {
	case TYPE_I8, TYPE_I16, TYPE_I32, TYPE_I64,
		TYPE_U8, TYPE_U16, TYPE_U32, TYPE_U64, TYPE_BYTE:
		return true
	default:
		return false
	}
}

func IsFloatTypeName(typeName TYPE_NAME) bool {
	switch typeName {
	case TYPE_F32, TYPE_F64:
		return true
	default:
		return false
	}
}
