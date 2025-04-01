package types

import (
	"fmt"
	"strconv"
)

func IsInteger(value string) bool {
	_, err := strconv.Atoi(value)
	return err == nil
}

func IsFloat(value string) bool {
	_, err := strconv.ParseFloat(value, 64)
	return err == nil
}

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
