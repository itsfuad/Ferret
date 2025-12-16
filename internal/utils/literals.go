package utils

import (
	"fmt"
	"sync/atomic"
)

const (
	FN   = "__func_lit__"
	ST   = "__struct_lit__"
	INTR = "__interface_lit__"
	ENUM = "__enum_lit__"
)

// Global counter for generating unique literal IDs
var litCountermap = map[string]*int64{
	FN:   new(int64),
	ST:   new(int64),
	INTR: new(int64),
	ENUM: new(int64),
}

// GenerateFuncLitID generates a unique ID for function literals
func GenerateFuncLitID() string {
	id := atomic.AddInt64(litCountermap[FN], 1)
	return fmt.Sprintf("%s%d", FN, id)
}

func GenerateStructLitID() string {
	id := atomic.AddInt64(litCountermap[ST], 1)
	return fmt.Sprintf("%s%d", ST, id)
}

func GenerateInterfaceLitID() string {
	id := atomic.AddInt64(litCountermap[INTR], 1)
	return fmt.Sprintf("%s%d", INTR, id)
}

func GenerateEnumLitID() string {
	id := atomic.AddInt64(litCountermap[ENUM], 1)
	return fmt.Sprintf("%s%d", ENUM, id)
}
