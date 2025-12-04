package utils

import (
	"fmt"
	"sync/atomic"
)

const (
	FN = "__func_lit__"
	ST = "__struct_lit__"
)

// Global counter for generating unique literal IDs
var litCountermap = map[string]*int64{
	FN:   new(int64),
	ST: new(int64),
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