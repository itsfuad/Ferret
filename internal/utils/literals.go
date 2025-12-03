package utils

import (
	"fmt"
	"sync/atomic"
)

// Global counter for generating unique literal IDs
var literalCounter int64

// GenerateFuncLitID generates a unique ID for function literals
func GenerateFuncLitID() string {
	id := atomic.AddInt64(&literalCounter, 1)
	return fmt.Sprintf("__literal_fn_%d", id)
}