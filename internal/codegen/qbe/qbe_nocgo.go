//go:build !cgo

package qbe

import "fmt"

func runQBE(args []string) (int, error) {
	return 1, fmt.Errorf("qbe backend requires cgo")
}
