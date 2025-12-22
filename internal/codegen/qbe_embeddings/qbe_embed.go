//go:build cgo

package qbe

/*
#cgo CFLAGS: -I${SRCDIR} -std=c99
#include <stdlib.h>
#include "qbe_embed.h"
*/
import "C"

import (
	"fmt"
	"unsafe"
)

func runQBE(args []string) (int, error) {
	if len(args) == 0 {
		return 1, fmt.Errorf("qbe: missing arguments")
	}

	cargs := make([]*C.char, len(args))
	for i, arg := range args {
		cargs[i] = C.CString(arg)
	}
	defer func() {
		for _, cstr := range cargs {
			C.free(unsafe.Pointer(cstr))
		}
	}()

	code := C.ferret_qbe_run(C.int(len(cargs)), (**C.char)(unsafe.Pointer(&cargs[0])))
	return int(code), nil
}
