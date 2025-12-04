//go:build js && wasm

package main

import (
	"syscall/js"

	"compiler/internal/compiler"
)

func main() {
	js.Global().Set("ferretCompile", js.FuncOf(compile))
	js.Global().Set("ferretWasmVersion", "0.0.4")
	println("Ferret WASM compiler ready")
	<-make(chan struct{})
}

func compile(this js.Value, args []js.Value) interface{} {
	if len(args) < 2 {
		return map[string]interface{}{
			"success": false,
			"output":  "Invalid arguments: expected (code: string, debug: bool)",
		}
	}

	result := compiler.Compile(&compiler.Options{
		Code:      args[0].String(),
		Debug:     args[1].Bool(),
		LogFormat: compiler.HTML,
	})

	return map[string]interface{}{
		"success": result.Success,
		"output":  result.Output,
	}
}
