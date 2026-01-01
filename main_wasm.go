//go:build js && wasm

package main

import (
	"syscall/js"

	"compiler/internal/compiler"
)

func main() {
	js.Global().Set("ferretCompile", js.FuncOf(compile))
	js.Global().Set("ferretWasmVersion", "0.0.2")
	println("Ferret WASM compiler ready")
	<-make(chan struct{})
}

func compile(this js.Value, args []js.Value) any {
	if len(args) < 2 {
		return map[string]any{
			"success": false,
			"output":  "Invalid arguments: expected (code: string, debug: bool)",
		}
	}

	result := compiler.Compile(&compiler.Options{
		Code:      args[0].String(),
		Debug:     args[1].Bool(),
		LogFormat: compiler.HTML,
	})

	return map[string]any{
		"success": result.Success,
		"output":  result.Output,
	}
}
