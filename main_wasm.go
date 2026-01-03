//go:build js && wasm

package main

import (
	"encoding/base64"
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
		Code:             args[0].String(),
		Debug:            args[1].Bool(),
		LogFormat:        compiler.HTML,
		CodegenBackend:   "wasm",
		OutputExecutable: "out.wasm",
	})

	wasm := ""
	if len(result.Artifact) > 0 {
		wasm = base64.StdEncoding.EncodeToString(result.Artifact)
	}

	return map[string]any{
		"success": result.Success,
		"output":  result.Output,
		"wasm":    wasm,
	}
}
