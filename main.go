//go:build !js && !wasm

package main

import (
	"flag"
	"fmt"
	"os"

	"compiler/internal/compiler"
)

const version = "0.1.0"

func main() {
	// Define flags
	debug := flag.Bool("d", false, "Enable debug output")
	showVersion := flag.Bool("v", false, "Show version")
	flag.BoolVar(debug, "debug", false, "Enable debug output")
	flag.BoolVar(showVersion, "version", false, "Show version")

	flag.Parse()

	// Handle version
	if *showVersion {
		fmt.Printf("Ferret compiler version %s\n", version)
		os.Exit(0)
	}

	// Get entry file
	args := flag.Args()
	if len(args) < 1 {
		fmt.Fprintln(os.Stderr, "Usage: ferret [options] <file>")
		fmt.Fprintln(os.Stderr, "\nOptions:")
		flag.PrintDefaults()
		os.Exit(1)
	}

	entryFile := args[0]

	// Compile
	result := compiler.Compile(compiler.Options{
		EntryFile: entryFile,
		Debug:     *debug,
		LogFormat: compiler.ANSI,
	})

	// Exit code
	if !result.Success {
		os.Exit(1)
	}
}
