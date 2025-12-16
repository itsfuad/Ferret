//go:build !js && !wasm

package main

import (
	"flag"
	"fmt"
	"os"

	"compiler/internal/compiler"
)

const version = "0.0.2-alpha"

func main() {
	// Define flags
	debug := flag.Bool("d", false, "Enable debug output")
	showVersion := flag.Bool("v", false, "Show version")
	saveAST := flag.Bool("ast", false, "Save AST")
	help := flag.Bool("h", false, "Show help")
	outputPath := flag.String("o", "", "Output executable path")
	keepCFile := flag.Bool("c", false, "Keep generated C files")
	typecheckOnly := flag.Bool("t", false, "Stop after type checking (skip codegen)")
	flag.BoolVar(debug, "debug", false, "Enable debug output")
	flag.BoolVar(showVersion, "version", false, "Show version")
	flag.BoolVar(help, "help", false, "Show help")
	flag.BoolVar(keepCFile, "keep-c", false, "Keep generated C files")
	flag.BoolVar(typecheckOnly, "typecheck", false, "Stop after type checking (skip codegen)")

	flag.Parse()

	// Handle version
	if *showVersion {
		fmt.Printf("Ferret compiler version %s\n", version)
		os.Exit(0)
	}

	// Handle help
	if *help {
		fmt.Fprintln(os.Stdout, "Usage: ferret [options] <file>")
		fmt.Fprintln(os.Stdout, "\nNote: Flags must come before the file argument")
		fmt.Fprintln(os.Stdout, "\nOptions:")
		flag.PrintDefaults()
		os.Exit(0)
	}

	// Get entry file
	args := flag.Args()
	if len(args) < 1 {
		fmt.Fprintln(os.Stderr, "Usage: ferret [options] <file>")
		fmt.Fprintln(os.Stderr, "\nNote: Flags must come before the file argument")
		fmt.Fprintln(os.Stderr, "\nOptions:")
		flag.PrintDefaults()
		os.Exit(1)
	}

	// Check if there are any arguments that look like flags after the file
	// This helps catch common mistakes like: ferret file.fer -c
	if len(args) > 1 {
		for _, arg := range args[1:] {
			if len(arg) > 0 && arg[0] == '-' {
				fmt.Fprintf(os.Stderr, "Warning: Flag '%s' appears after the file argument. Flags must come before the file.\n", arg)
				fmt.Fprintf(os.Stderr, "Use: ferret [options] <file>\n")
			}
		}
	}

	entryFile := args[0]

	// Compile
	result := compiler.Compile(&compiler.Options{
		EntryFile:        entryFile,
		Debug:            *debug,
		SaveAST:          *saveAST,
		LogFormat:        compiler.ANSI,
		OutputExecutable: *outputPath,
		KeepCFile:        *keepCFile,
		SkipCodegen:      *typecheckOnly,
	})

	// Exit code
	if !result.Success {
		os.Exit(1)
	}
}
