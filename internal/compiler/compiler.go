package compiler

import (
	"fmt"
	"os"
	"path/filepath"

	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/pipeline"
)

type FORMAT int

const (
	ANSI FORMAT = iota
	HTML
)

// Options for compilation
type Options struct {
	// For file-based compilation
	EntryFile string
	// For in-memory compilation (WASM)
	Code string
	// Debug output
	Debug bool
	// Output format: "ansi" or "html"
	LogFormat FORMAT
}

// Result of compilation
type Result struct {
	Success bool
	Output  string
}

// Compile compiles Ferret code and returns the result
func Compile(opts Options) Result {
	// Setup compiler config
	projectName := "playground"
	projectRoot := "/virtual"

	if opts.EntryFile != "" {
		absPath, err := filepath.Abs(opts.EntryFile)
		if err != nil {
			return Result{Success: false, Output: fmt.Sprintf("Failed to resolve path: %v", err)}
		}

		if _, err := os.Stat(absPath); os.IsNotExist(err) {
			return Result{Success: false, Output: fmt.Sprintf("File not found: %s", opts.EntryFile)}
		}

		projectRoot = filepath.Dir(absPath)
		projectName = filepath.Base(projectRoot)
	}

	execPath, _ := os.Executable()
	builtinPath := filepath.Join(filepath.Dir(execPath), "../ferret_libs")

	config := &context_v2.Config{
		ProjectName:        projectName,
		ProjectRoot:        projectRoot,
		Extension:          ".fer",
		BuiltinModulesPath: builtinPath,
		OutputPath:         filepath.Join(projectRoot, "bin", projectName),
	}

	ctx := context_v2.New(config, opts.Debug)

	// Set entry point
	var err error
	if opts.Code != "" {
		err = ctx.SetEntryPointWithCode(opts.Code, "main")
	} else {
		err = ctx.SetEntryPoint(opts.EntryFile)
	}

	if err != nil {
		ctx.ReportError(fmt.Sprintf("Failed to set entry point: %v", err), nil)
		if opts.LogFormat == HTML {
			return Result{Success: false, Output: ctx.Diagnostics.EmitAllToString()}
		}
		ctx.EmitDiagnostics()
		return Result{Success: false}
	}

	// Run pipeline
	p := pipeline.New(ctx)
	p.Run()

	// Emit diagnostics and return result
	if opts.LogFormat == HTML {
		output := ctx.Diagnostics.EmitAllToString()
		return Result{Success: !ctx.HasErrors(), Output: colors.ConvertANSIToHTML(output)}
	}

	ctx.EmitDiagnostics()
	return Result{Success: !ctx.HasErrors()}
}
