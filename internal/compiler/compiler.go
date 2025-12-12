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
	Debug   bool
	SaveAST bool
	// Output format: "ansi" or "html"
	LogFormat FORMAT
	// Output executable path (if empty, uses default: <projectRoot>/bin/<projectName>)
	OutputExecutable string
	// Keep generated C file after compilation
	KeepCFile bool
}

// Result of compilation
type Result struct {
	Success bool
	Output  string
}

// Compile compiles Ferret code and returns the result
func Compile(opts *Options) Result {
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

		// Find the actual project root (where runtime/ directory is)
		// Start from the entry file's directory and walk up until we find runtime/
		entryDir := filepath.Dir(absPath)
		currentDir := entryDir
		for {
			runtimePath := filepath.Join(currentDir, "runtime")
			if _, err := os.Stat(runtimePath); err == nil {
				// Found runtime directory, this is the project root
				projectRoot = currentDir
				break
			}
			parent := filepath.Dir(currentDir)
			if parent == currentDir {
				// Reached filesystem root, use entry file's directory as fallback
				projectRoot = entryDir
				break
			}
			currentDir = parent
		}
		projectName = filepath.Base(entryDir)
	}

	execPath, _ := os.Executable()
	execDir := filepath.Dir(execPath)
	builtinPath := filepath.Join(execDir, "../ferret_libs")
	runtimePath := filepath.Join(execDir, "../runtime")

	// Determine output path
	outputPath := filepath.Join(projectRoot, "bin", projectName)
	if opts.OutputExecutable != "" {
		// Use user-specified path
		outputPath = opts.OutputExecutable
		// Make it absolute if it's not already
		if !filepath.IsAbs(outputPath) {
			if absPath, err := filepath.Abs(outputPath); err == nil {
				outputPath = absPath
			}
		}
	}

	config := &context_v2.Config{
		ProjectName:        projectName,
		ProjectRoot:        projectRoot,
		Extension:          ".fer",
		BuiltinModulesPath: builtinPath,
		RuntimePath:        runtimePath, // Runtime path relative to executable
		OutputPath:         outputPath,
		KeepCFile:          opts.KeepCFile,
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
