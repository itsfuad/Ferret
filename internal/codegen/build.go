package codegen

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"

	"compiler/colors"
	"compiler/internal/context_v2"
)

// BuildOptions configures how to build the executable
type BuildOptions struct {
	CC          string   // C compiler (gcc, clang, etc.)
	CFlags      []string // Additional C flags
	RuntimePath string   // Path to runtime directory
	OutputPath  string   // Output executable path
	Debug       bool
}

// DefaultBuildOptions returns default build options
func DefaultBuildOptions() *BuildOptions {
	cc := "gcc"
	if runtime.GOOS == "darwin" {
		cc = "clang"
	}

	return &BuildOptions{
		CC:          cc,
		CFlags:      []string{"-std=c11", "-w"}, // -w suppresses all warnings
		RuntimePath: "runtime",
		Debug:       false,
	}
}

// BuildExecutable compiles the generated C code into an executable
func BuildExecutable(ctx *context_v2.CompilerContext, cFilePath string, opts *BuildOptions) error {
	if opts == nil {
		opts = DefaultBuildOptions()
	}

	// Determine runtime path
	runtimePath := opts.RuntimePath

	// If runtime path is already absolute and exists, use it
	if filepath.IsAbs(runtimePath) {
		if _, err := os.Stat(runtimePath); err == nil {
			// Path exists, use it
		} else {
			// Absolute path doesn't exist, try project root
			if ctx.Config.ProjectRoot != "" {
				possibleRuntime := filepath.Join(ctx.Config.ProjectRoot, "runtime")
				if absRuntime, err := filepath.Abs(possibleRuntime); err == nil {
					if _, err := os.Stat(absRuntime); err == nil {
						runtimePath = absRuntime
					}
				}
			}
		}
	} else {
		// Relative path - try project root first (most reliable)
		if ctx.Config.ProjectRoot != "" {
			possibleRuntime := filepath.Join(ctx.Config.ProjectRoot, "runtime")
			if absRuntime, err := filepath.Abs(possibleRuntime); err == nil {
				if _, err := os.Stat(absRuntime); err == nil {
					runtimePath = absRuntime
				}
			}
		}

		// Fallback: try relative to C file
		if _, err := os.Stat(runtimePath); os.IsNotExist(err) {
			cFileDir := filepath.Dir(cFilePath)
			possibleRuntime := filepath.Join(cFileDir, "..", "..", "runtime")
			if absRuntime, err := filepath.Abs(possibleRuntime); err == nil {
				if _, err := os.Stat(absRuntime); err == nil {
					runtimePath = absRuntime
				}
			}
		}

		// If still not found, try as absolute path
		if _, err := os.Stat(runtimePath); os.IsNotExist(err) {
			if absRuntime, err := filepath.Abs(runtimePath); err == nil {
				if _, err := os.Stat(absRuntime); err == nil {
					runtimePath = absRuntime
				}
			}
		}
	}

	// Check if runtime directory exists
	if _, err := os.Stat(runtimePath); os.IsNotExist(err) {
		return fmt.Errorf("runtime directory not found: %s", runtimePath)
	}

	// Build runtime library
	runtimeC := filepath.Join(runtimePath, "io.c")
	if _, err := os.Stat(runtimeC); os.IsNotExist(err) {
		return fmt.Errorf("runtime file not found: %s", runtimeC)
	}

	// Determine output path
	outputPath := opts.OutputPath
	if outputPath == "" {
		// Use same name as C file but without .c extension
		outputPath = cFilePath[:len(cFilePath)-2]
		if runtime.GOOS == "windows" {
			outputPath += ".exe"
		}
	}

	// Build command
	args := []string{}
	args = append(args, opts.CFlags...)
	args = append(args, "-o", outputPath)
	args = append(args, cFilePath)
	args = append(args, runtimeC)

	if ctx.Debug || opts.Debug {
		colors.CYAN.Printf("Building executable: %s %v\n", opts.CC, args)
	}

	// Run compiler
	cmd := exec.Command(opts.CC, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("compilation failed: %w", err)
	}

	if ctx.Debug || opts.Debug {
		colors.GREEN.Printf("  ✓ Built: %s\n", outputPath)
	}

	return nil
}
