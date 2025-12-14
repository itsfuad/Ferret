package codegen

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

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
		CFlags:      []string{"-std=c11", "-w", "-Os", "-flto", "-s"}, // -Os: optimize for size, -flto: link-time optimization, -s: strip symbols
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
	// Add include directory for runtime headers
	args = append(args, "-I", runtimePath)
	args = append(args, "-o", outputPath)
	args = append(args, cFilePath)
	args = append(args, runtimeC)
	args = append(args, "-lm") // Link math library for pow() function

	if ctx.Config.Debug || opts.Debug {
		colors.CYAN.Printf("Building executable: %s %v\n", opts.CC, args)
	}

	// Run compiler
	cmd := exec.Command(opts.CC, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("compilation failed: %w", err)
	}

	if ctx.Config.Debug || opts.Debug {
		colors.GREEN.Printf("  ✓ Built: %s\n", outputPath)
	}

	return nil
}

// BuildExecutableMultiple compiles multiple C files into an executable
func BuildExecutableMultiple(ctx *context_v2.CompilerContext, cFiles []string, includeDir string, opts *BuildOptions) error {
	if opts == nil {
		opts = DefaultBuildOptions()
	}

	// Determine runtime path - use from config if available, otherwise from opts
	runtimePath := opts.RuntimePath
	if runtimePath == "" && ctx.Config.RuntimePath != "" {
		runtimePath = ctx.Config.RuntimePath
	}

	// Make it absolute
	if !filepath.IsAbs(runtimePath) {
		if absRuntime, err := filepath.Abs(runtimePath); err == nil {
			runtimePath = absRuntime
		}
	}

	// Check if runtime directory exists
	if _, err := os.Stat(runtimePath); os.IsNotExist(err) {
		return fmt.Errorf("runtime directory not found: %s", runtimePath)
	}

	// Build runtime libraries
	runtimeFiles := []string{
		filepath.Join(runtimePath, "io.c"),
		filepath.Join(runtimePath, "interface.c"),
		filepath.Join(runtimePath, "bigint.c"),        // Big integer library (128/256-bit)
		filepath.Join(runtimePath, "array.c"),        // Dynamic array library
		filepath.Join(runtimePath, "string_builder.c"), // String builder library
	}

	// Check that all runtime files exist
	for _, runtimeFile := range runtimeFiles {
		if _, err := os.Stat(runtimeFile); os.IsNotExist(err) {
		// Some runtime files are optional (array.c, string_builder.c)
		// Only io.c, interface.c, and bigint.c are required
		if strings.HasSuffix(runtimeFile, "io.c") || strings.HasSuffix(runtimeFile, "interface.c") || strings.HasSuffix(runtimeFile, "bigint.c") {
			return fmt.Errorf("required runtime file not found: %s", runtimeFile)
		}
		}
	}

	// Determine output path
	outputPath := opts.OutputPath
	if outputPath == "" {
		return fmt.Errorf("output path must be specified")
	}

	// Build command
	args := []string{}
	args = append(args, opts.CFlags...)
	// Add include directories
	args = append(args, "-I", runtimePath)
	args = append(args, "-I", includeDir)
	args = append(args, "-o", outputPath)
	// Add all C files
	args = append(args, cFiles...)
	// Add runtime files (only include if they exist)
	for _, runtimeFile := range runtimeFiles {
		if _, err := os.Stat(runtimeFile); err == nil {
			args = append(args, runtimeFile)
		}
	}
	args = append(args, "-lm") // Link math library for pow() function

	if ctx.Config.Debug || opts.Debug {
		colors.CYAN.Printf("Building executable: %s %v\n", opts.CC, args)
	}

	// Run compiler
	cmd := exec.Command(opts.CC, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("compilation failed: %w", err)
	}

	if ctx.Config.Debug || opts.Debug {
		colors.GREEN.Printf("  ✓ Built: %s\n", outputPath)
	}

	return nil
}
