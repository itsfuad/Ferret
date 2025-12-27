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
	utilsfs "compiler/internal/utils/fs"
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

	cflags := []string{"-std=c99", "-w", "-Os", "-flto", "-s"}
	switch runtime.GOOS {
	case "linux":
		cflags = append(cflags, "-no-pie")
	case "darwin":
		cflags = append(cflags, "-Wl,-no_pie")
	case "openbsd":
		cflags = append(cflags, "-nopie")
	case "freebsd":
		// FreeBSD defaults are acceptable; no PIE flag needed.
	}

	return &BuildOptions{
		CC:          cc,
		CFlags:      cflags, // -Os: optimize for size, -flto: link-time optimization, -s: strip symbols
		RuntimePath: "runtime",
		Debug:       false,
	}
}

// BuildExecutable compiles multiple C files into an executable
func BuildExecutable(ctx *context_v2.CompilerContext, cFiles []string, includeDir string, opts *BuildOptions) error {
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
	if !utilsfs.IsDir(runtimePath) {
		return fmt.Errorf("runtime directory not found: %s", runtimePath)
	}

	// Build runtime libraries
	runtimeFiles := []string{
		filepath.Join(runtimePath, "alloc.c"),
		filepath.Join(runtimePath, "cast.c"),
		filepath.Join(runtimePath, "io.c"),
		filepath.Join(runtimePath, "interface.c"),
		filepath.Join(runtimePath, "map.c"),            // Hash map library
		filepath.Join(runtimePath, "bigint.c"),         // Big integer library (128/256-bit)
		filepath.Join(runtimePath, "optional.c"),       // Optional helpers
		filepath.Join(runtimePath, "array.c"),          // Dynamic array library
		filepath.Join(runtimePath, "panic.c"),          // Panic helper
		filepath.Join(runtimePath, "time.c"),           // Time helpers
		filepath.Join(runtimePath, "string_runtime.c"), // String helpers
		filepath.Join(runtimePath, "string_builder.c"), // String builder library
	}

	// Check that all runtime files exist
	for _, runtimeFile := range runtimeFiles {
		if !utilsfs.IsValidFile(runtimeFile) {
			// Some runtime files are optional (array.c, string_builder.c)
			// Only io.c, interface.c, and bigint.c are required
			if strings.HasSuffix(runtimeFile, "alloc.c") || strings.HasSuffix(runtimeFile, "cast.c") || strings.HasSuffix(runtimeFile, "io.c") || strings.HasSuffix(runtimeFile, "interface.c") || strings.HasSuffix(runtimeFile, "bigint.c") || strings.HasSuffix(runtimeFile, "optional.c") || strings.HasSuffix(runtimeFile, "string_runtime.c") {
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
		if utilsfs.IsValidFile(runtimeFile) {
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
