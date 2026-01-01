package qbe

import (
	"fmt"
	"runtime"

	"compiler/internal/context_v2"
)

// Run invokes the embedded QBE backend to compile SSA into assembly.
func Run(ctx *context_v2.CompilerContext, inputPath, outputPath string) error {
	if inputPath == "" {
		return fmt.Errorf("qbe: missing input path")
	}
	if outputPath == "" {
		return fmt.Errorf("qbe: missing output path")
	}

	args := []string{"qbe"}
	// Set explicit target based on OS and architecture
	// QBE targets: amd64_sysv (Linux/BSD/macOS x86_64), amd64_win64 (Windows), arm64 (Linux/macOS ARM64)
	switch runtime.GOOS {
	case "windows":
		args = append(args, "-t", "amd64_win64")
	case "linux", "freebsd", "openbsd", "netbsd":
		if runtime.GOARCH == "arm64" {
			args = append(args, "-t", "arm64")
		} else {
			args = append(args, "-t", "amd64_sysv")
		}
	case "darwin":
		if runtime.GOARCH == "arm64" {
			args = append(args, "-t", "arm64") // Apple Silicon (M1/M2/M3/M4)
		} else {
			args = append(args, "-t", "amd64_sysv") // Intel Mac uses System V ABI
		}
	}
	args = append(args, "-o", outputPath, inputPath)
	code, err := runQBE(args)
	if err != nil {
		return err
	}
	if code != 0 {
		return fmt.Errorf("qbe failed with exit code %d", code)
	}

	return nil
}
