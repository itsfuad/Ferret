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
	// Set explicit target to avoid ABI mismatches (QBE defaults to Darwin ABI with underscores)
	switch runtime.GOOS {
	case "windows":
		args = append(args, "-t", "amd64_win64")
	case "linux":
		args = append(args, "-t", "amd64_sysv")
	case "darwin":
		args = append(args, "-t", "amd64_apple")
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
