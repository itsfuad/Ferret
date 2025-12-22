package qbe

import (
	"fmt"

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

	args := []string{"qbe", "-o", outputPath, inputPath}
	code, err := runQBE(args)
	if err != nil {
		return err
	}
	if code != 0 {
		return fmt.Errorf("qbe failed with exit code %d", code)
	}

	return nil
}
