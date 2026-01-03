package pipeline

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"

	"compiler/colors"
	"compiler/internal/codegen/wasm"
	"compiler/internal/mir"
)

// runWasmCodegenPhase runs the WASM backend and writes a single .wasm module.
func (p *Pipeline) runWasmCodegenPhase() error {
	if err := p.ensureEntryMain(); err != nil {
		return err
	}

	modulesToGenerate := p.collectModulesForCodegen()
	if len(modulesToGenerate) == 0 {
		return fmt.Errorf("wasm: no modules to generate")
	}

	units := make([]wasm.Unit, 0, len(modulesToGenerate))
	entryModule := p.ctx.EntryModule

	for _, importPath := range modulesToGenerate {
		mod, ok := p.ctx.GetModule(importPath)
		if !ok || mod == nil {
			return fmt.Errorf("wasm: module not found: %s", importPath)
		}
		mirMod := mir.ModuleFromModule(mod)
		if mirMod == nil {
			return fmt.Errorf("wasm: missing MIR module for %s", importPath)
		}
		mir.LowerSwitches(mirMod)
		units = append(units, wasm.Unit{
			Module:  mod,
			MIR:     mirMod,
			IsEntry: importPath == entryModule,
		})
	}

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("  Generating WASM for %d modules\n", len(units))
	}

	wasmBytes, err := wasm.EmitProgram(p.ctx, units)
	if err != nil {
		p.ctx.ReportError(fmt.Sprintf("wasm codegen failed: %v", err), nil)
		return err
	}
	p.ctx.CodegenOutput = wasmBytes

	if runtime.GOOS == "js" {
		return nil
	}

	outputPath := p.ctx.Config.OutputPath
	if outputPath == "" {
		return fmt.Errorf("wasm: output path must be specified")
	}
	if err := os.MkdirAll(filepath.Dir(outputPath), 0755); err != nil {
		p.ctx.ReportError(fmt.Sprintf("failed to create wasm output directory: %v", err), nil)
		return err
	}
	if err := os.WriteFile(outputPath, wasmBytes, 0644); err != nil {
		p.ctx.ReportError(fmt.Sprintf("failed to write wasm output: %v", err), nil)
		return err
	}

	if p.ctx.Config.Debug {
		colors.GREEN.Printf("  ✓ Wrote WASM module: %s\n", outputPath)
	}

	return nil
}
