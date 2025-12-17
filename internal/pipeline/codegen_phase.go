package pipeline

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"compiler/colors"
	"compiler/internal/codegen"
	"compiler/internal/codegen/cgen"
	"compiler/internal/context_v2"
	"compiler/internal/phase"
	ustrings "compiler/internal/utils/strings"
)

// runCodegenPhase runs code generation on all type-checked modules
func (p *Pipeline) runCodegenPhase() error {
	outputPath := p.ctx.Config.OutputPath
	outputDir := filepath.Dir(outputPath)

	tempDir := filepath.Join(outputDir, "gen")
	if err := os.MkdirAll(tempDir, 0755); err != nil {
		return fmt.Errorf("failed to create temp directory: %w", err)
	}

	modulesToGenerate := p.collectModulesForCodegen()

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("  Generating code for %d modules: %v\n", len(modulesToGenerate), modulesToGenerate)
	}

	entryModule, importedModules := splitEntryModule(modulesToGenerate, p.ctx.EntryModule)

	var cFiles []string
	for _, importPath := range importedModules {
		if generated, err := p.generateModuleCode(importPath, tempDir, nil); err == nil {
			cFiles = append(cFiles, generated...)
		}
	}

	if entryModule != "" {
		if generated, err := p.generateModuleCode(entryModule, tempDir, importedModules); err == nil {
			cFiles = append(cFiles, generated...)
		}
	}

	execPath := p.ctx.Config.OutputPath
	if runtime.GOOS == "windows" && !strings.HasSuffix(execPath, ".exe") {
		defaultPattern := filepath.Join(p.ctx.Config.ProjectRoot, "bin", p.ctx.Config.ProjectName)
		if execPath == defaultPattern {
			execPath += ".exe"
		}
	}

	if err := p.buildExecutableMultiple(cFiles, execPath, tempDir); err != nil {
		p.ctx.ReportError(fmt.Sprintf("build failed: %v", err), nil)
		return err
	}

	if !p.ctx.Config.KeepCFile {
		if err := os.RemoveAll(tempDir); err != nil {
			if p.ctx.Config.Debug {
				colors.YELLOW.Printf("  ⚠ Could not remove temp directory: %v\n", err)
			}
		} else if p.ctx.Config.Debug {
			colors.GREEN.Printf("  ✓ Removed temp directory: %s\n", tempDir)
		}
	} else if p.ctx.Config.Debug {
		colors.CYAN.Printf("  ℹ Keeping generated files in: %s\n", tempDir)
	}

	if p.ctx.Config.Debug {
		colors.PURPLE.Printf("  ✓ Code generation complete\n")
	}

	return nil
}

func (p *Pipeline) collectModulesForCodegen() []string {
	modulesToGenerate := []string{p.ctx.EntryModule}
	for _, importPath := range p.ctx.GetModuleNames() {
		if importPath == p.ctx.EntryModule {
			continue
		}
		module, exists := p.ctx.GetModule(importPath)
		if !exists || module.Type == context_v2.ModuleBuiltin {
			continue
		}

		modulePhase := p.ctx.GetModulePhase(importPath)
		if modulePhase >= phase.PhaseTypeChecked && module.Type == context_v2.ModuleLocal {
			modulesToGenerate = append(modulesToGenerate, importPath)
			if p.ctx.Config.Debug {
				colors.CYAN.Printf("  Including module: %s (phase: %s, type: %s)\n", importPath, modulePhase, module.Type)
			}
		} else if p.ctx.Config.Debug {
			colors.YELLOW.Printf("  Skipping module: %s (phase: %s, type: %s)\n", importPath, modulePhase, module.Type)
		}
	}
	return modulesToGenerate
}

func splitEntryModule(mods []string, entry string) (string, []string) {
	var entryModule string
	var importedModules []string
	for _, m := range mods {
		if m == entry {
			entryModule = m
		} else {
			importedModules = append(importedModules, m)
		}
	}
	return entryModule, importedModules
}

// generateModuleCode emits header/implementation for a module and returns .c files generated
func (p *Pipeline) generateModuleCode(importPath, tempDir string, imports []string) ([]string, error) {
	module, exists := p.ctx.GetModule(importPath)
	if !exists {
		return nil, fmt.Errorf("module not found: %s", importPath)
	}

	gen := cgen.New(p.ctx, module)
	headerContent := codegen.FormatCSource(gen.GenerateHeader())
	headerName := p.sanitizeModuleName(importPath) + ".h"
	headerPath := filepath.Join(tempDir, headerName)
	if err := os.WriteFile(headerPath, []byte(headerContent), 0644); err != nil {
		p.ctx.ReportError(fmt.Sprintf("failed to write header for %s: %v", importPath, err), nil)
		return nil, err
	}

	implContent := gen.GenerateImplementation()
	var implBuilder strings.Builder
	implBuilder.WriteString("// Generated C code from Ferret\n")
	implBuilder.WriteString("// Module: " + importPath + "\n\n")
	codegen.WriteStandardIncludes(&implBuilder)
	codegen.WriteRuntimeIncludes(&implBuilder)
	if len(imports) > 0 {
		for _, importPath := range imports {
			headerName := p.sanitizeModuleName(importPath) + ".h"
			implBuilder.WriteString("#include \"" + headerName + "\"\n")
		}
	}
	implBuilder.WriteString("#include \"" + headerName + "\"\n\n")
	implBuilder.WriteString(implContent)

	implName := p.sanitizeModuleName(importPath) + ".c"
	implPath := filepath.Join(tempDir, implName)
	implOut := codegen.FormatCSource(implBuilder.String())
	if err := os.WriteFile(implPath, []byte(implOut), 0644); err != nil {
		p.ctx.ReportError(fmt.Sprintf("failed to write implementation for %s: %v", importPath, err), nil)
		return nil, err
	}

	if p.ctx.Config.Debug {
		colors.GREEN.Printf("  ✓ Generated: %s, %s\n", headerName, implName)
	}

	return []string{implPath}, nil
}

func (p *Pipeline) sanitizeModuleName(importPath string) string {
	return ustrings.ToIdentifier(importPath)
}

func (p *Pipeline) buildExecutableMultiple(cFiles []string, execPath, includeDir string) error {
	opts := codegen.DefaultBuildOptions()
	if p.ctx.Config.RuntimePath != "" {
		opts.RuntimePath = p.ctx.Config.RuntimePath
	}
	opts.OutputPath = execPath
	opts.Debug = p.ctx.Config.Debug

	return codegen.BuildExecutable(p.ctx, cFiles, includeDir, opts)
}
