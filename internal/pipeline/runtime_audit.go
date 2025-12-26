package pipeline

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"

	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/utils/fs"
)

var runtimeSymbolPattern = regexp.MustCompile(`(?m)^[A-Za-z_][A-Za-z0-9_\s\*]*\b(ferret_[A-Za-z0-9_]+)\s*\(`)

func (p *Pipeline) runRuntimeAudit() {
	if p.ctx == nil || p.ctx.Config == nil {
		return
	}

	runtimePath := p.ctx.Config.RuntimePath
	if runtimePath == "" || !fs.IsDir(runtimePath) {
		return
	}

	available := collectRuntimeSymbols(runtimePath)
	if len(available) == 0 {
		return
	}

	for _, mod := range p.ctx.Modules {
		if mod == nil || mod.Type != context_v2.ModuleBuiltin || mod.ImportPath == context_v2.GlobalModuleImport {
			continue
		}
		if mod.ModuleScope == nil {
			continue
		}
		for _, sym := range mod.ModuleScope.GetAllSymbols() {
			if sym == nil || !sym.IsNative || sym.NativeName == "" {
				continue
			}
			if _, ok := available[sym.NativeName]; ok {
				continue
			}

			diag := diagnostics.NewError(fmt.Sprintf("no runtime implementation for %s", sym.Name)).
				WithNote(fmt.Sprintf("module: %s", mod.ImportPath)).
				WithNote(fmt.Sprintf("expected symbol: %s", sym.NativeName))
			if sym.Decl != nil {
				diag = diag.WithPrimaryLabel(sym.Decl.Loc(), "missing runtime implementation")
			}
			p.ctx.Diagnostics.Add(diag)
		}
	}
}

func collectRuntimeSymbols(runtimePath string) map[string]struct{} {
	symbols := make(map[string]struct{})
	_ = filepath.WalkDir(runtimePath, func(path string, entry os.DirEntry, err error) error {
		if err != nil {
			return nil
		}
		if entry.IsDir() || filepath.Ext(path) != ".c" {
			return nil
		}
		data, readErr := os.ReadFile(path)
		if readErr != nil {
			return nil
		}
		matches := runtimeSymbolPattern.FindAllSubmatch(data, -1)
		for _, match := range matches {
			if len(match) < 2 {
				continue
			}
			symbols[string(match[1])] = struct{}{}
		}
		return nil
	})
	return symbols
}
