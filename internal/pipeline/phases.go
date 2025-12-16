package pipeline

import (
	"fmt"

	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/phase"
	"compiler/internal/semantics/cfganalyzer"
	"compiler/internal/semantics/collector"
	"compiler/internal/semantics/resolver"
	"compiler/internal/semantics/typechecker"
)

func (p *Pipeline) runCollectorPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		if p.ctx.GetModulePhase(importPath) < phase.PhaseParsed {
			continue
		}

		if module.Type == context_v2.ModuleBuiltin && module.AST != nil && len(module.AST.Nodes) == 0 {
			if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseCollected) {
				p.ctx.SetModulePhase(importPath, phase.PhaseCollected)
			}
			if p.ctx.Config.Debug {
				colors.PURPLE.Printf("  ✓ %s (native)\n", importPath)
			}
			continue
		}

		collector.CollectModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseCollected) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseCollected", importPath), nil)
		}

		if p.ctx.Config.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}
	return nil
}

func (p *Pipeline) runResolverPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		if p.ctx.GetModulePhase(importPath) < phase.PhaseCollected {
			continue
		}

		mod, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		if mod.Type == context_v2.ModuleBuiltin && mod.AST != nil && len(mod.AST.Nodes) == 0 {
			if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseResolved) {
				p.ctx.SetModulePhase(importPath, phase.PhaseResolved)
			}
			if p.ctx.Config.Debug {
				colors.PURPLE.Printf("  ✓ %s (native)\n", importPath)
			}
			continue
		}

		resolver.ResolveModule(p.ctx, mod)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseResolved) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseResolved", importPath), nil)
		}

		if p.ctx.Config.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}
	return nil
}

func (p *Pipeline) runTypeCheckerPhase() error {
	runMethodSignatureTypeCheckPhase(p)

	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		if p.ctx.GetModulePhase(importPath) < phase.PhaseResolved {
			continue
		}

		if module.Type == context_v2.ModuleBuiltin && module.AST != nil && len(module.AST.Nodes) == 0 {
			if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseTypeChecked) {
				p.ctx.SetModulePhase(importPath, phase.PhaseTypeChecked)
			}
			if p.ctx.Config.Debug {
				colors.PURPLE.Printf("  ✓ %s (native)\n", importPath)
			}
			continue
		}

		typechecker.CheckModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseTypeChecked) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseTypeChecked", importPath), nil)
		}

		if p.ctx.Config.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	return nil
}

// runMethodSignatureTypeCheckPhase type checks only method signatures (attaches methods to types)
func runMethodSignatureTypeCheckPhase(p *Pipeline) error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		if p.ctx.GetModulePhase(importPath) < phase.PhaseResolved {
			continue
		}

		typechecker.TypeCheckMethodSignatures(p.ctx, module)

		if p.ctx.Config.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	return nil
}

func (p *Pipeline) runCFGAnalysisPhase() error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		if p.ctx.GetModulePhase(importPath) < phase.PhaseTypeChecked {
			continue
		}

		cfganalyzer.AnalyzeModule(p.ctx, module)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseCFGAnalyzed) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseCFGAnalyzed", importPath), nil)
		}

		if p.ctx.Config.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	return nil
}
