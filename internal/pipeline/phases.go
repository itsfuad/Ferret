package pipeline

import (
	"fmt"

	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/hir"
	hiranalysis "compiler/internal/hir/analysis"
	"compiler/internal/phase"
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
	runTopLevelSignatureTypeCheckPhase(p)

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

// runTopLevelSignatureTypeCheckPhase resolves top-level types, methods, and function signatures
// so forward references at module scope type check correctly.
func runTopLevelSignatureTypeCheckPhase(p *Pipeline) error {
	for _, importPath := range p.ctx.GetModuleNames() {
		module, exists := p.ctx.GetModule(importPath)
		if !exists {
			continue
		}

		if p.ctx.GetModulePhase(importPath) < phase.PhaseResolved {
			continue
		}

		typechecker.TypeCheckTopLevelSignatures(p.ctx, module)

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

		if p.ctx.GetModulePhase(importPath) < phase.PhaseHIRGenerated {
			continue
		}

		// Skip the global prelude module (synthetic, has no HIR)
		if importPath == context_v2.GlobalModuleImport {
			if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseCFGAnalyzed) {
				p.ctx.SetModulePhase(importPath, phase.PhaseCFGAnalyzed)
			}
			continue
		}

		hirMod := hir.ModuleFromModule(module)
		if hirMod == nil {
			p.ctx.ReportError(fmt.Sprintf("HIR module missing for %s during CFG analysis", importPath), nil)
			continue
		}

		hiranalysis.AnalyzeModule(p.ctx, module, hirMod)

		if !p.ctx.AdvanceModulePhase(importPath, phase.PhaseCFGAnalyzed) {
			p.ctx.ReportError(fmt.Sprintf("cannot advance module %s to PhaseCFGAnalyzed", importPath), nil)
		}

		if p.ctx.Config.Debug {
			colors.PURPLE.Printf("  ✓ %s\n", importPath)
		}
	}

	return nil
}
