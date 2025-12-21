package pipeline

import (
	"fmt"
	"sync"

	"compiler/colors"
	"compiler/internal/context_v2"
)

// Pipeline coordinates the compilation process.
type Pipeline struct {
	ctx  *context_v2.CompilerContext
	seen sync.Map // map[string]struct{}
	wg   sync.WaitGroup
}

// New creates a new compilation pipeline.
func New(ctx *context_v2.CompilerContext) *Pipeline {
	return &Pipeline{ctx: ctx}
}

// Run executes the full compilation pipeline.
func (p *Pipeline) Run() error {
	if p.ctx.Config.Debug {
		colors.PrintAllColors()
		colors.CYAN.Printf("\n[Phase 1] Lex + Parse\n")
	}

	p.processModule(p.ctx.EntryModule, nil)
	p.wg.Wait()

	p.ctx.ComputeTopologicalOrder()

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("\n[Phase 2] Symbol Collection\n")
	}
	if err := p.runCollectorPhase(); err != nil {
		return err
	}

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("\n[Phase 3] Resolution\n")
	}
	if err := p.runResolverPhase(); err != nil {
		return err
	}

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("\n[Phase 4] Type Check\n")
	}
	if err := p.runTypeCheckerPhase(); err != nil {
		return err
	}

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("\n[Phase 5] HIR Generation\n")
	}

	if err := p.runHIRGenerationPhase(); err != nil {
		return err
	}

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("\n[Phase 6] Control Flow Analysis (HIR)\n")
	}

	if err := p.runCFGAnalysisPhase(); err != nil {
		return err
	}

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("\n[Phase 7] HIR Lowering\n")
	}

	if err := p.runHIRLoweringPhase(); err != nil {
		return err
	}

	if p.ctx.Config.Debug {
		colors.CYAN.Printf("\n[Phase 8] MIR Generation\n")
	}

	if err := p.runMIRGenerationPhase(); err != nil {
		return err
	}

	if p.ctx.HasErrors() {
		if p.ctx.Config.Debug {
			colors.YELLOW.Printf("\n[Skipping Code Generation] Errors detected in previous phases\n")
		}
		return fmt.Errorf("compilation failed with errors")
	}

	if p.ctx.Config.Debug {
		colors.YELLOW.Printf("\n[Skipping Code Generation] C backend detached (MIR is final for now)\n")
		colors.GREEN.Printf("\n✓ Compilation successful! (%d modules)\n", p.ctx.ModuleCount())
	}

	return nil
}
