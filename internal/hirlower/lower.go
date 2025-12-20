package hirlower

import (
	"compiler/internal/context_v2"
	"compiler/internal/hir"
)

// Lowerer rewrites source-shaped HIR into a lowered, canonical form.
type Lowerer struct {
	ctx *context_v2.CompilerContext
}

// New creates a new HIR lowerer.
func New(ctx *context_v2.CompilerContext) *Lowerer {
	return &Lowerer{ctx: ctx}
}

// LowerModule performs a lowering pass over a HIR module.
// For now this is a structural pass with no rewrites; it establishes the
// pipeline stage where desugaring will live.
func (l *Lowerer) LowerModule(mod *hir.Module) *hir.Module {
	if mod == nil {
		return nil
	}

	// Shallow copy the module and items slice; nodes are reused until rewrites land.
	lowered := *mod
	if len(mod.Items) > 0 {
		lowered.Items = append([]hir.Node(nil), mod.Items...)
	}

	return &lowered
}
