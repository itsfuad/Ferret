package mirgen

import (
	"compiler/internal/context_v2"
	"compiler/internal/hir"
	"compiler/internal/mir"
	"compiler/internal/source"
	"compiler/internal/types"
)

// Generator lowers lowered HIR into MIR.
type Generator struct {
	ctx       *context_v2.CompilerContext
	mod       *context_v2.Module
	layout    *mir.DataLayout
	nextValue mir.ValueID
	nextBlock mir.BlockID
}

// New creates a new MIR generator for a module.
func New(ctx *context_v2.CompilerContext, mod *context_v2.Module) *Generator {
	pointerSize := 8
	if ctx != nil && ctx.Config != nil && ctx.Config.PointerSize > 0 {
		pointerSize = ctx.Config.PointerSize
	}
	return &Generator{
		ctx:    ctx,
		mod:    mod,
		layout: mir.NewDataLayout(pointerSize),
	}
}

// GenerateModule lowers a lowered HIR module into MIR.
func (g *Generator) GenerateModule(hirMod *hir.Module) *mir.Module {
	if g == nil || g.mod == nil || hirMod == nil {
		return nil
	}

	mirMod := &mir.Module{
		ImportPath: g.mod.ImportPath,
		Location:   hirMod.Location,
	}

	for _, item := range hirMod.Items {
		switch n := item.(type) {
		case *hir.FuncDecl:
			if fn := g.lowerFuncDecl(n); fn != nil {
				mirMod.Functions = append(mirMod.Functions, fn)
			}
		case *hir.MethodDecl:
			if fn := g.lowerMethodDecl(n); fn != nil {
				mirMod.Functions = append(mirMod.Functions, fn)
			}
		}
	}

	return mirMod
}

func (g *Generator) lowerFuncDecl(decl *hir.FuncDecl) *mir.Function {
	if decl == nil || decl.Name == nil {
		return nil
	}

	fn := &mir.Function{
		Name:     decl.Name.Name,
		Return:   g.returnType(decl.Type),
		Location: decl.Location,
	}

	fn.Params = g.lowerParams(decl.Type)
	builder := newFunctionBuilder(g, fn)
	builder.buildFuncBody(decl.Body)

	return fn
}

func (g *Generator) lowerMethodDecl(decl *hir.MethodDecl) *mir.Function {
	if decl == nil || decl.Name == nil {
		return nil
	}

	fn := &mir.Function{
		Name:     decl.Name.Name,
		Return:   g.returnType(decl.Type),
		Location: decl.Location,
	}

	if decl.Receiver != nil {
		recv := g.newParam(decl.Receiver.Name, decl.Receiver.Type, decl.Receiver.Location)
		fn.Receiver = &recv
		fn.Params = append(fn.Params, recv)
	}

	fn.Params = append(fn.Params, g.lowerParams(decl.Type)...)
	builder := newFunctionBuilder(g, fn)
	builder.buildFuncBody(decl.Body)

	return fn
}

func (g *Generator) lowerParams(fnType *types.FunctionType) []mir.Param {
	if fnType == nil || len(fnType.Params) == 0 {
		return nil
	}

	params := make([]mir.Param, 0, len(fnType.Params))
	for _, param := range fnType.Params {
		params = append(params, g.newParam(param.Name, param.Type, source.Location{}))
	}
	return params
}

func (g *Generator) returnType(fnType *types.FunctionType) types.SemType {
	if fnType == nil || fnType.Return == nil {
		return types.TypeVoid
	}
	return fnType.Return
}

func (g *Generator) newParam(name string, typ types.SemType, loc source.Location) mir.Param {
	id := g.nextValueID()
	return mir.Param{
		ID:       id,
		Name:     name,
		Type:     typ,
		Location: loc,
	}
}

func (g *Generator) nextValueID() mir.ValueID {
	g.nextValue++
	return g.nextValue
}

func (g *Generator) nextBlockID() mir.BlockID {
	g.nextBlock++
	return g.nextBlock
}
