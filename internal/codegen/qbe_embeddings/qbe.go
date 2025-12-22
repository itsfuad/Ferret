package qbe

import (
	"fmt"
	"strings"

	"compiler/internal/context_v2"
	"compiler/internal/mir"
	"compiler/internal/types"
)

type Generator struct {
	ctx    *context_v2.CompilerContext
	mod    *context_v2.Module
	mirMod *mir.Module
	layout *mir.DataLayout

	data strings.Builder
	buf  strings.Builder

	valueTypes map[mir.ValueID]types.SemType
	stringLits map[string]string
	tempID     int
	stringID   int
}

func New(ctx *context_v2.CompilerContext, mod *context_v2.Module, mirMod *mir.Module) *Generator {
	pointerSize := 0
	if ctx != nil && ctx.Config != nil {
		pointerSize = ctx.Config.PointerSize
	}
	return &Generator{
		ctx:        ctx,
		mod:        mod,
		mirMod:     mirMod,
		layout:     mir.NewDataLayout(pointerSize),
		valueTypes: make(map[mir.ValueID]types.SemType),
		stringLits: make(map[string]string),
	}
}

func (g *Generator) Emit() (string, error) {
	if g.mirMod == nil {
		return "", fmt.Errorf("qbe: missing MIR module")
	}

	for _, fn := range g.mirMod.Functions {
		g.emitFunction(fn)
	}

	if g.ctx != nil && g.ctx.HasErrors() {
		return "", fmt.Errorf("qbe: codegen failed")
	}

	var out strings.Builder
	if g.data.Len() > 0 {
		out.WriteString(g.data.String())
		out.WriteString("\n")
	}
	out.WriteString(g.buf.String())
	return out.String(), nil
}

func (g *Generator) emitFunction(fn *mir.Function) {
	if fn == nil {
		return
	}

	g.valueTypes = make(map[mir.ValueID]types.SemType)
	g.tempID = 0

	fnName := g.qbeFuncName(fn.Name, g.moduleImportPath())
	retType, mainReturnsInt := g.qbeReturnType(fn.Name, fn.Return)

	g.buf.WriteString("export function")
	if retType != "" {
		g.buf.WriteString(" " + retType)
	}
	g.buf.WriteString(" $" + fnName + "(")

	for i, param := range fn.Params {
		paramSem := normalizeLargeValueType(param.Type)
		paramType, err := g.qbeType(paramSem)
		if err != nil {
			g.reportError(err.Error(), &param.Location)
			paramType = "w"
		}
		if i > 0 {
			g.buf.WriteString(", ")
		}
		g.buf.WriteString(paramType + " " + g.valueName(param.ID))
		g.valueTypes[param.ID] = paramSem
	}

	g.buf.WriteString(") {\n")

	for _, block := range fn.Blocks {
		g.emitBlock(block, mainReturnsInt)
	}

	g.buf.WriteString("}\n\n")
}

func (g *Generator) emitBlock(block *mir.Block, mainReturnsInt bool) {
	if block == nil {
		return
	}

	g.buf.WriteString(g.blockName(block.ID))
	g.buf.WriteString("\n")

	for _, instr := range block.Instrs {
		if _, ok := instr.(*mir.Phi); ok {
			g.emitInstr(instr)
		}
	}

	for _, instr := range block.Instrs {
		if _, ok := instr.(*mir.Phi); ok {
			continue
		}
		g.emitInstr(instr)
	}

	g.emitTerm(block.Term, mainReturnsInt)
}

func (g *Generator) emitInstr(instr mir.Instr) {
	switch i := instr.(type) {
	case *mir.Const:
		g.emitConst(i)
	case *mir.Binary:
		g.emitBinary(i)
	case *mir.Unary:
		g.emitUnary(i)
	case *mir.Cast:
		g.emitCast(i)
	case *mir.Alloca:
		g.emitAlloca(i)
	case *mir.Load:
		g.emitLoad(i)
	case *mir.PtrAdd:
		g.emitPtrAdd(i)
	case *mir.PtrOffset:
		g.emitPtrOffset(i)
	case *mir.Store:
		g.emitStore(i)
	case *mir.Call:
		g.emitCall(i)
	case *mir.CallIndirect:
		g.reportUnsupported("call_indirect", i.Loc())
	case *mir.Phi:
		g.emitPhi(i)
	case *mir.MakeStruct:
		g.reportUnsupported("make_struct", i.Loc())
	case *mir.ExtractField:
		g.reportUnsupported("extract_field", i.Loc())
	case *mir.InsertField:
		g.reportUnsupported("insert_field", i.Loc())
	case *mir.MakeArray:
		g.reportUnsupported("make_array", i.Loc())
	case *mir.ArrayGet:
		g.reportUnsupported("array_get", i.Loc())
	case *mir.ArraySet:
		g.reportUnsupported("array_set", i.Loc())
	case *mir.MapGet:
		g.reportUnsupported("map_get", i.Loc())
	case *mir.MapSet:
		g.reportUnsupported("map_set", i.Loc())
	case *mir.OptionalNone:
		g.reportUnsupported("optional_none", i.Loc())
	case *mir.OptionalSome:
		g.reportUnsupported("optional_some", i.Loc())
	case *mir.OptionalIsSome:
		g.reportUnsupported("optional_is_some", i.Loc())
	case *mir.OptionalUnwrap:
		g.reportUnsupported("optional_unwrap", i.Loc())
	case *mir.ResultOk:
		g.reportUnsupported("result_ok", i.Loc())
	case *mir.ResultErr:
		g.reportUnsupported("result_err", i.Loc())
	case *mir.ResultIsOk:
		g.reportUnsupported("result_is_ok", i.Loc())
	case *mir.ResultUnwrap:
		g.reportUnsupported("result_unwrap", i.Loc())
	default:
		g.reportUnsupported("instruction", instr.Loc())
	}
}

func (g *Generator) emitTerm(term mir.Term, mainReturnsInt bool) {
	if term == nil {
		g.reportUnsupported("terminator", nil)
		if mainReturnsInt {
			g.emitLine("ret 0")
		} else {
			g.emitLine("ret")
		}
		return
	}

	switch t := term.(type) {
	case *mir.Return:
		if t.HasValue {
			g.emitLine(fmt.Sprintf("ret %s", g.valueName(t.Value)))
			return
		}
		if mainReturnsInt {
			g.emitLine("ret 0")
			return
		}
		g.emitLine("ret")
	case *mir.Br:
		g.emitLine(fmt.Sprintf("jmp %s", g.blockName(t.Target)))
	case *mir.CondBr:
		g.emitLine(fmt.Sprintf("jnz %s, %s, %s", g.valueName(t.Cond), g.blockName(t.Then), g.blockName(t.Else)))
	case *mir.Switch:
		g.reportUnsupported("switch", t.Loc())
		g.emitLine(fmt.Sprintf("jmp %s", g.blockName(t.Default)))
	case *mir.Unreachable:
		if mainReturnsInt {
			g.emitLine("ret 0")
		} else {
			g.emitLine("ret")
		}
	default:
		g.reportUnsupported("terminator", t.Loc())
		if mainReturnsInt {
			g.emitLine("ret 0")
		} else {
			g.emitLine("ret")
		}
	}
}
