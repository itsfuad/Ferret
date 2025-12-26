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

	valueTypes  map[mir.ValueID]types.SemType
	stringLits  map[string]string
	enumTables  map[string]string
	enumCounts  map[string]int
	tempID      int
	stringID    int
	enumTableID int
	retOutParam string
	retOutType  types.SemType
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
		enumTables: make(map[string]string),
		enumCounts: make(map[string]int),
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
	g.retOutParam = ""
	g.retOutType = nil

	fnName := g.qbeFuncName(fn.Name, g.moduleImportPath())
	mainReturnsInt := false
	retType := ""
	if optType, ok := g.optionalType(fn.Return); ok {
		g.retOutType = optType
		g.retOutParam = "%ret"
	} else if resType, ok := g.resultType(fn.Return); ok {
		g.retOutType = resType
		g.retOutParam = "%ret"
	} else {
		retType, mainReturnsInt = g.qbeReturnType(fn.Name, fn.Return)
	}

	g.buf.WriteString("export function")
	if retType != "" {
		g.buf.WriteString(" " + retType)
	}
	g.buf.WriteString(" $" + fnName + "(")

	if g.retOutType != nil {
		if len(fn.Params) > 0 {
			g.buf.WriteString("l " + g.retOutParam + ", ")
		} else {
			g.buf.WriteString("l " + g.retOutParam)
		}
	}

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
		g.emitMapGet(i)
	case *mir.MapSet:
		g.emitMapSet(i)
	case *mir.OptionalNone:
		g.emitOptionalNone(i)
	case *mir.OptionalSome:
		g.emitOptionalSome(i)
	case *mir.OptionalIsSome:
		g.emitOptionalIsSome(i)
	case *mir.OptionalUnwrap:
		g.emitOptionalUnwrap(i)
	case *mir.ResultOk:
		g.emitResultOk(i)
	case *mir.ResultErr:
		g.emitResultErr(i)
	case *mir.ResultIsOk:
		g.emitResultIsOk(i)
	case *mir.ResultUnwrap:
		g.emitResultUnwrap(i)
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
		if g.retOutType != nil {
			if t.HasValue {
				if _, ok := g.optionalType(g.retOutType); ok {
					g.emitOptionalCopy(g.retOutParam, g.valueName(t.Value), g.retOutType, &t.Location)
				} else if _, ok := g.resultType(g.retOutType); ok {
					g.emitResultCopy(g.retOutParam, g.valueName(t.Value), g.retOutType, &t.Location)
				}
			}
			g.emitLine("ret")
			return
		}
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
