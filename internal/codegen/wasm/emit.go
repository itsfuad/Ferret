package wasm

import (
	"fmt"
	"strconv"
	"strings"

	"compiler/internal/context_v2"
	"compiler/internal/mir"
	"compiler/internal/source"
	"compiler/internal/tokens"
	"compiler/internal/types"
	ustrings "compiler/internal/utils/strings"
)

type Unit struct {
	Module   *context_v2.Module
	MIR      *mir.Module
	IsEntry  bool
	EntryMod string
}

type funcInfo struct {
	name      string
	fn        *mir.Function
	mod       *context_v2.Module
	valueType map[mir.ValueID]types.SemType
}

type funcSig struct {
	params  []ValType
	results []ValType
}

type Generator struct {
	ctx *context_v2.CompilerContext

	layout *mir.DataLayout
	units  []Unit

	funcs       map[string]*funcInfo
	entryModule string

	imports   map[string]funcSig
	importIDs map[string]uint32

	dataBase   uint32
	data       []byte
	stringPtrs map[string]uint32
}

func EmitProgram(ctx *context_v2.CompilerContext, units []Unit) ([]byte, error) {
	if ctx == nil {
		return nil, fmt.Errorf("wasm: missing compiler context")
	}
	gen := &Generator{
		ctx:        ctx,
		layout:     mir.NewDataLayout(4),
		units:      units,
		funcs:      make(map[string]*funcInfo),
		imports:    make(map[string]funcSig),
		importIDs:  make(map[string]uint32),
		dataBase:   1024,
		stringPtrs: make(map[string]uint32),
	}
	for _, u := range units {
		if u.IsEntry {
			gen.entryModule = u.Module.ImportPath
			break
		}
	}
	if gen.entryModule == "" && len(units) > 0 && units[0].Module != nil {
		gen.entryModule = units[0].Module.ImportPath
	}

	if err := gen.collectFunctions(); err != nil {
		return nil, err
	}
	if err := gen.collectImports(); err != nil {
		return nil, err
	}

	mod := &ModuleBuilder{}

	importNames := make([]string, 0, len(gen.imports))
	for name := range gen.imports {
		importNames = append(importNames, name)
	}
	sortStrings(importNames)
	for _, name := range importNames {
		sig := gen.imports[name]
		typeIndex := mod.addType(sig.params, sig.results)
		index := mod.addImportFunc("ferret", name, typeIndex)
		gen.importIDs[name] = index
	}

	functionIndex := make(map[string]uint32)
	funcOrder := make([]string, 0, len(gen.funcs))
	for name := range gen.funcs {
		funcOrder = append(funcOrder, name)
	}
	sortStrings(funcOrder)
	baseIndex := uint32(len(importNames))
	for i, name := range funcOrder {
		functionIndex[name] = baseIndex + uint32(i)
	}

	for _, name := range funcOrder {
		info := gen.funcs[name]
		sig, err := gen.funcSignature(info.fn)
		if err != nil {
			return nil, err
		}
		typeIndex := mod.addType(sig.params, sig.results)
		locals, body, err := gen.emitFunction(info, functionIndex)
		if err != nil {
			return nil, err
		}
		mod.addFunction(typeIndex, locals, body)
	}

	for _, name := range funcOrder {
		info := gen.funcs[name]
		if info.fn != nil && info.fn.Name == "main" && gen.isEntryModule(info.mod) {
			if idx, ok := functionIndex[info.name]; ok {
				mod.addExport("main", exportKindFunc, idx)
			}
		}
	}
	mod.addExport("memory", exportKindMem, 0)

	dataEnd := gen.dataBase + uint32(len(gen.data))
	dataInit := append([]byte{0x41}, encodeU32(uint32(dataEnd))...)
	globalIndex := mod.addGlobal(valTypeI32, false, dataInit)
	mod.addExport("__data_end", exportKindGlobal, globalIndex)

	if len(gen.data) > 0 {
		mod.addData(gen.dataBase, gen.data)
	}

	mod.memoryMin = gen.memoryMinPages()
	return mod.emit(), nil
}

func (g *Generator) collectFunctions() error {
	for _, unit := range g.units {
		if unit.Module == nil || unit.MIR == nil {
			continue
		}
		for _, fn := range unit.MIR.Functions {
			if fn == nil {
				continue
			}
			name := g.funcName(fn.Name, unit.Module.ImportPath)
			if name == "" {
				continue
			}
			info := &funcInfo{
				name:      name,
				fn:        fn,
				mod:       unit.Module,
				valueType: analyzeFunctionTypes(fn),
			}
			g.funcs[name] = info
		}
	}
	if len(g.funcs) == 0 {
		return fmt.Errorf("wasm: no functions to emit")
	}
	return nil
}

func (g *Generator) collectImports() error {
	for _, info := range g.funcs {
		if info == nil || info.fn == nil {
			continue
		}
		for _, block := range info.fn.Blocks {
			if block == nil {
				continue
			}
			for _, instr := range block.Instrs {
				if _, ok := instr.(*mir.Alloca); ok {
					g.imports["ferret_alloc"] = funcSig{
						params:  []ValType{valTypeI64},
						results: []ValType{valTypeI32},
					}
				}
				call, ok := instr.(*mir.Call)
				if !ok || call == nil {
					continue
				}
				target, err := g.resolveCallTarget(info.mod, call.Target)
				if err != nil {
					g.reportError(err.Error(), call.Loc())
					continue
				}
				if _, ok := g.funcs[target]; ok {
					continue
				}
				sig, err := g.callSignature(info, call)
				if err != nil {
					return err
				}
				if existing, ok := g.imports[target]; ok {
					if !sigEqual(existing, sig) {
						return fmt.Errorf("wasm: conflicting signatures for import %s", target)
					}
					continue
				}
				g.imports[target] = sig
			}
		}
	}
	return nil
}

func (g *Generator) emitFunction(info *funcInfo, functionIndex map[string]uint32) ([]ValType, []byte, error) {
	if info == nil || info.fn == nil {
		return nil, nil, fmt.Errorf("wasm: missing function")
	}

	paramCount := len(info.fn.Params)
	localIndex := make(map[mir.ValueID]uint32)
	locals := make([]ValType, 0, 8)

	for i, p := range info.fn.Params {
		localIndex[p.ID] = uint32(i)
	}

	blockLocal := uint32(paramCount)
	locals = append(locals, valTypeI32)

	nextLocal := blockLocal + 1
	allocLocal := func(id mir.ValueID, typ types.SemType) error {
		if id == mir.InvalidValue {
			return nil
		}
		if _, ok := localIndex[id]; ok {
			return nil
		}
		valType, err := wasmValueType(typ)
		if err != nil {
			return err
		}
		localIndex[id] = nextLocal
		locals = append(locals, valType)
		nextLocal++
		return nil
	}

	for _, block := range info.fn.Blocks {
		for _, instr := range block.Instrs {
			switch v := instr.(type) {
			case *mir.Const:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.Binary:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.Unary:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.Cast:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.Alloca:
				if err := allocLocal(v.Result, types.NewReference(v.Type)); err != nil {
					return nil, nil, err
				}
			case *mir.Load:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.PtrAdd:
				if err := allocLocal(v.Result, types.NewReference(v.Elem)); err != nil {
					return nil, nil, err
				}
			case *mir.PtrOffset:
				if err := allocLocal(v.Result, types.NewReference(v.Elem)); err != nil {
					return nil, nil, err
				}
			case *mir.Call:
				if v.Result != mir.InvalidValue && !isVoidType(v.Type) {
					if err := allocLocal(v.Result, v.Type); err != nil {
						return nil, nil, err
					}
				}
			case *mir.Phi:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.MakeStruct:
				if err := allocLocal(v.Result, types.NewReference(v.Type)); err != nil {
					return nil, nil, err
				}
			case *mir.ExtractField:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.InsertField:
				if err := allocLocal(v.Result, types.NewReference(v.Type)); err != nil {
					return nil, nil, err
				}
			case *mir.MakeArray:
				if err := allocLocal(v.Result, types.NewReference(v.Type)); err != nil {
					return nil, nil, err
				}
			case *mir.ArrayGet:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.MapGet:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.OptionalNone:
				if err := allocLocal(v.Result, types.NewReference(v.Type)); err != nil {
					return nil, nil, err
				}
			case *mir.OptionalSome:
				if err := allocLocal(v.Result, types.NewReference(v.Type)); err != nil {
					return nil, nil, err
				}
			case *mir.OptionalIsSome:
				if err := allocLocal(v.Result, types.TypeBool); err != nil {
					return nil, nil, err
				}
			case *mir.OptionalUnwrap:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.UnionVariantCheck:
				if err := allocLocal(v.Result, types.TypeBool); err != nil {
					return nil, nil, err
				}
			case *mir.UnionExtract:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			case *mir.ResultOk:
				if err := allocLocal(v.Result, types.NewReference(v.Type)); err != nil {
					return nil, nil, err
				}
			case *mir.ResultErr:
				if err := allocLocal(v.Result, types.NewReference(v.Type)); err != nil {
					return nil, nil, err
				}
			case *mir.ResultIsOk:
				if err := allocLocal(v.Result, types.TypeBool); err != nil {
					return nil, nil, err
				}
			case *mir.ResultUnwrap:
				if err := allocLocal(v.Result, v.Type); err != nil {
					return nil, nil, err
				}
			}
		}
	}

	blockIndex := make(map[mir.BlockID]uint32)
	for i, block := range info.fn.Blocks {
		if block == nil {
			continue
		}
		blockIndex[block.ID] = uint32(i)
	}

	phiMoves := collectPhiMoves(info.fn.Blocks)

	var code []byte
	entryIdx := uint32(0)
	if len(info.fn.Blocks) > 0 {
		entryIdx = blockIndex[info.fn.Blocks[0].ID]
	}
	code = append(code, opcodeI32Const)
	code = append(code, encodeU32(entryIdx)...)
	code = append(code, opcodeLocalSet)
	code = append(code, encodeU32(blockLocal)...)

	code = append(code, opcodeLoop, blockTypeVoid)

	for _, block := range info.fn.Blocks {
		if block == nil {
			continue
		}
		idx := blockIndex[block.ID]
		code = append(code, opcodeLocalGet)
		code = append(code, encodeU32(blockLocal)...)
		code = append(code, opcodeI32Const)
		code = append(code, encodeU32(idx)...)
		code = append(code, opcodeI32Eq)
		code = append(code, opcodeIf, blockTypeVoid)
		for _, instr := range block.Instrs {
			if _, ok := instr.(*mir.Phi); ok {
				continue
			}
			insn, err := g.emitInstr(info, instr, localIndex, functionIndex)
			if err != nil {
				return nil, nil, err
			}
			code = append(code, insn...)
		}
		term, err := g.emitTerm(info, block, localIndex, functionIndex, blockIndex, blockLocal, phiMoves)
		if err != nil {
			return nil, nil, err
		}
		code = append(code, term...)
		code = append(code, opcodeEnd)
	}

	code = append(code, opcodeUnreachable)
	code = append(code, opcodeEnd)

	return locals, code, nil
}

func (g *Generator) emitInstr(info *funcInfo, instr mir.Instr, locals map[mir.ValueID]uint32, fnIndex map[string]uint32) ([]byte, error) {
	switch v := instr.(type) {
	case *mir.Const:
		return g.emitConst(v, locals)
	case *mir.Binary:
		return g.emitBinary(v, locals)
	case *mir.Unary:
		return g.emitUnary(v, locals)
	case *mir.Cast:
		return g.emitCast(info, v, locals)
	case *mir.Alloca:
		return g.emitAlloca(v, locals)
	case *mir.Load:
		return g.emitLoad(v, locals)
	case *mir.Store:
		return g.emitStore(info, v, locals)
	case *mir.PtrAdd:
		return g.emitPtrAdd(v, locals)
	case *mir.PtrOffset:
		return g.emitPtrOffset(v, locals)
	case *mir.Call:
		return g.emitCall(info, v, locals, fnIndex)
	case *mir.CallIndirect:
		g.reportUnsupported("call indirect", v.Loc())
		return []byte{opcodeUnreachable}, nil
	case *mir.MakeStruct, *mir.ExtractField, *mir.InsertField:
		g.reportUnsupported("struct op", instr.Loc())
		return []byte{opcodeUnreachable}, nil
	case *mir.MakeArray, *mir.ArrayGet, *mir.ArraySet:
		g.reportUnsupported("array op", instr.Loc())
		return []byte{opcodeUnreachable}, nil
	case *mir.MapGet, *mir.MapSet:
		g.reportUnsupported("map op", instr.Loc())
		return []byte{opcodeUnreachable}, nil
	case *mir.OptionalNone, *mir.OptionalSome, *mir.OptionalIsSome, *mir.OptionalUnwrap:
		g.reportUnsupported("optional op", instr.Loc())
		return []byte{opcodeUnreachable}, nil
	case *mir.UnionVariantCheck, *mir.UnionExtract:
		g.reportUnsupported("union op", instr.Loc())
		return []byte{opcodeUnreachable}, nil
	case *mir.ResultOk, *mir.ResultErr, *mir.ResultIsOk, *mir.ResultUnwrap:
		g.reportUnsupported("result op", instr.Loc())
		return []byte{opcodeUnreachable}, nil
	default:
		g.reportUnsupported("instruction", instr.Loc())
		return []byte{opcodeUnreachable}, nil
	}
}

func (g *Generator) emitTerm(info *funcInfo, block *mir.Block, locals map[mir.ValueID]uint32, fnIndex map[string]uint32, blockIndex map[mir.BlockID]uint32, blockLocal uint32, phiMoves map[mir.BlockID]map[mir.BlockID][]phiMove) ([]byte, error) {
	if block == nil || block.Term == nil {
		return nil, nil
	}
	switch t := block.Term.(type) {
	case *mir.Return:
		var out []byte
		if t.HasValue && t.Value != mir.InvalidValue {
			out = append(out, opcodeLocalGet)
			out = append(out, encodeU32(locals[t.Value])...)
		}
		out = append(out, opcodeReturn)
		return out, nil
	case *mir.Br:
		return g.emitBranch(block.ID, t.Target, locals, blockIndex, blockLocal, phiMoves), nil
	case *mir.CondBr:
		var out []byte
		out = append(out, opcodeLocalGet)
		out = append(out, encodeU32(locals[t.Cond])...)
		out = append(out, opcodeIf, blockTypeVoid)
		out = append(out, g.emitPhiMoves(block.ID, t.Then, locals, phiMoves)...)
		out = append(out, opcodeI32Const)
		out = append(out, encodeU32(blockIndex[t.Then])...)
		out = append(out, opcodeLocalSet)
		out = append(out, encodeU32(blockLocal)...)
		out = append(out, opcodeElse)
		out = append(out, g.emitPhiMoves(block.ID, t.Else, locals, phiMoves)...)
		out = append(out, opcodeI32Const)
		out = append(out, encodeU32(blockIndex[t.Else])...)
		out = append(out, opcodeLocalSet)
		out = append(out, encodeU32(blockLocal)...)
		out = append(out, opcodeEnd)
		out = append(out, opcodeBr)
		out = append(out, encodeU32(1)...)
		return out, nil
	case *mir.Switch:
		g.reportUnsupported("switch", t.Loc())
		return []byte{opcodeUnreachable}, nil
	case *mir.Unreachable:
		return []byte{opcodeUnreachable}, nil
	default:
		g.reportUnsupported("terminator", t.Loc())
		return []byte{opcodeUnreachable}, nil
	}
}

func (g *Generator) emitBranch(pred, target mir.BlockID, locals map[mir.ValueID]uint32, blockIndex map[mir.BlockID]uint32, blockLocal uint32, phiMoves map[mir.BlockID]map[mir.BlockID][]phiMove) []byte {
	var out []byte
	out = append(out, g.emitPhiMoves(pred, target, locals, phiMoves)...)
	out = append(out, opcodeI32Const)
	out = append(out, encodeU32(blockIndex[target])...)
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(blockLocal)...)
	out = append(out, opcodeBr)
	out = append(out, encodeU32(1)...)
	return out
}

func (g *Generator) emitPhiMoves(pred, succ mir.BlockID, locals map[mir.ValueID]uint32, phiMoves map[mir.BlockID]map[mir.BlockID][]phiMove) []byte {
	movesBySucc, ok := phiMoves[pred]
	if !ok {
		return nil
	}
	moves := movesBySucc[succ]
	if len(moves) == 0 {
		return nil
	}
	var out []byte
	for _, move := range moves {
		out = append(out, opcodeLocalGet)
		out = append(out, encodeU32(locals[move.src])...)
		out = append(out, opcodeLocalSet)
		out = append(out, encodeU32(locals[move.dst])...)
	}
	return out
}

func (g *Generator) emitConst(c *mir.Const, locals map[mir.ValueID]uint32) ([]byte, error) {
	if c == nil {
		return nil, nil
	}
	valType, err := wasmValueType(c.Type)
	if err != nil {
		return nil, err
	}
	var out []byte
	switch valType {
	case valTypeI32:
		value, err := parseI32Const(c.Type, c.Value, g.stringPtr)
		if err != nil {
			return nil, err
		}
		out = append(out, opcodeI32Const)
		out = append(out, encodeS32(value)...)
	case valTypeI64:
		value, err := parseI64Const(c.Type, c.Value)
		if err != nil {
			return nil, err
		}
		out = append(out, opcodeI64Const)
		out = append(out, encodeS64(value)...)
	case valTypeF32:
		value, err := strconv.ParseFloat(stripNumeric(c.Value), 32)
		if err != nil {
			return nil, err
		}
		out = append(out, opcodeF32Const)
		out = append(out, encodeF32(float32(value))...)
	case valTypeF64:
		value, err := strconv.ParseFloat(stripNumeric(c.Value), 64)
		if err != nil {
			return nil, err
		}
		out = append(out, opcodeF64Const)
		out = append(out, encodeF64(value)...)
	default:
		return nil, fmt.Errorf("wasm: unsupported const type")
	}
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(locals[c.Result])...)
	return out, nil
}

func (g *Generator) emitBinary(b *mir.Binary, locals map[mir.ValueID]uint32) ([]byte, error) {
	if b == nil {
		return nil, nil
	}
	valType, err := wasmValueType(b.Type)
	if err != nil {
		return nil, err
	}
	opcode, err := binaryOpcode(b.Op, valType, isUnsignedType(b.Type))
	if err != nil {
		g.reportError(err.Error(), b.Loc())
		return []byte{opcodeUnreachable}, nil
	}
	var out []byte
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[b.Left])...)
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[b.Right])...)
	out = append(out, opcode...)
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(locals[b.Result])...)
	return out, nil
}

func (g *Generator) emitUnary(u *mir.Unary, locals map[mir.ValueID]uint32) ([]byte, error) {
	if u == nil {
		return nil, nil
	}
	valType, err := wasmValueType(u.Type)
	if err != nil {
		return nil, err
	}
	var out []byte
	switch u.Op {
	case tokens.MINUS_TOKEN:
		switch valType {
		case valTypeI32:
			out = append(out, opcodeI32Const)
			out = append(out, encodeS32(0)...)
			out = append(out, opcodeLocalGet)
			out = append(out, encodeU32(locals[u.X])...)
			out = append(out, opcodeI32Sub)
		case valTypeI64:
			out = append(out, opcodeI64Const)
			out = append(out, encodeS64(0)...)
			out = append(out, opcodeLocalGet)
			out = append(out, encodeU32(locals[u.X])...)
			out = append(out, opcodeI64Sub)
		case valTypeF32:
			out = append(out, opcodeLocalGet)
			out = append(out, encodeU32(locals[u.X])...)
			out = append(out, opcodeF32Neg)
		case valTypeF64:
			out = append(out, opcodeLocalGet)
			out = append(out, encodeU32(locals[u.X])...)
			out = append(out, opcodeF64Neg)
		default:
			return nil, fmt.Errorf("wasm: unsupported unary -")
		}
	case tokens.NOT_TOKEN:
		out = append(out, opcodeLocalGet)
		out = append(out, encodeU32(locals[u.X])...)
		out = append(out, opcodeI32Eqz)
	default:
		return nil, fmt.Errorf("wasm: unsupported unary op %s", u.Op)
	}
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(locals[u.Result])...)
	return out, nil
}

func (g *Generator) emitCast(info *funcInfo, c *mir.Cast, locals map[mir.ValueID]uint32) ([]byte, error) {
	if c == nil {
		return nil, nil
	}
	if info == nil {
		return nil, fmt.Errorf("wasm: missing cast context")
	}
	fromType, ok := info.valueType[c.X]
	if !ok {
		return nil, fmt.Errorf("wasm: missing cast source type")
	}
	fromVal, err := wasmValueType(fromType)
	if err != nil {
		return nil, err
	}
	toVal, err := wasmValueType(c.Type)
	if err != nil {
		return nil, err
	}
	var out []byte
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[c.X])...)
	op, ok := castOpcode(fromVal, toVal, isUnsignedType(fromType))
	if !ok {
		return nil, fmt.Errorf("wasm: unsupported cast")
	}
	out = append(out, op...)
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(locals[c.Result])...)
	return out, nil
}

func (g *Generator) emitAlloca(a *mir.Alloca, locals map[mir.ValueID]uint32) ([]byte, error) {
	if a == nil {
		return nil, nil
	}
	size := g.layout.SizeOf(a.Type)
	if size <= 0 {
		return nil, fmt.Errorf("wasm: invalid alloca size")
	}
	var out []byte
	out = append(out, opcodeI64Const)
	out = append(out, encodeS64(int64(size))...)
	callIndex, ok := g.importIDs["ferret_alloc"]
	if !ok {
		return nil, fmt.Errorf("wasm: missing import ferret_alloc")
	}
	out = append(out, opcodeCall)
	out = append(out, encodeU32(callIndex)...)
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(locals[a.Result])...)
	return out, nil
}

func (g *Generator) emitLoad(l *mir.Load, locals map[mir.ValueID]uint32) ([]byte, error) {
	if l == nil {
		return nil, nil
	}
	op, err := loadOpcode(l.Type)
	if err != nil {
		return nil, err
	}
	var out []byte
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[l.Addr])...)
	out = append(out, op...)
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(locals[l.Result])...)
	return out, nil
}

func (g *Generator) emitStore(info *funcInfo, s *mir.Store, locals map[mir.ValueID]uint32) ([]byte, error) {
	if s == nil {
		return nil, nil
	}
	if info == nil {
		return nil, fmt.Errorf("wasm: missing store context")
	}
	typ, ok := info.valueType[s.Value]
	if !ok {
		return nil, fmt.Errorf("wasm: missing store type")
	}
	op, err := storeOpcode(typ)
	if err != nil {
		return nil, err
	}
	var out []byte
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[s.Addr])...)
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[s.Value])...)
	out = append(out, op...)
	return out, nil
}

func (g *Generator) emitPtrAdd(p *mir.PtrAdd, locals map[mir.ValueID]uint32) ([]byte, error) {
	if p == nil {
		return nil, nil
	}
	var out []byte
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[p.Base])...)
	out = append(out, opcodeI32Const)
	out = append(out, encodeS32(int32(p.Offset))...)
	out = append(out, opcodeI32Add)
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(locals[p.Result])...)
	return out, nil
}

func (g *Generator) emitPtrOffset(p *mir.PtrOffset, locals map[mir.ValueID]uint32) ([]byte, error) {
	if p == nil {
		return nil, nil
	}
	var out []byte
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[p.Base])...)
	out = append(out, opcodeLocalGet)
	out = append(out, encodeU32(locals[p.Offset])...)
	out = append(out, opcodeI32Add)
	out = append(out, opcodeLocalSet)
	out = append(out, encodeU32(locals[p.Result])...)
	return out, nil
}

func (g *Generator) emitCall(info *funcInfo, c *mir.Call, locals map[mir.ValueID]uint32, fnIndex map[string]uint32) ([]byte, error) {
	if c == nil || info == nil {
		return nil, nil
	}
	target, err := g.resolveCallTarget(info.mod, c.Target)
	if err != nil {
		g.reportError(err.Error(), c.Loc())
		return []byte{opcodeUnreachable}, nil
	}
	var out []byte
	for _, arg := range c.Args {
		out = append(out, opcodeLocalGet)
		out = append(out, encodeU32(locals[arg])...)
	}
	if idx, ok := fnIndex[target]; ok {
		out = append(out, opcodeCall)
		out = append(out, encodeU32(idx)...)
	} else if idx, ok := g.importIDs[target]; ok {
		out = append(out, opcodeCall)
		out = append(out, encodeU32(idx)...)
	} else {
		return nil, fmt.Errorf("wasm: missing call target %s", target)
	}

	if c.Result != mir.InvalidValue && !isVoidType(c.Type) {
		out = append(out, opcodeLocalSet)
		out = append(out, encodeU32(locals[c.Result])...)
	}
	return out, nil
}

func (g *Generator) callSignature(info *funcInfo, call *mir.Call) (funcSig, error) {
	params := make([]ValType, 0, len(call.Args))
	for _, arg := range call.Args {
		typ, ok := info.valueType[arg]
		if !ok {
			return funcSig{}, fmt.Errorf("wasm: missing arg type for %s", call.Target)
		}
		v, err := wasmValueType(typ)
		if err != nil {
			return funcSig{}, err
		}
		params = append(params, v)
	}
	results := []ValType{}
	if !isVoidType(call.Type) {
		ret, err := wasmValueType(call.Type)
		if err != nil {
			return funcSig{}, err
		}
		results = append(results, ret)
	}
	return funcSig{params: params, results: results}, nil
}

func (g *Generator) funcSignature(fn *mir.Function) (funcSig, error) {
	if fn == nil {
		return funcSig{}, fmt.Errorf("wasm: missing function")
	}
	params := make([]ValType, 0, len(fn.Params))
	for _, p := range fn.Params {
		v, err := wasmValueType(p.Type)
		if err != nil {
			return funcSig{}, err
		}
		params = append(params, v)
	}
	results := []ValType{}
	if !isVoidType(fn.Return) {
		ret, err := wasmValueType(fn.Return)
		if err != nil {
			return funcSig{}, err
		}
		results = append(results, ret)
	}
	return funcSig{params: params, results: results}, nil
}

func (g *Generator) funcName(name, importPath string) string {
	base := sanitizeName(name)
	if name != "main" && importPath != "" && importPath != g.entryModule {
		prefix := sanitizeName(ustrings.ToIdentifier(importPath))
		return prefix + "_" + base
	}
	return base
}

func sanitizeName(name string) string {
	name = strings.ReplaceAll(name, "::", "_")
	name = strings.ReplaceAll(name, "/", "_")
	return name
}

func (g *Generator) resolveCallTarget(mod *context_v2.Module, target string) (string, error) {
	if target == "" {
		return "", fmt.Errorf("wasm: invalid call target")
	}
	if strings.HasPrefix(target, "ferret_") {
		return target, nil
	}
	if strings.Contains(target, "::") {
		parts := strings.Split(target, "::")
		moduleAlias := strings.Join(parts[:len(parts)-1], "::")
		funcName := parts[len(parts)-1]
		importPath := ""
		if mod != nil && mod.ImportAliasMap != nil {
			importPath = mod.ImportAliasMap[moduleAlias]
		}
		if importPath == "" {
			return "", fmt.Errorf("wasm: unknown module alias %q", moduleAlias)
		}
		if g.ctx != nil {
			if imported, ok := g.ctx.GetModule(importPath); ok && imported.ModuleScope != nil {
				if sym, ok := imported.ModuleScope.GetSymbol(funcName); ok && sym.IsNative && sym.NativeName != "" {
					return sym.NativeName, nil
				}
			}
		}
		return g.funcName(funcName, importPath), nil
	}
	if mod != nil && mod.ModuleScope != nil {
		if sym, ok := mod.ModuleScope.GetSymbol(target); ok && sym.IsNative && sym.NativeName != "" {
			return sym.NativeName, nil
		}
	}
	if mod != nil {
		return g.funcName(target, mod.ImportPath), nil
	}
	return target, nil
}

func (g *Generator) isEntryModule(mod *context_v2.Module) bool {
	return mod != nil && mod.ImportPath == g.entryModule
}

func (g *Generator) reportError(msg string, loc *source.Location) {
	if g.ctx == nil {
		return
	}
	g.ctx.ReportError(msg, loc)
}

func (g *Generator) reportUnsupported(what string, loc *source.Location) {
	g.reportError(fmt.Sprintf("wasm: unsupported %s", what), loc)
}

func (g *Generator) memoryMinPages() uint32 {
	size := g.dataBase + uint32(len(g.data))
	if size == 0 {
		return 1
	}
	const page = 65536
	pages := (size + page - 1) / page
	if pages == 0 {
		pages = 1
	}
	return pages
}

func (g *Generator) stringPtr(value string) int32 {
	if ptr, ok := g.stringPtrs[value]; ok {
		return int32(ptr)
	}
	offset := g.dataBase + uint32(len(g.data))
	g.stringPtrs[value] = offset
	g.data = append(g.data, []byte(value)...)
	g.data = append(g.data, 0)
	for len(g.data)%4 != 0 {
		g.data = append(g.data, 0)
	}
	return int32(offset)
}

func analyzeFunctionTypes(fn *mir.Function) map[mir.ValueID]types.SemType {
	typesMap := make(map[mir.ValueID]types.SemType)
	if fn == nil {
		return typesMap
	}
	for _, p := range fn.Params {
		typesMap[p.ID] = p.Type
	}
	for _, block := range fn.Blocks {
		if block == nil {
			continue
		}
		for _, instr := range block.Instrs {
			switch v := instr.(type) {
			case *mir.Const:
				typesMap[v.Result] = v.Type
			case *mir.Binary:
				if isCompareToken(v.Op) {
					typesMap[v.Result] = types.TypeBool
				} else {
					typesMap[v.Result] = v.Type
				}
			case *mir.Unary:
				if v.Op == tokens.NOT_TOKEN {
					typesMap[v.Result] = types.TypeBool
				} else {
					typesMap[v.Result] = v.Type
				}
			case *mir.Cast:
				typesMap[v.Result] = v.Type
			case *mir.Alloca:
				typesMap[v.Result] = types.NewReference(v.Type)
			case *mir.Load:
				typesMap[v.Result] = v.Type
			case *mir.PtrAdd:
				typesMap[v.Result] = types.NewReference(v.Elem)
			case *mir.PtrOffset:
				typesMap[v.Result] = types.NewReference(v.Elem)
			case *mir.Call:
				typesMap[v.Result] = v.Type
			case *mir.CallIndirect:
				typesMap[v.Result] = v.Type
			case *mir.Phi:
				typesMap[v.Result] = v.Type
			case *mir.MakeStruct:
				typesMap[v.Result] = types.NewReference(v.Type)
			case *mir.ExtractField:
				typesMap[v.Result] = v.Type
			case *mir.InsertField:
				typesMap[v.Result] = types.NewReference(v.Type)
			case *mir.MakeArray:
				typesMap[v.Result] = types.NewReference(v.Type)
			case *mir.ArrayGet:
				typesMap[v.Result] = v.Type
			case *mir.MapGet:
				typesMap[v.Result] = v.Type
			case *mir.OptionalNone:
				typesMap[v.Result] = types.NewReference(v.Type)
			case *mir.OptionalSome:
				typesMap[v.Result] = types.NewReference(v.Type)
			case *mir.OptionalIsSome:
				typesMap[v.Result] = types.TypeBool
			case *mir.OptionalUnwrap:
				typesMap[v.Result] = v.Type
			case *mir.UnionVariantCheck:
				typesMap[v.Result] = types.TypeBool
			case *mir.UnionExtract:
				typesMap[v.Result] = v.Type
			case *mir.ResultOk:
				typesMap[v.Result] = types.NewReference(v.Type)
			case *mir.ResultErr:
				typesMap[v.Result] = types.NewReference(v.Type)
			case *mir.ResultIsOk:
				typesMap[v.Result] = types.TypeBool
			case *mir.ResultUnwrap:
				typesMap[v.Result] = v.Type
			}
		}
	}
	return typesMap
}

type phiMove struct {
	dst mir.ValueID
	src mir.ValueID
}

func collectPhiMoves(blocks []*mir.Block) map[mir.BlockID]map[mir.BlockID][]phiMove {
	out := make(map[mir.BlockID]map[mir.BlockID][]phiMove)
	for _, block := range blocks {
		if block == nil {
			continue
		}
		for _, instr := range block.Instrs {
			phi, ok := instr.(*mir.Phi)
			if !ok {
				continue
			}
			for _, incoming := range phi.Incoming {
				if _, ok := out[incoming.Pred]; !ok {
					out[incoming.Pred] = make(map[mir.BlockID][]phiMove)
				}
				out[incoming.Pred][block.ID] = append(out[incoming.Pred][block.ID], phiMove{
					dst: phi.Result,
					src: incoming.Value,
				})
			}
		}
	}
	return out
}

func wasmValueType(typ types.SemType) (ValType, error) {
	if typ == nil {
		return valTypeI32, nil
	}
	typ = types.UnwrapType(typ)
	switch t := typ.(type) {
	case *types.PrimitiveType:
		switch t.GetName() {
		case types.TYPE_I64, types.TYPE_U64:
			return valTypeI64, nil
		case types.TYPE_F32:
			return valTypeF32, nil
		case types.TYPE_F64:
			return valTypeF64, nil
		case types.TYPE_F128, types.TYPE_F256, types.TYPE_I128, types.TYPE_I256, types.TYPE_U128, types.TYPE_U256:
			return valTypeI32, nil
		default:
			return valTypeI32, nil
		}
	case *types.ReferenceType:
		return valTypeI32, nil
	case *types.ArrayType, *types.MapType, *types.StructType, *types.UnionType, *types.InterfaceType, *types.OptionalType, *types.ResultType:
		return valTypeI32, nil
	default:
		return valTypeI32, nil
	}
}

func isVoidType(typ types.SemType) bool {
	if typ == nil {
		return true
	}
	if prim, ok := types.UnwrapType(typ).(*types.PrimitiveType); ok {
		name := prim.GetName()
		return name == types.TYPE_VOID || name == types.TYPE_NONE
	}
	return false
}

func isUnsignedType(typ types.SemType) bool {
	if prim, ok := types.UnwrapType(typ).(*types.PrimitiveType); ok {
		switch prim.GetName() {
		case types.TYPE_U8, types.TYPE_U16, types.TYPE_U32, types.TYPE_U64, types.TYPE_U128, types.TYPE_U256:
			return true
		case types.TYPE_BOOL, types.TYPE_BYTE:
			return true
		}
	}
	return false
}

func parseI32Const(typ types.SemType, value string, stringPtr func(string) int32) (int32, error) {
	if prim, ok := types.UnwrapType(typ).(*types.PrimitiveType); ok && prim.GetName() == types.TYPE_STRING {
		return stringPtr(value), nil
	}
	switch strings.ToLower(value) {
	case "true":
		return 1, nil
	case "false":
		return 0, nil
	}
	clean := stripNumeric(value)
	v, err := strconv.ParseInt(clean, 10, 32)
	if err != nil {
		u, uerr := strconv.ParseUint(clean, 10, 32)
		if uerr != nil {
			return 0, err
		}
		return int32(u), nil
	}
	return int32(v), nil
}

func parseI64Const(typ types.SemType, value string) (int64, error) {
	if prim, ok := types.UnwrapType(typ).(*types.PrimitiveType); ok && prim.GetName() == types.TYPE_STRING {
		return 0, fmt.Errorf("wasm: string const cannot be i64")
	}
	clean := stripNumeric(value)
	v, err := strconv.ParseInt(clean, 10, 64)
	if err != nil {
		u, uerr := strconv.ParseUint(clean, 10, 64)
		if uerr != nil {
			return 0, err
		}
		return int64(u), nil
	}
	return v, nil
}

func stripNumeric(value string) string {
	return strings.ReplaceAll(strings.TrimSpace(value), "_", "")
}

func sigEqual(a, b funcSig) bool {
	if len(a.params) != len(b.params) || len(a.results) != len(b.results) {
		return false
	}
	for i := range a.params {
		if a.params[i] != b.params[i] {
			return false
		}
	}
	for i := range a.results {
		if a.results[i] != b.results[i] {
			return false
		}
	}
	return true
}

func sortStrings(values []string) {
	for i := 0; i < len(values); i++ {
		for j := i + 1; j < len(values); j++ {
			if values[j] < values[i] {
				values[i], values[j] = values[j], values[i]
			}
		}
	}
}

func isCompareToken(op tokens.TOKEN) bool {
	switch op {
	case tokens.LESS_TOKEN, tokens.LESS_EQUAL_TOKEN,
		tokens.GREATER_TOKEN, tokens.GREATER_EQUAL_TOKEN,
		tokens.DOUBLE_EQUAL_TOKEN, tokens.NOT_EQUAL_TOKEN:
		return true
	default:
		return false
	}
}
