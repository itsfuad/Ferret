package gen

import (
	"fmt"
	"strconv"
	"strings"

	"compiler/internal/hir"
	"compiler/internal/hir/consteval"
	"compiler/internal/mir"
	"compiler/internal/semantics/symbols"
	"compiler/internal/source"
	"compiler/internal/tokens"
	"compiler/internal/types"
	"compiler/internal/utils/numeric"
)

type functionBuilder struct {
	gen          *Generator
	fn           *mir.Function
	current      *mir.Block
	paramsByName map[string]mir.ValueID
	slots        map[*symbols.Symbol]mir.ValueID
	ptrElem      map[mir.ValueID]types.SemType
	loopStack    []loopTargets
	retParam     mir.ValueID
	retType      types.SemType
}

func newFunctionBuilder(gen *Generator, fn *mir.Function) *functionBuilder {
	return &functionBuilder{
		gen:          gen,
		fn:           fn,
		paramsByName: make(map[string]mir.ValueID),
		slots:        make(map[*symbols.Symbol]mir.ValueID),
		ptrElem:      make(map[mir.ValueID]types.SemType),
		loopStack:    nil,
		retParam:     mir.InvalidValue,
	}
}

func (b *functionBuilder) buildFuncBody(body *hir.Block) {
	entry := b.newBlock("entry", b.fn.Location)
	b.setBlock(entry)

	for _, param := range b.fn.Params {
		if param.Name != "" {
			b.paramsByName[param.Name] = param.ID
		}
	}
	if len(b.fn.Params) > 0 && b.fn.Params[0].Name == "__ret" {
		b.retParam = b.fn.Params[0].ID
		if ref, ok := types.UnwrapType(b.fn.Params[0].Type).(*types.ReferenceType); ok {
			b.retType = ref.Inner
		} else {
			b.retType = b.fn.Params[0].Type
		}
		b.ptrElem[b.retParam] = b.retType
	}

	if body != nil {
		b.lowerBlock(body)
	}

	if b.current.Term == nil {
		b.finalizeCurrent()
	}
}

func (b *functionBuilder) finalizeCurrent() {
	if b.current == nil || b.current.Term != nil {
		return
	}
	if b.fn.Return != nil && b.fn.Return.Equals(types.TypeVoid) {
		b.current.Term = &mir.Return{HasValue: false, Location: b.current.Location}
		return
	}
	b.current.Term = &mir.Unreachable{Location: b.current.Location}
}

func (b *functionBuilder) lowerBlock(block *hir.Block) {
	if block == nil || b.current == nil {
		return
	}

	for _, node := range block.Nodes {
		b.lowerNode(node)
		if b.current.Term != nil {
			return
		}
	}
}

func (b *functionBuilder) lowerNode(node hir.Node) {
	if node == nil {
		return
	}

	switch n := node.(type) {
	case *hir.DeclStmt:
		b.lowerDecl(n.Decl)
	case *hir.VarDecl:
		b.lowerVarDecl(n)
	case *hir.ConstDecl:
		b.lowerConstDecl(n)
	case *hir.AssignStmt:
		b.lowerAssign(n)
	case *hir.ReturnStmt:
		b.lowerReturn(n)
	case *hir.BreakStmt:
		b.lowerBreak(n)
	case *hir.ContinueStmt:
		b.lowerContinue(n)
	case *hir.ExprStmt:
		b.lowerExpr(n.X)
	case *hir.Block:
		b.lowerBlock(n)
	case *hir.IfStmt:
		b.lowerIf(n)
	case *hir.WhileStmt:
		b.lowerWhile(n)
	case *hir.MatchStmt:
		b.lowerMatch(n)
	default:
		b.reportUnsupported("statement", node.Loc())
	}
}

func (b *functionBuilder) lowerDecl(decl hir.Decl) {
	switch d := decl.(type) {
	case *hir.VarDecl:
		b.lowerVarDecl(d)
	case *hir.ConstDecl:
		b.lowerConstDecl(d)
	default:
		b.reportUnsupported("declaration", decl.Loc())
	}
}

func (b *functionBuilder) lowerVarDecl(decl *hir.VarDecl) {
	if decl == nil {
		return
	}

	for _, item := range decl.Decls {
		b.lowerDeclItem(item)
	}
}

func (b *functionBuilder) lowerConstDecl(decl *hir.ConstDecl) {
	if decl == nil {
		return
	}

	for _, item := range decl.Decls {
		b.lowerDeclItem(item)
	}
}

func (b *functionBuilder) lowerDeclItem(item hir.DeclItem) {
	if item.Name == nil {
		return
	}

	typ := item.Type
	if typ == nil && item.Value != nil {
		if vtyp := b.exprType(item.Value); vtyp != nil {
			typ = vtyp
		}
	}

	addr := b.emitAlloca(typ, item.Name.Location)
	if item.Name.Symbol != nil {
		b.slots[item.Name.Symbol] = addr
	}

	if item.Value != nil {
		if lit, ok := item.Value.(*hir.CompositeLit); ok {
			if arrType, ok := types.UnwrapType(typ).(*types.ArrayType); ok && arrType.Length >= 0 {
				b.lowerArrayLiteralInto(addr, arrType, lit)
				return
			}
		}

		val := b.lowerExpr(item.Value)
		if val != mir.InvalidValue {
			b.emitStore(addr, val, item.Name.Location)
		}
	}
}

func (b *functionBuilder) lowerAssign(stmt *hir.AssignStmt) {
	if stmt == nil {
		return
	}

	if idx, ok := stmt.Lhs.(*hir.IndexExpr); ok {
		b.lowerIndexAssign(idx, stmt.Rhs, stmt.Op, stmt.Location)
		return
	}

	addr := b.lowerLValue(stmt.Lhs)
	if addr == mir.InvalidValue {
		return
	}

	if stmt.Op != nil && stmt.Op.Kind != tokens.EQUALS_TOKEN {
		cur := b.emitLoad(addr, b.exprType(stmt.Lhs), stmt.Location)
		rhs := b.lowerExpr(stmt.Rhs)
		if cur == mir.InvalidValue || rhs == mir.InvalidValue {
			return
		}
		op := assignTokenToBinary(stmt.Op.Kind)
		if op == "" {
			b.reportUnsupported("assignment operator", stmt.Loc())
			return
		}
		typ := b.exprType(stmt.Lhs)
		res := b.emitBinary(op, cur, rhs, typ, stmt.Location)
		b.emitStore(addr, res, stmt.Location)
		return
	}

	rhs := b.lowerExpr(stmt.Rhs)
	if rhs == mir.InvalidValue {
		return
	}
	b.emitStore(addr, rhs, stmt.Location)
}

func (b *functionBuilder) lowerReturn(stmt *hir.ReturnStmt) {
	if stmt == nil {
		return
	}

	if stmt.Result == nil {
		b.current.Term = &mir.Return{HasValue: false, Location: stmt.Location}
		return
	}

	val := b.lowerExpr(stmt.Result)
	if val == mir.InvalidValue {
		b.current.Term = &mir.Unreachable{Location: stmt.Location}
		return
	}

	if b.retParam != mir.InvalidValue {
		b.emitStore(b.retParam, val, stmt.Location)
		b.current.Term = &mir.Return{HasValue: false, Location: stmt.Location}
		return
	}

	b.current.Term = &mir.Return{HasValue: true, Value: val, Location: stmt.Location}
}

func (b *functionBuilder) lowerIf(stmt *hir.IfStmt) {
	if stmt == nil {
		return
	}

	cond := b.lowerExpr(stmt.Cond)
	if cond == mir.InvalidValue {
		b.current.Term = &mir.Unreachable{Location: stmt.Location}
		return
	}

	thenBlock := b.newBlock("if.then", stmt.Location)
	mergeBlock := b.newBlock("if.end", stmt.Location)

	var elseBlock *mir.Block
	if stmt.Else != nil {
		elseBlock = b.newBlock("if.else", stmt.Location)
	}

	elseTarget := mergeBlock.ID
	if elseBlock != nil {
		elseTarget = elseBlock.ID
	}

	b.current.Term = &mir.CondBr{
		Cond:     cond,
		Then:     thenBlock.ID,
		Else:     elseTarget,
		Location: stmt.Location,
	}

	b.setBlock(thenBlock)
	if stmt.Body != nil {
		b.lowerBlock(stmt.Body)
	}
	b.branchIfNoTerm(mergeBlock.ID, stmt.Location)

	if elseBlock != nil {
		b.setBlock(elseBlock)
		b.lowerNode(stmt.Else)
		b.branchIfNoTerm(mergeBlock.ID, stmt.Location)
	}

	b.setBlock(mergeBlock)
}

func (b *functionBuilder) lowerBreak(stmt *hir.BreakStmt) {
	if stmt == nil {
		return
	}

	loop := b.currentLoop()
	if loop == nil {
		b.reportUnsupported("break outside loop", &stmt.Location)
		b.current.Term = &mir.Unreachable{Location: stmt.Location}
		return
	}

	b.current.Term = &mir.Br{Target: loop.breakTarget, Location: stmt.Location}
}

func (b *functionBuilder) lowerContinue(stmt *hir.ContinueStmt) {
	if stmt == nil {
		return
	}

	loop := b.currentLoop()
	if loop == nil {
		b.reportUnsupported("continue outside loop", &stmt.Location)
		b.current.Term = &mir.Unreachable{Location: stmt.Location}
		return
	}

	b.current.Term = &mir.Br{Target: loop.continueTarget, Location: stmt.Location}
}

func (b *functionBuilder) lowerWhile(stmt *hir.WhileStmt) {
	if stmt == nil {
		return
	}

	condBlock := b.newBlock("while.cond", stmt.Location)
	bodyBlock := b.newBlock("while.body", stmt.Location)
	exitBlock := b.newBlock("while.end", stmt.Location)

	b.branchIfNoTerm(condBlock.ID, stmt.Location)

	b.setBlock(condBlock)
	cond := b.lowerExpr(stmt.Cond)
	if cond == mir.InvalidValue {
		b.current.Term = &mir.Unreachable{Location: stmt.Location}
	} else {
		b.current.Term = &mir.CondBr{
			Cond:     cond,
			Then:     bodyBlock.ID,
			Else:     exitBlock.ID,
			Location: stmt.Location,
		}
	}

	b.pushLoop(exitBlock.ID, condBlock.ID)
	b.setBlock(bodyBlock)
	if stmt.Body != nil {
		b.lowerBlock(stmt.Body)
	}
	b.branchIfNoTerm(condBlock.ID, stmt.Location)
	b.popLoop()

	b.setBlock(exitBlock)
}

func (b *functionBuilder) lowerMatch(stmt *hir.MatchStmt) {
	if stmt == nil {
		return
	}

	cond := b.lowerExpr(stmt.Expr)
	if cond == mir.InvalidValue {
		b.current.Term = &mir.Unreachable{Location: stmt.Location}
		return
	}

	mergeBlock := b.newBlock("match.end", stmt.Location)
	defaultBlock := mergeBlock
	cases := make([]mir.SwitchCase, 0, len(stmt.Cases))

	caseBlocks := make([]*mir.Block, 0, len(stmt.Cases))
	for _, clause := range stmt.Cases {
		block := b.newBlock("match.case", clause.Location)
		caseBlocks = append(caseBlocks, block)

		if clause.Pattern == nil {
			defaultBlock = block
			continue
		}

		if lit, ok := clause.Pattern.(*hir.Literal); ok {
			cases = append(cases, mir.SwitchCase{
				Value:  lit.Value,
				Target: block.ID,
			})
			continue
		}

		b.reportUnsupported("match pattern", clause.Pattern.Loc())
	}

	b.current.Term = &mir.Switch{
		Cond:     cond,
		Cases:    cases,
		Default:  defaultBlock.ID,
		Location: stmt.Location,
	}

	for idx, clause := range stmt.Cases {
		block := caseBlocks[idx]
		b.setBlock(block)
		if clause.Body != nil {
			b.lowerBlock(clause.Body)
		}
		b.branchIfNoTerm(mergeBlock.ID, clause.Location)
	}

	b.setBlock(mergeBlock)
}

func (b *functionBuilder) lowerExpr(expr hir.Expr) mir.ValueID {
	if expr == nil {
		return mir.InvalidValue
	}

	switch e := expr.(type) {
	case *hir.Literal:
		if isLargePrimitiveType(e.Type) {
			return b.emitLargeConst(e.Type, e.Value, e.Location)
		}
		return b.emitConst(e.Type, e.Value, e.Location)
	case *hir.Ident:
		return b.loadIdent(e)
	case *hir.OptionalNone:
		id := b.gen.nextValueID()
		b.emitInstr(&mir.OptionalNone{
			Result:   id,
			Type:     e.Type,
			Location: e.Location,
		})
		return id
	case *hir.OptionalSome:
		value := b.lowerExpr(e.Value)
		if value == mir.InvalidValue {
			return mir.InvalidValue
		}
		id := b.gen.nextValueID()
		b.emitInstr(&mir.OptionalSome{
			Result:   id,
			Value:    value,
			Type:     e.Type,
			Location: e.Location,
		})
		return id
	case *hir.OptionalIsSome:
		value := b.lowerExpr(e.Value)
		if value == mir.InvalidValue {
			return mir.InvalidValue
		}
		id := b.gen.nextValueID()
		b.emitInstr(&mir.OptionalIsSome{
			Result:   id,
			Value:    value,
			Location: e.Location,
		})
		return id
	case *hir.OptionalIsNone:
		value := b.lowerExpr(e.Value)
		if value == mir.InvalidValue {
			return mir.InvalidValue
		}
		isSome := b.gen.nextValueID()
		b.emitInstr(&mir.OptionalIsSome{
			Result:   isSome,
			Value:    value,
			Location: e.Location,
		})
		return b.emitUnary(tokens.NOT_TOKEN, isSome, types.TypeBool, e.Location)
	case *hir.OptionalUnwrap:
		value := b.lowerExpr(e.Value)
		if value == mir.InvalidValue {
			return mir.InvalidValue
		}
		defaultVal := mir.InvalidValue
		hasDefault := false
		if e.Default != nil {
			defaultVal = b.lowerExpr(e.Default)
			if defaultVal == mir.InvalidValue {
				return mir.InvalidValue
			}
			hasDefault = true
		}
		id := b.gen.nextValueID()
		b.emitInstr(&mir.OptionalUnwrap{
			Result:     id,
			Value:      value,
			Default:    defaultVal,
			HasDefault: hasDefault,
			Type:       e.Type,
			Location:   e.Location,
		})
		return id
	case *hir.ResultOk:
		value := b.lowerExpr(e.Value)
		if value == mir.InvalidValue {
			return mir.InvalidValue
		}
		id := b.gen.nextValueID()
		b.emitInstr(&mir.ResultOk{
			Result:   id,
			Value:    value,
			Type:     e.Type,
			Location: e.Location,
		})
		return id
	case *hir.ResultErr:
		value := b.lowerExpr(e.Value)
		if value == mir.InvalidValue {
			return mir.InvalidValue
		}
		id := b.gen.nextValueID()
		b.emitInstr(&mir.ResultErr{
			Result:   id,
			Value:    value,
			Type:     e.Type,
			Location: e.Location,
		})
		return id
	case *hir.ResultUnwrap:
		if e.Catch != nil {
			b.reportUnsupported("result catch", e.Loc())
			return mir.InvalidValue
		}
		value := b.lowerExpr(e.Value)
		if value == mir.InvalidValue {
			return mir.InvalidValue
		}
		id := b.gen.nextValueID()
		b.emitInstr(&mir.ResultUnwrap{
			Result:     id,
			Value:      value,
			Default:    mir.InvalidValue,
			HasDefault: false,
			Type:       e.Type,
			Location:   e.Location,
		})
		return id
	case *hir.BinaryExpr:
		left := b.lowerExpr(e.X)
		right := b.lowerExpr(e.Y)
		if left == mir.InvalidValue || right == mir.InvalidValue {
			return mir.InvalidValue
		}
		if isLargePrimitiveType(b.exprType(e.X)) {
			if isCompareOp(e.Op.Kind) {
				return b.emitLargeCompare(e.Op.Kind, left, right, b.exprType(e.X), e.Location)
			}
			return b.emitLargeBinary(e.Op.Kind, left, right, b.exprType(e.X), e.Location)
		}
		return b.emitBinary(e.Op.Kind, left, right, e.Type, e.Location)
	case *hir.UnaryExpr:
		operand := b.lowerExpr(e.X)
		if operand == mir.InvalidValue {
			return mir.InvalidValue
		}
		return b.emitUnary(e.Op.Kind, operand, e.Type, e.Location)
	case *hir.PrefixExpr:
		return b.lowerPrefix(e)
	case *hir.PostfixExpr:
		return b.lowerPostfix(e)
	case *hir.ParenExpr:
		return b.lowerExpr(e.X)
	case *hir.CallExpr:
		return b.lowerCall(e)
	case *hir.IndexExpr:
		return b.lowerIndexValue(e)
	case *hir.CastExpr:
		value := b.lowerExpr(e.X)
		if value == mir.InvalidValue {
			return mir.InvalidValue
		}
		return b.castValue(value, b.exprType(e.X), e.Type, e.Location)
	case *hir.CoalescingExpr:
		cond := b.lowerExpr(e.Cond)
		if cond == mir.InvalidValue {
			return mir.InvalidValue
		}
		def := b.lowerExpr(e.Default)
		if def == mir.InvalidValue {
			return mir.InvalidValue
		}
		id := b.gen.nextValueID()
		b.emitInstr(&mir.OptionalUnwrap{
			Result:     id,
			Value:      cond,
			Default:    def,
			HasDefault: true,
			Type:       e.Type,
			Location:   e.Location,
		})
		return id
	case *hir.ArrayLenExpr:
		arrVal := b.lowerExpr(e.X)
		if arrVal == mir.InvalidValue {
			return mir.InvalidValue
		}
		if arrType := b.arrayTypeOf(e.X); arrType != nil && arrType.Length >= 0 {
			return b.emitConst(types.TypeI32, strconv.Itoa(arrType.Length), e.Location)
		}
		return b.emitArrayLen(arrVal, e.Location)
	case *hir.SelectorExpr:
		return b.lowerSelector(e)
	case *hir.ScopeResolutionExpr:
		return b.lowerQualifiedValue(e)
	default:
		b.reportUnsupported("expression", expr.Loc())
		return mir.InvalidValue
	}
}

func (b *functionBuilder) lowerPrefix(expr *hir.PrefixExpr) mir.ValueID {
	if expr == nil {
		return mir.InvalidValue
	}

	if expr.Op.Kind != tokens.PLUS_PLUS_TOKEN && expr.Op.Kind != tokens.MINUS_MINUS_TOKEN {
		operand := b.lowerExpr(expr.X)
		if operand == mir.InvalidValue {
			return mir.InvalidValue
		}
		return b.emitUnary(expr.Op.Kind, operand, expr.Type, expr.Location)
	}

	addr := b.lowerLValue(expr.X)
	if addr == mir.InvalidValue {
		return mir.InvalidValue
	}

	cur := b.emitLoad(addr, b.exprType(expr.X), expr.Location)
	if cur == mir.InvalidValue {
		return mir.InvalidValue
	}

	typ := b.exprType(expr.X)
	one := mir.InvalidValue
	if isLargePrimitiveType(typ) {
		one = b.emitLargeConst(typ, "1", expr.Location)
	} else {
		one = b.emitConst(typ, "1", expr.Location)
	}
	op := tokens.PLUS_TOKEN
	if expr.Op.Kind == tokens.MINUS_MINUS_TOKEN {
		op = tokens.MINUS_TOKEN
	}
	next := b.emitBinary(op, cur, one, typ, expr.Location)
	b.emitStore(addr, next, expr.Location)
	return next
}

func (b *functionBuilder) lowerPostfix(expr *hir.PostfixExpr) mir.ValueID {
	if expr == nil {
		return mir.InvalidValue
	}

	if expr.Op.Kind != tokens.PLUS_PLUS_TOKEN && expr.Op.Kind != tokens.MINUS_MINUS_TOKEN {
		operand := b.lowerExpr(expr.X)
		if operand == mir.InvalidValue {
			return mir.InvalidValue
		}
		return b.emitUnary(expr.Op.Kind, operand, expr.Type, expr.Location)
	}

	addr := b.lowerLValue(expr.X)
	if addr == mir.InvalidValue {
		return mir.InvalidValue
	}

	cur := b.emitLoad(addr, b.exprType(expr.X), expr.Location)
	if cur == mir.InvalidValue {
		return mir.InvalidValue
	}

	typ := b.exprType(expr.X)
	one := mir.InvalidValue
	if isLargePrimitiveType(typ) {
		one = b.emitLargeConst(typ, "1", expr.Location)
	} else {
		one = b.emitConst(typ, "1", expr.Location)
	}
	op := tokens.PLUS_TOKEN
	if expr.Op.Kind == tokens.MINUS_MINUS_TOKEN {
		op = tokens.MINUS_TOKEN
	}
	next := b.emitBinary(op, cur, one, typ, expr.Location)
	b.emitStore(addr, next, expr.Location)
	return cur
}

func (b *functionBuilder) lowerCall(expr *hir.CallExpr) mir.ValueID {
	if expr == nil {
		return mir.InvalidValue
	}

	target, ok := b.callTarget(expr.Fun)
	if !ok {
		b.reportUnsupported("call target", &expr.Location)
		return mir.InvalidValue
	}

	args := make([]mir.ValueID, 0, len(expr.Args))
	for _, arg := range expr.Args {
		val := b.lowerExpr(arg)
		if val == mir.InvalidValue {
			return mir.InvalidValue
		}
		args = append(args, val)
	}

	retType := expr.Type
	if isLargePrimitiveType(retType) {
		out := b.emitAlloca(retType, expr.Location)
		callArgs := append([]mir.ValueID{out}, args...)
		b.emitInstr(&mir.Call{
			Result:   mir.InvalidValue,
			Target:   target,
			Args:     callArgs,
			Type:     types.TypeVoid,
			Location: expr.Location,
		})
		if expr.Catch != nil {
			b.reportUnsupported("catch clause", &expr.Location)
		}
		return out
	}

	result := mir.InvalidValue
	if retType != nil && !retType.Equals(types.TypeVoid) {
		result = b.gen.nextValueID()
	}

	b.emitInstr(&mir.Call{
		Result:   result,
		Target:   target,
		Args:     args,
		Type:     retType,
		Location: expr.Location,
	})

	if expr.Catch != nil {
		b.reportUnsupported("catch clause", &expr.Location)
	}

	return result
}

func (b *functionBuilder) lowerQualifiedValue(expr *hir.ScopeResolutionExpr) mir.ValueID {
	if expr == nil {
		return mir.InvalidValue
	}

	if name, ok := b.qualifiedName(expr); ok {
		b.reportUnsupported(fmt.Sprintf("qualified value %s", name), &expr.Location)
		return mir.InvalidValue
	}

	b.reportUnsupported("qualified value", &expr.Location)
	return mir.InvalidValue
}

func (b *functionBuilder) lowerSelector(expr *hir.SelectorExpr) mir.ValueID {
	if expr == nil {
		return mir.InvalidValue
	}

	addr := b.lowerFieldAddr(expr)
	if addr == mir.InvalidValue {
		return mir.InvalidValue
	}

	return b.emitLoad(addr, expr.Type, expr.Location)
}

func (b *functionBuilder) lowerLValue(expr hir.Expr) mir.ValueID {
	switch e := expr.(type) {
	case *hir.Ident:
		return b.addrForIdent(e)
	case *hir.SelectorExpr:
		return b.lowerFieldAddr(e)
	case *hir.IndexExpr:
		return b.lowerIndexAddr(e)
	default:
		b.reportUnsupported("lvalue", expr.Loc())
		return mir.InvalidValue
	}
}

func (b *functionBuilder) loadIdent(ident *hir.Ident) mir.ValueID {
	if ident == nil {
		return mir.InvalidValue
	}

	if addr := b.addrForIdent(ident); addr != mir.InvalidValue {
		return b.emitLoad(addr, ident.Type, ident.Location)
	}

	if ident.Symbol != nil && (ident.Symbol.Kind == symbols.SymbolParameter || ident.Symbol.Kind == symbols.SymbolReceiver) {
		if val, ok := b.paramsByName[ident.Name]; ok {
			return val
		}
	}

	if val, ok := b.paramsByName[ident.Name]; ok {
		return val
	}

	b.reportUnsupported("identifier", &ident.Location)
	return mir.InvalidValue
}

func (b *functionBuilder) addrForIdent(ident *hir.Ident) mir.ValueID {
	if ident == nil {
		return mir.InvalidValue
	}

	if ident.Symbol != nil {
		if addr, ok := b.slots[ident.Symbol]; ok {
			return addr
		}
		if ident.Symbol.Kind == symbols.SymbolParameter || ident.Symbol.Kind == symbols.SymbolReceiver {
			if val, ok := b.paramsByName[ident.Name]; ok {
				addr := b.emitAlloca(ident.Type, ident.Location)
				b.emitStore(addr, val, ident.Location)
				b.slots[ident.Symbol] = addr
				return addr
			}
		}
	}

	return mir.InvalidValue
}

func (b *functionBuilder) lowerFieldAddr(expr *hir.SelectorExpr) mir.ValueID {
	if expr == nil || expr.Field == nil {
		return mir.InvalidValue
	}

	baseIdent, ok := expr.X.(*hir.Ident)
	if !ok || baseIdent == nil {
		b.reportUnsupported("selector base", expr.Loc())
		return mir.InvalidValue
	}

	baseAddr := b.addrForIdent(baseIdent)
	if baseAddr == mir.InvalidValue {
		return mir.InvalidValue
	}

	baseType := types.UnwrapType(baseIdent.Type)
	basePtr := baseAddr
	if ref, ok := baseType.(*types.ReferenceType); ok {
		basePtr = b.emitLoad(baseAddr, baseIdent.Type, baseIdent.Location)
		baseType = types.UnwrapType(ref.Inner)
		b.ptrElem[basePtr] = ref.Inner
	}

	structType, ok := baseType.(*types.StructType)
	if !ok {
		b.reportUnsupported("selector struct", expr.Loc())
		return mir.InvalidValue
	}

	layout := b.gen.layout.StructLayout(structType)
	offset, ok := layout.FieldOffset(expr.Field.Name)
	if !ok {
		b.reportUnsupported("selector field", expr.Loc())
		return mir.InvalidValue
	}

	return b.emitPtrAdd(basePtr, offset, expr.Type, expr.Location)
}

func (b *functionBuilder) lowerArrayLiteralInto(addr mir.ValueID, arrType *types.ArrayType, lit *hir.CompositeLit) {
	if arrType == nil || lit == nil {
		return
	}

	elemSize := b.gen.layout.SizeOf(arrType.Element)
	if elemSize <= 0 {
		b.reportUnsupported("array element size", lit.Loc())
		return
	}

	for i, elt := range lit.Elts {
		if i >= arrType.Length {
			break
		}
		if _, ok := elt.(*hir.KeyValueExpr); ok {
			b.reportUnsupported("array key/value literal", elt.Loc())
			return
		}
		value := b.lowerExpr(elt)
		if value == mir.InvalidValue {
			return
		}
		offset := i * elemSize
		elemAddr := b.emitPtrAdd(addr, offset, arrType.Element, lit.Location)
		b.emitStore(elemAddr, value, lit.Location)
	}
}

func (b *functionBuilder) lowerIndexValue(expr *hir.IndexExpr) mir.ValueID {
	if expr == nil {
		return mir.InvalidValue
	}

	if mapType := b.mapTypeOf(expr.X); mapType != nil {
		return b.lowerMapIndexValue(expr, mapType)
	}

	if arrType := b.arrayTypeOf(expr.X); arrType != nil && arrType.Length < 0 {
		return b.lowerDynamicIndexValue(expr, arrType)
	}

	addr := b.lowerIndexAddr(expr)
	if addr == mir.InvalidValue {
		return mir.InvalidValue
	}

	return b.emitLoad(addr, expr.Type, expr.Location)
}

func (b *functionBuilder) lowerIndexAddr(expr *hir.IndexExpr) mir.ValueID {
	if expr == nil {
		return mir.InvalidValue
	}

	arrType := b.arrayTypeOf(expr.X)
	if arrType == nil {
		b.reportUnsupported("index base", expr.Loc())
		return mir.InvalidValue
	}

	if arrType.Length < 0 {
		b.reportUnsupported("dynamic array address", expr.Loc())
		return mir.InvalidValue
	}

	baseIdent, ok := expr.X.(*hir.Ident)
	if !ok || baseIdent == nil {
		b.reportUnsupported("index base", expr.Loc())
		return mir.InvalidValue
	}

	baseAddr := b.addrForIdent(baseIdent)
	if baseAddr == mir.InvalidValue {
		return mir.InvalidValue
	}

	baseType := types.UnwrapType(baseIdent.Type)
	basePtr := baseAddr
	if ref, ok := baseType.(*types.ReferenceType); ok {
		basePtr = b.emitLoad(baseAddr, baseIdent.Type, baseIdent.Location)
		baseType = types.UnwrapType(ref.Inner)
		b.ptrElem[basePtr] = ref.Inner
	}

	elemSize := b.gen.layout.SizeOf(arrType.Element)
	if elemSize <= 0 {
		b.reportUnsupported("array element size", expr.Loc())
		return mir.InvalidValue
	}

	index, ok := b.constArrayIndex(expr, arrType)
	if !ok {
		if b.gen == nil || b.gen.ctx == nil || !b.gen.ctx.Diagnostics.HasErrors() {
			b.reportUnsupported("array index", expr.Loc())
		}
		return mir.InvalidValue
	}

	offset := index * elemSize
	return b.emitPtrAdd(basePtr, offset, arrType.Element, expr.Location)
}

func (b *functionBuilder) lowerIndexAssign(expr *hir.IndexExpr, rhs hir.Expr, op *tokens.Token, loc source.Location) {
	if expr == nil {
		return
	}

	if mapType := b.mapTypeOf(expr.X); mapType != nil {
		b.lowerMapIndexAssign(expr, rhs, op, loc, mapType)
		return
	}

	arrType := b.arrayTypeOf(expr.X)
	if arrType == nil {
		b.reportUnsupported("index base", expr.Loc())
		return
	}

	if arrType.Length < 0 {
		b.lowerDynamicIndexAssign(expr, rhs, op, loc, arrType)
		return
	}

	addr := b.lowerIndexAddr(expr)
	if addr == mir.InvalidValue {
		return
	}

	if op != nil && op.Kind != tokens.EQUALS_TOKEN {
		cur := b.emitLoad(addr, expr.Type, loc)
		rhsVal := b.lowerExpr(rhs)
		if cur == mir.InvalidValue || rhsVal == mir.InvalidValue {
			return
		}
		opKind := assignTokenToBinary(op.Kind)
		if opKind == "" {
			b.reportUnsupported("assignment operator", &loc)
			return
		}
		res := b.emitBinary(opKind, cur, rhsVal, expr.Type, loc)
		b.emitStore(addr, res, loc)
		return
	}

	rhsVal := b.lowerExpr(rhs)
	if rhsVal == mir.InvalidValue {
		return
	}
	b.emitStore(addr, rhsVal, loc)
}

func (b *functionBuilder) lowerDynamicIndexValue(expr *hir.IndexExpr, arrType *types.ArrayType) mir.ValueID {
	arrVal := b.lowerExpr(expr.X)
	if arrVal == mir.InvalidValue {
		return mir.InvalidValue
	}

	indexVal := b.lowerExpr(expr.Index)
	if indexVal == mir.InvalidValue {
		return mir.InvalidValue
	}

	indexVal = b.castValue(indexVal, b.exprType(expr.Index), types.TypeI32, expr.Location)
	lenVal := b.emitArrayLen(arrVal, expr.Location)
	indexVal = b.emitBoundsCheckedIndex(indexVal, lenVal, types.TypeI32, expr.Location)
	if indexVal == mir.InvalidValue {
		return mir.InvalidValue
	}
	return b.emitDynamicArrayGet(arrVal, indexVal, arrType.Element, expr.Location)
}

func (b *functionBuilder) lowerDynamicIndexAssign(expr *hir.IndexExpr, rhs hir.Expr, op *tokens.Token, loc source.Location, arrType *types.ArrayType) {
	arrVal := b.lowerExpr(expr.X)
	if arrVal == mir.InvalidValue {
		return
	}

	indexVal := b.lowerExpr(expr.Index)
	if indexVal == mir.InvalidValue {
		return
	}

	indexVal = b.castValue(indexVal, b.exprType(expr.Index), types.TypeI32, expr.Location)

	value := b.lowerExpr(rhs)
	if value == mir.InvalidValue {
		return
	}

	lenVal := b.emitArrayLen(arrVal, loc)
	indexVal = b.emitBoundsCheckedIndex(indexVal, lenVal, types.TypeI32, loc)
	if indexVal == mir.InvalidValue {
		return
	}

	if op != nil && op.Kind != tokens.EQUALS_TOKEN {
		cur := b.emitDynamicArrayGet(arrVal, indexVal, arrType.Element, loc)
		if cur == mir.InvalidValue {
			return
		}
		opKind := assignTokenToBinary(op.Kind)
		if opKind == "" {
			b.reportUnsupported("assignment operator", &loc)
			return
		}
		value = b.emitBinary(opKind, cur, value, expr.Type, loc)
	}

	temp := b.emitAlloca(arrType.Element, loc)
	b.emitStore(temp, value, loc)

	b.emitInstr(&mir.Call{
		Result:   mir.InvalidValue,
		Target:   "ferret_array_set",
		Args:     []mir.ValueID{arrVal, indexVal, temp},
		Type:     types.TypeBool,
		Location: loc,
	})
}

func (b *functionBuilder) constArrayIndex(expr *hir.IndexExpr, arrType *types.ArrayType) (int, bool) {
	if expr == nil || arrType == nil || arrType.Length < 0 {
		return 0, false
	}
	if b.gen == nil || b.gen.ctx == nil || b.gen.mod == nil {
		return 0, false
	}

	val := consteval.EvaluateHIRExpr(b.gen.ctx, b.gen.mod, expr.Index)
	if val == nil {
		return 0, false
	}
	index, ok := val.AsInt64()
	if !ok {
		return 0, false
	}

	if index < 0 {
		index = int64(arrType.Length) + index
	}
	if index < 0 || index >= int64(arrType.Length) {
		return 0, false
	}
	return int(index), true
}

func (b *functionBuilder) lowerMapIndexValue(expr *hir.IndexExpr, mapType *types.MapType) mir.ValueID {
	if expr == nil || mapType == nil {
		return mir.InvalidValue
	}

	mapVal := b.lowerExpr(expr.X)
	if mapVal == mir.InvalidValue {
		return mir.InvalidValue
	}

	keyVal := b.lowerExpr(expr.Index)
	if keyVal == mir.InvalidValue {
		return mir.InvalidValue
	}

	if mapType.Key != nil {
		keyVal = b.castValue(keyVal, b.exprType(expr.Index), mapType.Key, expr.Location)
	}

	result := b.gen.nextValueID()
	b.emitInstr(&mir.MapGet{
		Result:   result,
		Map:      mapVal,
		Key:      keyVal,
		Type:     expr.Type,
		Location: expr.Location,
	})
	return result
}

func (b *functionBuilder) lowerMapIndexAssign(expr *hir.IndexExpr, rhs hir.Expr, op *tokens.Token, loc source.Location, mapType *types.MapType) {
	if expr == nil || mapType == nil {
		return
	}
	if op != nil && op.Kind != tokens.EQUALS_TOKEN {
		b.reportUnsupported("map compound assignment", &loc)
		return
	}

	mapVal := b.lowerExpr(expr.X)
	if mapVal == mir.InvalidValue {
		return
	}

	keyVal := b.lowerExpr(expr.Index)
	if keyVal == mir.InvalidValue {
		return
	}

	valueVal := b.lowerExpr(rhs)
	if valueVal == mir.InvalidValue {
		return
	}

	if mapType.Key != nil {
		keyVal = b.castValue(keyVal, b.exprType(expr.Index), mapType.Key, expr.Location)
	}
	if mapType.Value != nil {
		valueVal = b.castValue(valueVal, b.exprType(rhs), mapType.Value, loc)
	}

	b.emitInstr(&mir.MapSet{
		Map:      mapVal,
		Key:      keyVal,
		Value:    valueVal,
		Location: loc,
	})
}

func (b *functionBuilder) arrayTypeOf(expr hir.Expr) *types.ArrayType {
	if expr == nil {
		return nil
	}

	baseType := b.exprType(expr)
	if baseType == nil {
		return nil
	}
	baseType = types.UnwrapType(baseType)
	if ref, ok := baseType.(*types.ReferenceType); ok {
		baseType = types.UnwrapType(ref.Inner)
	}

	arrType, _ := baseType.(*types.ArrayType)
	return arrType
}

func (b *functionBuilder) mapTypeOf(expr hir.Expr) *types.MapType {
	if expr == nil {
		return nil
	}

	baseType := b.exprType(expr)
	if baseType == nil {
		return nil
	}
	baseType = types.UnwrapType(baseType)
	if ref, ok := baseType.(*types.ReferenceType); ok {
		baseType = types.UnwrapType(ref.Inner)
	}

	mapType, _ := baseType.(*types.MapType)
	return mapType
}

func (b *functionBuilder) callTarget(expr hir.Expr) (string, bool) {
	if expr == nil {
		return "", false
	}

	switch e := expr.(type) {
	case *hir.Ident:
		return e.Name, true
	case *hir.ScopeResolutionExpr:
		return b.qualifiedName(e)
	default:
		return "", false
	}
}

func (b *functionBuilder) qualifiedName(expr hir.Expr) (string, bool) {
	switch e := expr.(type) {
	case *hir.Ident:
		return e.Name, e.Name != ""
	case *hir.ScopeResolutionExpr:
		left, ok := b.qualifiedName(e.X)
		if !ok || e.Selector == nil || e.Selector.Name == "" {
			return "", false
		}
		return left + "::" + e.Selector.Name, true
	default:
		return "", false
	}
}

func (b *functionBuilder) exprType(expr hir.Expr) types.SemType {
	switch e := expr.(type) {
	case *hir.Literal:
		return e.Type
	case *hir.Ident:
		return e.Type
	case *hir.BinaryExpr:
		return e.Type
	case *hir.UnaryExpr:
		return e.Type
	case *hir.PrefixExpr:
		return e.Type
	case *hir.PostfixExpr:
		return e.Type
	case *hir.CallExpr:
		return e.Type
	case *hir.IndexExpr:
		return e.Type
	case *hir.SelectorExpr:
		return e.Type
	case *hir.ScopeResolutionExpr:
		return e.Type
	case *hir.ParenExpr:
		return e.Type
	default:
		return nil
	}
}

func (b *functionBuilder) emitConst(typ types.SemType, value string, loc source.Location) mir.ValueID {
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Const{
		Result:   id,
		Type:     typ,
		Value:    value,
		Location: loc,
	})
	return id
}

func (b *functionBuilder) emitMemcpy(dst, src mir.ValueID, typ types.SemType, loc source.Location) {
	if typ == nil || typ.Size() <= 0 {
		b.reportUnsupported("memcpy size", &loc)
		return
	}
	size := strconv.FormatInt(int64(typ.Size()), 10)
	sizeVal := b.emitConst(types.TypeU64, size, loc)
	b.emitInstr(&mir.Call{
		Result:   mir.InvalidValue,
		Target:   "ferret_memcpy",
		Args:     []mir.ValueID{dst, src, sizeVal},
		Type:     types.TypeVoid,
		Location: loc,
	})
}

func (b *functionBuilder) emitLargeConst(typ types.SemType, value string, loc source.Location) mir.ValueID {
	typeName, ok := largePrimitiveName(typ)
	if !ok {
		b.reportUnsupported("large const", &loc)
		return mir.InvalidValue
	}
	if isLargeIntName(typeName) {
		clean := strings.TrimSpace(value)
		sign := ""
		if strings.HasPrefix(clean, "-") {
			sign = "-"
			clean = strings.TrimPrefix(clean, "-")
		}
		if bigInt, err := numeric.StringToBigInt(clean); err == nil {
			if sign == "-" {
				bigInt.Neg(bigInt)
			}
			value = bigInt.String()
		} else {
			value = strings.ReplaceAll(value, "_", "")
		}
	} else {
		value = strings.ReplaceAll(value, "_", "")
	}
	out := b.emitAlloca(typ, loc)
	lit := b.emitConst(types.TypeString, value, loc)
	b.emitInstr(&mir.Call{
		Result:   mir.InvalidValue,
		Target:   "ferret_" + typeName + "_from_string_ptr",
		Args:     []mir.ValueID{lit, out},
		Type:     types.TypeVoid,
		Location: loc,
	})
	return out
}

func (b *functionBuilder) emitLargeBinary(op tokens.TOKEN, left, right mir.ValueID, typ types.SemType, loc source.Location) mir.ValueID {
	typeName, ok := largePrimitiveName(typ)
	if !ok {
		b.reportUnsupported("large binary", &loc)
		return mir.InvalidValue
	}
	fn, ok := largeBinaryFunc(op, typeName)
	if !ok {
		b.reportUnsupported(fmt.Sprintf("binary op %s", op), &loc)
		return mir.InvalidValue
	}
	out := b.emitAlloca(typ, loc)
	b.emitInstr(&mir.Call{
		Result:   mir.InvalidValue,
		Target:   fn,
		Args:     []mir.ValueID{left, right, out},
		Type:     types.TypeVoid,
		Location: loc,
	})
	return out
}

func (b *functionBuilder) emitLargeCompare(op tokens.TOKEN, left, right mir.ValueID, typ types.SemType, loc source.Location) mir.ValueID {
	typeName, ok := largePrimitiveName(typ)
	if !ok {
		b.reportUnsupported("large compare", &loc)
		return mir.InvalidValue
	}

	switch op {
	case tokens.DOUBLE_EQUAL_TOKEN:
		return b.emitLargeCompareCall(typeName, "eq", left, right, loc)
	case tokens.NOT_EQUAL_TOKEN:
		eq := b.emitLargeCompareCall(typeName, "eq", left, right, loc)
		return b.emitUnary(tokens.NOT_TOKEN, eq, types.TypeBool, loc)
	case tokens.LESS_TOKEN:
		return b.emitLargeCompareCall(typeName, "lt", left, right, loc)
	case tokens.GREATER_TOKEN:
		return b.emitLargeCompareCall(typeName, "gt", left, right, loc)
	case tokens.LESS_EQUAL_TOKEN:
		lt := b.emitLargeCompareCall(typeName, "lt", left, right, loc)
		eq := b.emitLargeCompareCall(typeName, "eq", left, right, loc)
		return b.emitBinary(tokens.OR_TOKEN, lt, eq, types.TypeBool, loc)
	case tokens.GREATER_EQUAL_TOKEN:
		gt := b.emitLargeCompareCall(typeName, "gt", left, right, loc)
		eq := b.emitLargeCompareCall(typeName, "eq", left, right, loc)
		return b.emitBinary(tokens.OR_TOKEN, gt, eq, types.TypeBool, loc)
	default:
		b.reportUnsupported(fmt.Sprintf("compare op %s", op), &loc)
		return mir.InvalidValue
	}
}

func (b *functionBuilder) emitLargeCompareCall(typeName, op string, left, right mir.ValueID, loc source.Location) mir.ValueID {
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Call{
		Result:   id,
		Target:   "ferret_" + typeName + "_" + op + "_ptr",
		Args:     []mir.ValueID{left, right},
		Type:     types.TypeBool,
		Location: loc,
	})
	return id
}

func (b *functionBuilder) emitLargeUnary(op tokens.TOKEN, value mir.ValueID, typ types.SemType, loc source.Location) mir.ValueID {
	switch op {
	case tokens.PLUS_TOKEN:
		return value
	case tokens.MINUS_TOKEN:
		zero := b.emitLargeConst(typ, "0", loc)
		return b.emitLargeBinary(tokens.MINUS_TOKEN, zero, value, typ, loc)
	default:
		b.reportUnsupported(fmt.Sprintf("unary op %s", op), &loc)
		return mir.InvalidValue
	}
}

func (b *functionBuilder) emitLargeCast(value mir.ValueID, from, to types.SemType, loc source.Location) mir.ValueID {
	fromName, fromLarge := largePrimitiveName(from)
	toName, toLarge := largePrimitiveName(to)

	if fromLarge && toLarge {
		if fromName == toName {
			return value
		}
		out := b.emitAlloca(to, loc)
		str := b.emitLargeToString(fromName, value, loc)
		b.emitInstr(&mir.Call{
			Result:   mir.InvalidValue,
			Target:   "ferret_" + toName + "_from_string_ptr",
			Args:     []mir.ValueID{str, out},
			Type:     types.TypeVoid,
			Location: loc,
		})
		return out
	}

	if toLarge && !fromLarge {
		fn, argType, ok := largeFromSmallFunc(toName, from)
		if !ok {
			b.reportUnsupported("cast to large type", &loc)
			return mir.InvalidValue
		}
		if argType != nil && !argType.Equals(from) {
			value = b.castValue(value, from, argType, loc)
		}
		out := b.emitAlloca(to, loc)
		b.emitInstr(&mir.Call{
			Result:   mir.InvalidValue,
			Target:   fn,
			Args:     []mir.ValueID{value, out},
			Type:     types.TypeVoid,
			Location: loc,
		})
		return out
	}

	if fromLarge && !toLarge {
		fn, retType, ok := largeToSmallFunc(fromName, to)
		if !ok {
			b.reportUnsupported("cast from large type", &loc)
			return mir.InvalidValue
		}
		id := b.gen.nextValueID()
		b.emitInstr(&mir.Call{
			Result:   id,
			Target:   fn,
			Args:     []mir.ValueID{value},
			Type:     retType,
			Location: loc,
		})
		if retType == nil || to == nil || retType.Equals(to) {
			return id
		}
		return b.emitCast(id, to, loc)
	}

	return b.emitCast(value, to, loc)
}

func (b *functionBuilder) emitLargeToString(typeName string, value mir.ValueID, loc source.Location) mir.ValueID {
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Call{
		Result:   id,
		Target:   "ferret_" + typeName + "_to_string_ptr",
		Args:     []mir.ValueID{value},
		Type:     types.TypeString,
		Location: loc,
	})
	return id
}

func (b *functionBuilder) emitBinary(op tokens.TOKEN, left, right mir.ValueID, typ types.SemType, loc source.Location) mir.ValueID {
	if isLargePrimitiveType(typ) {
		return b.emitLargeBinary(op, left, right, typ, loc)
	}
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Binary{
		Result:   id,
		Op:       op,
		Left:     left,
		Right:    right,
		Type:     typ,
		Location: loc,
	})
	return id
}

func (b *functionBuilder) emitUnary(op tokens.TOKEN, value mir.ValueID, typ types.SemType, loc source.Location) mir.ValueID {
	if isLargePrimitiveType(typ) {
		return b.emitLargeUnary(op, value, typ, loc)
	}
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Unary{
		Result:   id,
		Op:       op,
		X:        value,
		Type:     typ,
		Location: loc,
	})
	return id
}

func (b *functionBuilder) emitCast(value mir.ValueID, typ types.SemType, loc source.Location) mir.ValueID {
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Cast{
		Result:   id,
		X:        value,
		Type:     typ,
		Location: loc,
	})
	return id
}

func (b *functionBuilder) emitAlloca(typ types.SemType, loc source.Location) mir.ValueID {
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Alloca{
		Result:   id,
		Type:     typ,
		Location: loc,
	})
	b.ptrElem[id] = typ
	return id
}

func (b *functionBuilder) emitLoad(addr mir.ValueID, typ types.SemType, loc source.Location) mir.ValueID {
	if isLargePrimitiveType(typ) {
		return addr
	}
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Load{
		Result:   id,
		Addr:     addr,
		Type:     typ,
		Location: loc,
	})
	if ref, ok := types.UnwrapType(typ).(*types.ReferenceType); ok {
		b.ptrElem[id] = ref.Inner
	}
	return id
}

func (b *functionBuilder) emitStore(addr, value mir.ValueID, loc source.Location) {
	if elem, ok := b.ptrElem[addr]; ok && isLargePrimitiveType(elem) {
		b.emitMemcpy(addr, value, elem, loc)
		return
	}
	b.emitInstr(&mir.Store{
		Addr:     addr,
		Value:    value,
		Location: loc,
	})
}

func (b *functionBuilder) emitPtrAdd(base mir.ValueID, offset int, elem types.SemType, loc source.Location) mir.ValueID {
	id := b.gen.nextValueID()
	b.emitInstr(&mir.PtrAdd{
		Result:   id,
		Base:     base,
		Offset:   offset,
		Elem:     elem,
		Location: loc,
	})
	b.ptrElem[id] = elem
	return id
}

func (b *functionBuilder) emitPtrOffset(base mir.ValueID, offset mir.ValueID, elem types.SemType, loc source.Location) mir.ValueID {
	id := b.gen.nextValueID()
	b.emitInstr(&mir.PtrOffset{
		Result:   id,
		Base:     base,
		Offset:   offset,
		Elem:     elem,
		Location: loc,
	})
	b.ptrElem[id] = elem
	return id
}

func (b *functionBuilder) emitArrayLen(arrVal mir.ValueID, loc source.Location) mir.ValueID {
	id := b.gen.nextValueID()
	b.emitInstr(&mir.Call{
		Result:   id,
		Target:   "ferret_array_len",
		Args:     []mir.ValueID{arrVal},
		Type:     types.TypeI32,
		Location: loc,
	})
	return id
}

func (b *functionBuilder) emitDynamicArrayGet(arrVal, indexVal mir.ValueID, elemType types.SemType, loc source.Location) mir.ValueID {
	ptrType := types.NewReference(elemType)
	ptrVal := b.gen.nextValueID()
	b.emitInstr(&mir.Call{
		Result:   ptrVal,
		Target:   "ferret_array_get",
		Args:     []mir.ValueID{arrVal, indexVal},
		Type:     ptrType,
		Location: loc,
	})
	return b.emitLoad(ptrVal, elemType, loc)
}

func (b *functionBuilder) emitBoundsCheckedIndex(indexVal, lenVal mir.ValueID, indexType types.SemType, loc source.Location) mir.ValueID {
	if indexVal == mir.InvalidValue || lenVal == mir.InvalidValue {
		return mir.InvalidValue
	}
	if b.current == nil || b.current.Term != nil {
		return indexVal
	}

	zero := b.emitConst(indexType, "0", loc)
	condNeg := b.emitBinary(tokens.LESS_TOKEN, indexVal, zero, types.TypeBool, loc)

	negBlock := b.newBlock("index.neg", loc)
	posBlock := b.newBlock("index.pos", loc)
	mergeBlock := b.newBlock("index.merge", loc)
	okBlock := b.newBlock("index.ok", loc)
	oobBlock := b.newBlock("index.oob", loc)

	b.current.Term = &mir.CondBr{
		Cond:     condNeg,
		Then:     negBlock.ID,
		Else:     posBlock.ID,
		Location: loc,
	}

	b.setBlock(negBlock)
	idxNeg := b.emitBinary(tokens.PLUS_TOKEN, lenVal, indexVal, indexType, loc)
	negBlock.Term = &mir.Br{Target: mergeBlock.ID, Location: loc}

	b.setBlock(posBlock)
	idxPos := indexVal
	posBlock.Term = &mir.Br{Target: mergeBlock.ID, Location: loc}

	b.setBlock(mergeBlock)
	idxAdj := b.gen.nextValueID()
	b.emitInstr(&mir.Phi{
		Result: idxAdj,
		Type:   indexType,
		Incoming: []mir.PhiIncoming{
			{Pred: negBlock.ID, Value: idxNeg},
			{Pred: posBlock.ID, Value: idxPos},
		},
		Location: loc,
	})

	condLow := b.emitBinary(tokens.LESS_TOKEN, idxAdj, zero, types.TypeBool, loc)
	condHigh := b.emitBinary(tokens.GREATER_EQUAL_TOKEN, idxAdj, lenVal, types.TypeBool, loc)
	condOOB := b.emitBinary(tokens.OR_TOKEN, condLow, condHigh, types.TypeBool, loc)
	mergeBlock.Term = &mir.CondBr{
		Cond:     condOOB,
		Then:     oobBlock.ID,
		Else:     okBlock.ID,
		Location: loc,
	}

	b.setBlock(oobBlock)
	oobBlock.Term = &mir.Unreachable{Location: loc}

	b.setBlock(okBlock)
	return idxAdj
}

func (b *functionBuilder) emitInstr(instr mir.Instr) {
	if b.current == nil || b.current.Term != nil || instr == nil {
		return
	}
	b.current.Instrs = append(b.current.Instrs, instr)
}

func (b *functionBuilder) newBlock(name string, loc source.Location) *mir.Block {
	block := &mir.Block{
		ID:       b.gen.nextBlockID(),
		Name:     name,
		Location: loc,
	}
	b.fn.Blocks = append(b.fn.Blocks, block)
	return block
}

func (b *functionBuilder) setBlock(block *mir.Block) {
	b.current = block
}

func (b *functionBuilder) branchIfNoTerm(target mir.BlockID, loc source.Location) {
	if b.current == nil || b.current.Term != nil {
		return
	}
	b.current.Term = &mir.Br{Target: target, Location: loc}
}

func (b *functionBuilder) pushLoop(breakTarget, continueTarget mir.BlockID) {
	b.loopStack = append(b.loopStack, loopTargets{
		breakTarget:    breakTarget,
		continueTarget: continueTarget,
	})
}

func (b *functionBuilder) popLoop() {
	if len(b.loopStack) == 0 {
		return
	}
	b.loopStack = b.loopStack[:len(b.loopStack)-1]
}

func (b *functionBuilder) currentLoop() *loopTargets {
	if len(b.loopStack) == 0 {
		return nil
	}
	return &b.loopStack[len(b.loopStack)-1]
}

func (b *functionBuilder) reportUnsupported(kind string, loc *source.Location) {
	if b.gen.ctx == nil {
		return
	}
	message := fmt.Sprintf("MIR lowering unsupported: %s", kind)
	b.gen.ctx.ReportError(message, loc)
}

func (b *functionBuilder) castValue(value mir.ValueID, from, to types.SemType, loc source.Location) mir.ValueID {
	if from == nil || to == nil {
		return value
	}
	if from.Equals(to) {
		return value
	}
	if isLargePrimitiveType(from) || isLargePrimitiveType(to) {
		return b.emitLargeCast(value, from, to, loc)
	}
	return b.emitCast(value, to, loc)
}

type loopTargets struct {
	breakTarget    mir.BlockID
	continueTarget mir.BlockID
}

func assignTokenToBinary(token tokens.TOKEN) tokens.TOKEN {
	switch token {
	case tokens.PLUS_EQUALS_TOKEN:
		return tokens.PLUS_TOKEN
	case tokens.MINUS_EQUALS_TOKEN:
		return tokens.MINUS_TOKEN
	case tokens.MUL_EQUALS_TOKEN:
		return tokens.MUL_TOKEN
	case tokens.DIV_EQUALS_TOKEN:
		return tokens.DIV_TOKEN
	case tokens.MOD_EQUALS_TOKEN:
		return tokens.MOD_TOKEN
	case tokens.POW_EQUALS_TOKEN:
		return tokens.EXP_TOKEN
	case tokens.EXP_EQUALS_TOKEN:
		return tokens.BIT_XOR_TOKEN
	default:
		return ""
	}
}

func isCompareOp(op tokens.TOKEN) bool {
	switch op {
	case tokens.LESS_TOKEN, tokens.LESS_EQUAL_TOKEN, tokens.GREATER_TOKEN, tokens.GREATER_EQUAL_TOKEN,
		tokens.DOUBLE_EQUAL_TOKEN, tokens.NOT_EQUAL_TOKEN:
		return true
	default:
		return false
	}
}

func largeBinaryFunc(op tokens.TOKEN, typeName string) (string, bool) {
	switch op {
	case tokens.PLUS_TOKEN:
		return "ferret_" + typeName + "_add_ptr", true
	case tokens.MINUS_TOKEN:
		return "ferret_" + typeName + "_sub_ptr", true
	case tokens.MUL_TOKEN:
		return "ferret_" + typeName + "_mul_ptr", true
	case tokens.DIV_TOKEN:
		return "ferret_" + typeName + "_div_ptr", true
	case tokens.MOD_TOKEN:
		if isLargeIntName(typeName) {
			return "ferret_" + typeName + "_mod_ptr", true
		}
	case tokens.BIT_AND_TOKEN:
		if isLargeIntName(typeName) {
			return "ferret_" + typeName + "_and_ptr", true
		}
	case tokens.BIT_OR_TOKEN:
		if isLargeIntName(typeName) {
			return "ferret_" + typeName + "_or_ptr", true
		}
	case tokens.BIT_XOR_TOKEN:
		if isLargeIntName(typeName) {
			return "ferret_" + typeName + "_xor_ptr", true
		}
	}
	return "", false
}

func largeFromSmallFunc(toName string, from types.SemType) (string, types.SemType, bool) {
	if from == nil {
		return "", nil, false
	}
	from = types.UnwrapType(from)
	if prim, ok := from.(*types.PrimitiveType); ok {
		if !types.IsNumericTypeName(prim.GetName()) {
			return "", nil, false
		}
		switch toName {
		case string(types.TYPE_I128), string(types.TYPE_I256):
			return "ferret_" + toName + "_from_i64_ptr", types.TypeI64, true
		case string(types.TYPE_U128), string(types.TYPE_U256):
			return "ferret_" + toName + "_from_u64_ptr", types.TypeU64, true
		case string(types.TYPE_F128), string(types.TYPE_F256):
			return "ferret_" + toName + "_from_f64_ptr", types.TypeF64, true
		}
	}
	return "", nil, false
}

func largeToSmallFunc(fromName string, to types.SemType) (string, types.SemType, bool) {
	if to == nil {
		return "", nil, false
	}
	to = types.UnwrapType(to)
	prim, ok := to.(*types.PrimitiveType)
	if !ok {
		return "", nil, false
	}
	if isLargeFloatName(fromName) {
		if types.IsFloatTypeName(prim.GetName()) || types.IsIntegerTypeName(prim.GetName()) {
			return "ferret_" + fromName + "_to_f64_ptr", types.TypeF64, true
		}
	}
	if isLargeIntName(fromName) {
		suffix := "i64"
		retType := types.TypeI64
		if fromName == string(types.TYPE_U128) || fromName == string(types.TYPE_U256) {
			suffix = "u64"
			retType = types.TypeU64
		}
		if types.IsIntegerTypeName(prim.GetName()) || types.IsFloatTypeName(prim.GetName()) {
			return "ferret_" + fromName + "_to_" + suffix + "_ptr", retType, true
		}
	}
	return "", nil, false
}
