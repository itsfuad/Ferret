package hirlower

import (
	"compiler/internal/context_v2"
	"compiler/internal/hir"
	"compiler/internal/source"
	"compiler/internal/tokens"
	"compiler/internal/types"
)

// Lowerer rewrites source-shaped HIR into a lowered, canonical form.
type Lowerer struct {
	ctx               *context_v2.CompilerContext
	currentReturnType types.SemType
}

// New creates a new HIR lowerer.
func New(ctx *context_v2.CompilerContext) *Lowerer {
	return &Lowerer{ctx: ctx}
}

// LowerModule performs a lowering pass over a HIR module.
// This pass makes optional operations explicit for later MIR/codegen stages.
func (l *Lowerer) LowerModule(mod *hir.Module) *hir.Module {
	if mod == nil {
		return nil
	}

	lowered := &hir.Module{
		ImportPath: mod.ImportPath,
		Location:   mod.Location,
	}

	if len(mod.Items) > 0 {
		lowered.Items = make([]hir.Node, 0, len(mod.Items))
		for _, item := range mod.Items {
			lowered.Items = append(lowered.Items, l.lowerNode(item))
		}
	}

	return lowered
}

func (l *Lowerer) lowerNode(node hir.Node) hir.Node {
	if node == nil {
		return nil
	}

	switch n := node.(type) {
	case *hir.FuncDecl:
		return l.lowerFuncDecl(n)
	case *hir.MethodDecl:
		return l.lowerMethodDecl(n)
	case *hir.VarDecl:
		return l.lowerVarDecl(n)
	case *hir.ConstDecl:
		return l.lowerConstDecl(n)
	case *hir.TypeDecl:
		return n
	case *hir.Block:
		return l.lowerBlock(n)
	case *hir.DeclStmt:
		return &hir.DeclStmt{Decl: l.lowerDecl(n.Decl), Location: n.Location}
	case *hir.AssignStmt:
		return l.lowerAssignStmt(n)
	case *hir.ReturnStmt:
		return l.lowerReturnStmt(n)
	case *hir.ExprStmt:
		return &hir.ExprStmt{X: l.lowerExpr(n.X, types.TypeUnknown), Location: n.Location}
	case *hir.IfStmt:
		return l.lowerIfStmt(n)
	case *hir.ForStmt:
		return l.lowerForStmt(n)
	case *hir.WhileStmt:
		return l.lowerWhileStmt(n)
	case *hir.MatchStmt:
		return l.lowerMatchStmt(n)
	case *hir.DeferStmt:
		return l.lowerDeferStmt(n)
	case *hir.ImportStmt, *hir.BreakStmt, *hir.ContinueStmt, *hir.Invalid:
		return n
	default:
		return n
	}
}

func (l *Lowerer) lowerDecl(decl hir.Decl) hir.Decl {
	if decl == nil {
		return nil
	}

	switch d := decl.(type) {
	case *hir.VarDecl:
		return l.lowerVarDecl(d)
	case *hir.ConstDecl:
		return l.lowerConstDecl(d)
	case *hir.TypeDecl:
		return d
	case *hir.FuncDecl:
		return l.lowerFuncDecl(d)
	case *hir.MethodDecl:
		return l.lowerMethodDecl(d)
	default:
		return d
	}
}

func (l *Lowerer) lowerBlock(block *hir.Block) *hir.Block {
	if block == nil {
		return nil
	}

	lowered := &hir.Block{Location: block.Location}
	if len(block.Nodes) > 0 {
		lowered.Nodes = make([]hir.Node, 0, len(block.Nodes))
		for _, node := range block.Nodes {
			lowered.Nodes = append(lowered.Nodes, l.lowerNode(node))
		}
	}
	return lowered
}

func (l *Lowerer) lowerFuncDecl(decl *hir.FuncDecl) *hir.FuncDecl {
	if decl == nil {
		return nil
	}

	prevReturn := l.currentReturnType
	l.currentReturnType = types.TypeVoid
	if decl.Type != nil && decl.Type.Return != nil {
		l.currentReturnType = decl.Type.Return
	}

	body := l.lowerBlock(decl.Body)
	l.currentReturnType = prevReturn

	return &hir.FuncDecl{
		Name:     decl.Name,
		Type:     decl.Type,
		Body:     body,
		Location: decl.Location,
	}
}

func (l *Lowerer) lowerMethodDecl(decl *hir.MethodDecl) *hir.MethodDecl {
	if decl == nil {
		return nil
	}

	prevReturn := l.currentReturnType
	l.currentReturnType = types.TypeVoid
	if decl.Type != nil && decl.Type.Return != nil {
		l.currentReturnType = decl.Type.Return
	}

	body := l.lowerBlock(decl.Body)
	l.currentReturnType = prevReturn

	return &hir.MethodDecl{
		Receiver: decl.Receiver,
		Name:     decl.Name,
		Type:     decl.Type,
		Body:     body,
		Location: decl.Location,
	}
}

func (l *Lowerer) lowerVarDecl(decl *hir.VarDecl) *hir.VarDecl {
	if decl == nil {
		return nil
	}

	lowered := &hir.VarDecl{Location: decl.Location}
	if len(decl.Decls) > 0 {
		lowered.Decls = make([]hir.DeclItem, 0, len(decl.Decls))
		for _, item := range decl.Decls {
			lowered.Decls = append(lowered.Decls, l.lowerDeclItem(item))
		}
	}

	return lowered
}

func (l *Lowerer) lowerConstDecl(decl *hir.ConstDecl) *hir.ConstDecl {
	if decl == nil {
		return nil
	}

	lowered := &hir.ConstDecl{Location: decl.Location}
	if len(decl.Decls) > 0 {
		lowered.Decls = make([]hir.DeclItem, 0, len(decl.Decls))
		for _, item := range decl.Decls {
			lowered.Decls = append(lowered.Decls, l.lowerDeclItem(item))
		}
	}

	return lowered
}

func (l *Lowerer) lowerDeclItem(item hir.DeclItem) hir.DeclItem {
	expected := item.Type
	if expected == nil || expected.Equals(types.TypeUnknown) {
		if item.Name != nil {
			expected = item.Name.Type
		}
	}

	return hir.DeclItem{
		Name:  item.Name,
		Type:  item.Type,
		Value: l.lowerExpr(item.Value, expected),
	}
}

func (l *Lowerer) lowerAssignStmt(stmt *hir.AssignStmt) *hir.AssignStmt {
	if stmt == nil {
		return nil
	}

	lhs := l.lowerExpr(stmt.Lhs, types.TypeUnknown)
	expected := types.TypeUnknown
	if stmt.Op == nil || stmt.Op.Kind == tokens.EQUALS_TOKEN {
		expected = l.exprType(lhs)
	}

	return &hir.AssignStmt{
		Lhs:      lhs,
		Rhs:      l.lowerExpr(stmt.Rhs, expected),
		Op:       stmt.Op,
		Location: stmt.Location,
	}
}

func (l *Lowerer) lowerReturnStmt(stmt *hir.ReturnStmt) *hir.ReturnStmt {
	if stmt == nil {
		return nil
	}

	expected := l.currentReturnType
	if expected == nil {
		expected = types.TypeVoid
	}

	if resultType, ok := types.UnwrapType(expected).(*types.ResultType); ok {
		if stmt.IsError {
			expected = resultType.Err
		} else {
			expected = resultType.Ok
		}
	}

	return &hir.ReturnStmt{
		Result:   l.lowerExpr(stmt.Result, expected),
		IsError:  stmt.IsError,
		Location: stmt.Location,
	}
}

func (l *Lowerer) lowerIfStmt(stmt *hir.IfStmt) *hir.IfStmt {
	if stmt == nil {
		return nil
	}

	return &hir.IfStmt{
		Cond:     l.lowerExpr(stmt.Cond, types.TypeBool),
		Body:     l.lowerBlock(stmt.Body),
		Else:     l.lowerNode(stmt.Else),
		Location: stmt.Location,
	}
}

func (l *Lowerer) lowerForStmt(stmt *hir.ForStmt) *hir.ForStmt {
	if stmt == nil {
		return nil
	}

	return &hir.ForStmt{
		Iterator: l.lowerNode(stmt.Iterator),
		Range:    l.lowerExpr(stmt.Range, types.TypeUnknown),
		Body:     l.lowerBlock(stmt.Body),
		Location: stmt.Location,
	}
}

func (l *Lowerer) lowerWhileStmt(stmt *hir.WhileStmt) *hir.WhileStmt {
	if stmt == nil {
		return nil
	}

	return &hir.WhileStmt{
		Cond:     l.lowerExpr(stmt.Cond, types.TypeBool),
		Body:     l.lowerBlock(stmt.Body),
		Location: stmt.Location,
	}
}

func (l *Lowerer) lowerMatchStmt(stmt *hir.MatchStmt) *hir.MatchStmt {
	if stmt == nil {
		return nil
	}

	lowered := &hir.MatchStmt{
		Expr:     l.lowerExpr(stmt.Expr, types.TypeUnknown),
		Location: stmt.Location,
	}

	if len(stmt.Cases) > 0 {
		lowered.Cases = make([]hir.CaseClause, 0, len(stmt.Cases))
		for _, clause := range stmt.Cases {
			lowered.Cases = append(lowered.Cases, hir.CaseClause{
				Pattern:  l.lowerExpr(clause.Pattern, types.TypeUnknown),
				Body:     l.lowerBlock(clause.Body),
				Location: clause.Location,
			})
		}
	}

	return lowered
}

func (l *Lowerer) lowerDeferStmt(stmt *hir.DeferStmt) *hir.DeferStmt {
	if stmt == nil {
		return nil
	}

	return &hir.DeferStmt{
		Call:     l.lowerExpr(stmt.Call, types.TypeUnknown),
		Location: stmt.Location,
	}
}

func (l *Lowerer) lowerExpr(expr hir.Expr, expected types.SemType) hir.Expr {
	if expr == nil {
		return nil
	}

	var lowered hir.Expr
	switch e := expr.(type) {
	case *hir.Ident:
		lowered = e
	case *hir.Literal:
		lowered = e
	case *hir.OptionalNone:
		lowered = e
	case *hir.OptionalSome:
		innerExpected := l.optionalInnerType(e.Type)
		lowered = &hir.OptionalSome{
			Value:    l.lowerExpr(e.Value, innerExpected),
			Type:     e.Type,
			Location: e.Location,
		}
	case *hir.OptionalIsSome:
		lowered = &hir.OptionalIsSome{
			Value:    l.lowerExpr(e.Value, types.TypeUnknown),
			Type:     e.Type,
			Location: e.Location,
		}
	case *hir.OptionalIsNone:
		lowered = &hir.OptionalIsNone{
			Value:    l.lowerExpr(e.Value, types.TypeUnknown),
			Type:     e.Type,
			Location: e.Location,
		}
	case *hir.OptionalUnwrap:
		lowered = &hir.OptionalUnwrap{
			Value:    l.lowerExpr(e.Value, types.TypeUnknown),
			Default:  l.lowerExpr(e.Default, e.Type),
			Type:     e.Type,
			Location: e.Location,
		}
	case *hir.BinaryExpr:
		lowered = l.lowerBinaryExpr(e)
	case *hir.UnaryExpr:
		lowered = &hir.UnaryExpr{Op: e.Op, X: l.lowerExpr(e.X, types.TypeUnknown), Type: e.Type, Location: e.Location}
	case *hir.PrefixExpr:
		lowered = &hir.PrefixExpr{Op: e.Op, X: l.lowerExpr(e.X, types.TypeUnknown), Type: e.Type, Location: e.Location}
	case *hir.PostfixExpr:
		lowered = &hir.PostfixExpr{Op: e.Op, X: l.lowerExpr(e.X, types.TypeUnknown), Type: e.Type, Location: e.Location}
	case *hir.CallExpr:
		lowered = l.lowerCallExpr(e)
	case *hir.SelectorExpr:
		lowered = &hir.SelectorExpr{X: l.lowerExpr(e.X, types.TypeUnknown), Field: e.Field, Type: e.Type, Location: e.Location}
	case *hir.ScopeResolutionExpr:
		lowered = &hir.ScopeResolutionExpr{X: l.lowerExpr(e.X, types.TypeUnknown), Selector: e.Selector, Type: e.Type, Location: e.Location}
	case *hir.RangeExpr:
		lowered = &hir.RangeExpr{
			Start:     l.lowerExpr(e.Start, types.TypeUnknown),
			End:       l.lowerExpr(e.End, types.TypeUnknown),
			Incr:      l.lowerExpr(e.Incr, types.TypeUnknown),
			Inclusive: e.Inclusive,
			Type:      e.Type,
			Location:  e.Location,
		}
	case *hir.IndexExpr:
		lowered = &hir.IndexExpr{X: l.lowerExpr(e.X, types.TypeUnknown), Index: l.lowerExpr(e.Index, types.TypeUnknown), Type: e.Type, Location: e.Location}
	case *hir.CastExpr:
		lowered = &hir.CastExpr{X: l.lowerExpr(e.X, types.TypeUnknown), TargetType: e.TargetType, Type: e.Type, Location: e.Location}
	case *hir.CoalescingExpr:
		lowered = l.lowerCoalescingExpr(e)
	case *hir.ForkExpr:
		lowered = &hir.ForkExpr{Call: l.lowerExpr(e.Call, types.TypeUnknown), Type: e.Type, Location: e.Location}
	case *hir.ParenExpr:
		lowered = &hir.ParenExpr{X: l.lowerExpr(e.X, expected), Type: e.Type, Location: e.Location}
	case *hir.CompositeLit:
		lowered = l.lowerCompositeLit(e)
	case *hir.KeyValueExpr:
		lowered = &hir.KeyValueExpr{
			Key:      l.lowerExpr(e.Key, types.TypeUnknown),
			Value:    l.lowerExpr(e.Value, types.TypeUnknown),
			Location: e.Location,
		}
	case *hir.FuncLit:
		lowered = l.lowerFuncLit(e)
	case *hir.Invalid:
		lowered = e
	default:
		lowered = e
	}

	return l.wrapOptional(lowered, expected)
}

func (l *Lowerer) lowerBinaryExpr(expr *hir.BinaryExpr) hir.Expr {
	left := l.lowerExpr(expr.X, types.TypeUnknown)
	right := l.lowerExpr(expr.Y, types.TypeUnknown)

	if expr.Op.Kind == tokens.DOUBLE_EQUAL_TOKEN || expr.Op.Kind == tokens.NOT_EQUAL_TOKEN {
		leftNone := isNoneExpr(left)
		rightNone := isNoneExpr(right)
		if leftNone != rightNone {
			optionalExpr := left
			if leftNone {
				optionalExpr = right
			}
			if isOptionalType(l.exprType(optionalExpr)) {
				if expr.Op.Kind == tokens.DOUBLE_EQUAL_TOKEN {
					return &hir.OptionalIsNone{
						Value:    optionalExpr,
						Type:     expr.Type,
						Location: expr.Location,
					}
				}
				return &hir.OptionalIsSome{
					Value:    optionalExpr,
					Type:     expr.Type,
					Location: expr.Location,
				}
			}
		}
	}

	return &hir.BinaryExpr{X: left, Op: expr.Op, Y: right, Type: expr.Type, Location: expr.Location}
}

func (l *Lowerer) lowerCoalescingExpr(expr *hir.CoalescingExpr) hir.Expr {
	cond := l.lowerExpr(expr.Cond, types.TypeUnknown)
	def := l.lowerExpr(expr.Default, expr.Type)

	if isOptionalType(l.exprType(expr.Cond)) {
		return &hir.OptionalUnwrap{
			Value:    cond,
			Default:  def,
			Type:     expr.Type,
			Location: expr.Location,
		}
	}

	return &hir.CoalescingExpr{
		Cond:     cond,
		Default:  def,
		Type:     expr.Type,
		Location: expr.Location,
	}
}

func (l *Lowerer) lowerCallExpr(expr *hir.CallExpr) *hir.CallExpr {
	if expr == nil {
		return nil
	}

	funType := l.getFunctionType(expr.Fun)
	args := make([]hir.Expr, 0, len(expr.Args))
	for i, arg := range expr.Args {
		expected := l.expectedArgType(funType, i)
		args = append(args, l.lowerExpr(arg, expected))
	}

	return &hir.CallExpr{
		Fun:      l.lowerExpr(expr.Fun, types.TypeUnknown),
		Args:     args,
		Catch:    l.lowerCatchClause(expr.Catch),
		Type:     expr.Type,
		Location: expr.Location,
	}
}

func (l *Lowerer) lowerCatchClause(clause *hir.CatchClause) *hir.CatchClause {
	if clause == nil {
		return nil
	}

	return &hir.CatchClause{
		ErrIdent: clause.ErrIdent,
		Handler:  l.lowerBlock(clause.Handler),
		Fallback: l.lowerExpr(clause.Fallback, types.TypeUnknown),
		Location: clause.Location,
	}
}

func (l *Lowerer) lowerCompositeLit(lit *hir.CompositeLit) *hir.CompositeLit {
	if lit == nil {
		return nil
	}

	lowered := &hir.CompositeLit{Type: lit.Type, Location: lit.Location}
	if len(lit.Elts) == 0 {
		return lowered
	}

	unwrapped := types.UnwrapType(lit.Type)
	switch t := unwrapped.(type) {
	case *types.StructType:
		fieldTypes := make(map[string]types.SemType, len(t.Fields))
		for _, field := range t.Fields {
			fieldTypes[field.Name] = field.Type
		}

		lowered.Elts = make([]hir.Expr, 0, len(lit.Elts))
		for _, elt := range lit.Elts {
			if kv, ok := elt.(*hir.KeyValueExpr); ok {
				fieldType := fieldTypes[fieldNameFromKey(kv.Key)]
				lowered.Elts = append(lowered.Elts, l.lowerKeyValueExpr(kv, types.TypeUnknown, fieldType))
				continue
			}
			lowered.Elts = append(lowered.Elts, l.lowerExpr(elt, types.TypeUnknown))
		}
	case *types.MapType:
		lowered.Elts = make([]hir.Expr, 0, len(lit.Elts))
		for _, elt := range lit.Elts {
			if kv, ok := elt.(*hir.KeyValueExpr); ok {
				lowered.Elts = append(lowered.Elts, l.lowerKeyValueExpr(kv, t.Key, t.Value))
				continue
			}
			lowered.Elts = append(lowered.Elts, l.lowerExpr(elt, types.TypeUnknown))
		}
	case *types.ArrayType:
		lowered.Elts = make([]hir.Expr, 0, len(lit.Elts))
		for _, elt := range lit.Elts {
			lowered.Elts = append(lowered.Elts, l.lowerExpr(elt, t.Element))
		}
	default:
		lowered.Elts = make([]hir.Expr, 0, len(lit.Elts))
		for _, elt := range lit.Elts {
			lowered.Elts = append(lowered.Elts, l.lowerExpr(elt, types.TypeUnknown))
		}
	}

	return lowered
}

func (l *Lowerer) lowerKeyValueExpr(kv *hir.KeyValueExpr, keyExpected, valueExpected types.SemType) *hir.KeyValueExpr {
	if kv == nil {
		return nil
	}

	return &hir.KeyValueExpr{
		Key:      l.lowerExpr(kv.Key, keyExpected),
		Value:    l.lowerExpr(kv.Value, valueExpected),
		Location: kv.Location,
	}
}

func (l *Lowerer) lowerFuncLit(lit *hir.FuncLit) *hir.FuncLit {
	if lit == nil {
		return nil
	}

	prevReturn := l.currentReturnType
	l.currentReturnType = types.TypeVoid
	if fnType, ok := types.UnwrapType(lit.Type).(*types.FunctionType); ok {
		l.currentReturnType = fnType.Return
	}

	body := l.lowerBlock(lit.Body)
	l.currentReturnType = prevReturn

	return &hir.FuncLit{
		ID:       lit.ID,
		Type:     lit.Type,
		Body:     body,
		Location: lit.Location,
	}
}

func (l *Lowerer) wrapOptional(expr hir.Expr, expected types.SemType) hir.Expr {
	if expr == nil {
		return nil
	}

	expectedOpt := l.optionalType(expected)
	if expectedOpt == nil {
		return expr
	}

	if isNoneExpr(expr) {
		return &hir.OptionalNone{Type: expected, Location: locationFromExpr(expr)}
	}

	if l.isOptionalProducer(expr) {
		return expr
	}

	actualType := l.exprType(expr)
	if actualType == nil || actualType.Equals(types.TypeUnknown) {
		return expr
	}

	if expectedOpt.Inner == nil {
		return expr
	}

	if isOptionalType(actualType) || actualType.Equals(expectedOpt.Inner) {
		value := l.forceExprType(expr, expectedOpt.Inner)
		return &hir.OptionalSome{Value: value, Type: expected, Location: locationFromExpr(expr)}
	}

	return expr
}

func (l *Lowerer) isOptionalProducer(expr hir.Expr) bool {
	if expr == nil {
		return false
	}

	switch e := expr.(type) {
	case *hir.OptionalSome, *hir.OptionalNone:
		return true
	case *hir.Ident:
		if e.Symbol != nil && e.Symbol.Type != nil {
			return isOptionalType(e.Symbol.Type)
		}
		return false
	case *hir.CallExpr:
		if fn := l.getFunctionType(e.Fun); fn != nil {
			return isOptionalType(fn.Return)
		}
		return false
	case *hir.IndexExpr:
		baseType := types.UnwrapType(l.exprType(e.X))
		_, ok := baseType.(*types.MapType)
		return ok
	case *hir.SelectorExpr:
		return isOptionalType(l.selectorFieldType(e))
	case *hir.ScopeResolutionExpr:
		return isOptionalType(l.exprType(e))
	case *hir.CastExpr:
		return isOptionalType(e.TargetType)
	case *hir.ParenExpr:
		return l.isOptionalProducer(e.X)
	default:
		return false
	}
}

func (l *Lowerer) forceExprType(expr hir.Expr, target types.SemType) hir.Expr {
	if expr == nil || target == nil {
		return expr
	}

	switch e := expr.(type) {
	case *hir.Literal:
		return &hir.Literal{Kind: e.Kind, Value: e.Value, Type: target, Location: e.Location}
	case *hir.BinaryExpr:
		return &hir.BinaryExpr{X: e.X, Op: e.Op, Y: e.Y, Type: target, Location: e.Location}
	case *hir.UnaryExpr:
		return &hir.UnaryExpr{Op: e.Op, X: e.X, Type: target, Location: e.Location}
	case *hir.PrefixExpr:
		return &hir.PrefixExpr{Op: e.Op, X: e.X, Type: target, Location: e.Location}
	case *hir.PostfixExpr:
		return &hir.PostfixExpr{Op: e.Op, X: e.X, Type: target, Location: e.Location}
	case *hir.CastExpr:
		return &hir.CastExpr{X: e.X, TargetType: e.TargetType, Type: target, Location: e.Location}
	case *hir.CompositeLit:
		return &hir.CompositeLit{Type: target, Elts: e.Elts, Location: e.Location}
	case *hir.ParenExpr:
		return &hir.ParenExpr{X: e.X, Type: target, Location: e.Location}
	case *hir.CoalescingExpr:
		return &hir.CoalescingExpr{Cond: e.Cond, Default: e.Default, Type: target, Location: e.Location}
	default:
		return expr
	}
}

func (l *Lowerer) selectorFieldType(expr *hir.SelectorExpr) types.SemType {
	if expr == nil || expr.Field == nil {
		return types.TypeUnknown
	}

	baseType := l.exprType(expr.X)
	baseType = dereferenceType(baseType)
	unwrapped := types.UnwrapType(baseType)

	if structType, ok := unwrapped.(*types.StructType); ok {
		for _, field := range structType.Fields {
			if field.Name == expr.Field.Name {
				return field.Type
			}
		}
	}

	return types.TypeUnknown
}

func (l *Lowerer) optionalType(t types.SemType) *types.OptionalType {
	if t == nil {
		return nil
	}

	if opt, ok := t.(*types.OptionalType); ok {
		return opt
	}

	if opt, ok := types.UnwrapType(t).(*types.OptionalType); ok {
		return opt
	}

	return nil
}

func (l *Lowerer) optionalInnerType(t types.SemType) types.SemType {
	opt := l.optionalType(t)
	if opt == nil {
		return types.TypeUnknown
	}
	return opt.Inner
}

func (l *Lowerer) exprType(expr hir.Expr) types.SemType {
	if expr == nil {
		return types.TypeUnknown
	}

	switch e := expr.(type) {
	case *hir.Ident:
		return safeType(e.Type)
	case *hir.Literal:
		return safeType(e.Type)
	case *hir.OptionalNone:
		return safeType(e.Type)
	case *hir.OptionalSome:
		return safeType(e.Type)
	case *hir.OptionalIsSome:
		return safeType(e.Type)
	case *hir.OptionalIsNone:
		return safeType(e.Type)
	case *hir.OptionalUnwrap:
		return safeType(e.Type)
	case *hir.BinaryExpr:
		return safeType(e.Type)
	case *hir.UnaryExpr:
		return safeType(e.Type)
	case *hir.PrefixExpr:
		return safeType(e.Type)
	case *hir.PostfixExpr:
		return safeType(e.Type)
	case *hir.CallExpr:
		return safeType(e.Type)
	case *hir.SelectorExpr:
		return safeType(e.Type)
	case *hir.RangeExpr:
		return safeType(e.Type)
	case *hir.IndexExpr:
		return safeType(e.Type)
	case *hir.CastExpr:
		return safeType(e.Type)
	case *hir.CoalescingExpr:
		return safeType(e.Type)
	case *hir.ForkExpr:
		return safeType(e.Type)
	case *hir.ScopeResolutionExpr:
		return safeType(e.Type)
	case *hir.ParenExpr:
		return safeType(e.Type)
	case *hir.CompositeLit:
		return safeType(e.Type)
	case *hir.FuncLit:
		return safeType(e.Type)
	case *hir.KeyValueExpr:
		return types.TypeUnknown
	default:
		return types.TypeUnknown
	}
}

func (l *Lowerer) getFunctionType(expr hir.Expr) *types.FunctionType {
	if expr == nil {
		return nil
	}

	t := l.exprType(expr)
	if t == nil {
		return nil
	}

	if fn, ok := types.UnwrapType(t).(*types.FunctionType); ok {
		return fn
	}

	return nil
}

func (l *Lowerer) expectedArgType(fn *types.FunctionType, index int) types.SemType {
	if fn == nil || len(fn.Params) == 0 || index < 0 {
		return types.TypeUnknown
	}

	last := len(fn.Params) - 1
	isVariadic := fn.Params[last].IsVariadic

	if !isVariadic {
		if index >= len(fn.Params) {
			return types.TypeUnknown
		}
		return fn.Params[index].Type
	}

	if index < last {
		return fn.Params[index].Type
	}
	return fn.Params[last].Type
}

func isNoneExpr(expr hir.Expr) bool {
	if expr == nil {
		return false
	}

	switch e := expr.(type) {
	case *hir.OptionalNone:
		return true
	case *hir.Literal:
		return e.Kind == hir.LiteralNone
	case *hir.Ident:
		if e.Name == "none" {
			return true
		}
		if e.Symbol != nil && e.Symbol.Type != nil {
			return e.Symbol.Type.Equals(types.TypeNone)
		}
	}

	return false
}

func isOptionalType(t types.SemType) bool {
	if t == nil {
		return false
	}
	if _, ok := t.(*types.OptionalType); ok {
		return true
	}
	if _, ok := types.UnwrapType(t).(*types.OptionalType); ok {
		return true
	}
	return false
}

func dereferenceType(t types.SemType) types.SemType {
	if ref, ok := t.(*types.ReferenceType); ok {
		return ref.Inner
	}
	return t
}

func fieldNameFromKey(expr hir.Expr) string {
	switch e := expr.(type) {
	case *hir.Ident:
		return e.Name
	case *hir.SelectorExpr:
		if e.Field != nil {
			return e.Field.Name
		}
	}
	return ""
}

func locationFromExpr(expr hir.Expr) source.Location {
	if expr == nil || expr.Loc() == nil {
		return source.Location{}
	}
	return *expr.Loc()
}

func safeType(t types.SemType) types.SemType {
	if t == nil {
		return types.TypeUnknown
	}
	return t
}
