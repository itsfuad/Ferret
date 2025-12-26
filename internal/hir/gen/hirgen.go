package gen

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/hir"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/semantics/typechecker"
	"compiler/internal/source"
	"compiler/internal/types"
)

// Generator lowers typed AST into HIR.
type Generator struct {
	ctx *context_v2.CompilerContext
	mod *context_v2.Module
}

// New creates a new HIR generator for a module.
func New(ctx *context_v2.CompilerContext, mod *context_v2.Module) *Generator {
	return &Generator{ctx: ctx, mod: mod}
}

// GenerateModule generates HIR for the module AST.
func (g *Generator) GenerateModule() *hir.Module {
	if g == nil || g.mod == nil || g.mod.AST == nil {
		return nil
	}

	prevScope := g.mod.CurrentScope
	if g.mod.ModuleScope != nil {
		g.mod.CurrentScope = g.mod.ModuleScope
	}
	defer func() {
		g.mod.CurrentScope = prevScope
	}()

	items := make([]hir.Node, 0, len(g.mod.AST.Nodes))
	for _, node := range g.mod.AST.Nodes {
		if lowered := g.lowerNode(node); lowered != nil {
			items = append(items, lowered)
		}
	}

	return &hir.Module{
		ImportPath: g.mod.ImportPath,
		Items:      items,
		Location:   locFromNode(g.mod.AST),
	}
}

func (g *Generator) lowerNode(node ast.Node) hir.Node {
	if node == nil {
		return nil
	}

	switch n := node.(type) {
	case *ast.VarDecl:
		return g.lowerVarDecl(n)
	case *ast.ConstDecl:
		return g.lowerConstDecl(n)
	case *ast.TypeDecl:
		return g.lowerTypeDecl(n)
	case *ast.FuncDecl:
		return g.lowerFuncDecl(n)
	case *ast.MethodDecl:
		return g.lowerMethodDecl(n)
	case *ast.DeclStmt:
		return g.lowerDeclStmt(n)
	case *ast.AssignStmt:
		return g.lowerAssignStmt(n)
	case *ast.ReturnStmt:
		return g.lowerReturnStmt(n)
	case *ast.ImportStmt:
		return g.lowerImportStmt(n)
	case *ast.BreakStmt:
		return &hir.BreakStmt{Location: locFromNode(n)}
	case *ast.ContinueStmt:
		return &hir.ContinueStmt{Location: locFromNode(n)}
	case *ast.ExprStmt:
		return g.lowerExprStmt(n)
	case *ast.Block:
		return g.lowerBlock(n)
	case *ast.IfStmt:
		return g.lowerIfStmt(n)
	case *ast.ForStmt:
		return g.lowerForStmt(n)
	case *ast.WhileStmt:
		return g.lowerWhileStmt(n)
	case *ast.MatchStmt:
		return g.lowerMatchStmt(n)
	case *ast.DeferStmt:
		return g.lowerDeferStmt(n)
	case *ast.Invalid:
		return &hir.Invalid{Location: locFromNode(n)}
	default:
		g.reportUnsupported("node", node)
		return &hir.Invalid{Location: locFromNode(node)}
	}
}

func (g *Generator) lowerExpr(expr ast.Expression) hir.Expr {
	if expr == nil {
		return nil
	}

	switch e := expr.(type) {
	case *ast.BasicLit:
		return &hir.Literal{
			Kind:     lowerLiteralKind(e.Kind),
			Value:    e.Value,
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.IdentifierExpr:
		return g.lowerIdentExpr(e)
	case *ast.BinaryExpr:
		return &hir.BinaryExpr{
			X:        g.lowerExpr(e.X),
			Op:       e.Op,
			Y:        g.lowerExpr(e.Y),
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.UnaryExpr:
		return &hir.UnaryExpr{
			Op:       e.Op,
			X:        g.lowerExpr(e.X),
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.PrefixExpr:
		return &hir.PrefixExpr{
			Op:       e.Op,
			X:        g.lowerExpr(e.X),
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.PostfixExpr:
		return &hir.PostfixExpr{
			X:        g.lowerExpr(e.X),
			Op:       e.Op,
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.CallExpr:
		return g.lowerCallExpr(e)
	case *ast.SelectorExpr:
		return g.lowerSelectorExpr(e)
	case *ast.ScopeResolutionExpr:
		return g.lowerScopeResolutionExpr(e)
	case *ast.RangeExpr:
		return &hir.RangeExpr{
			Start:     g.lowerExpr(e.Start),
			End:       g.lowerExpr(e.End),
			Incr:      g.lowerExpr(e.Incr),
			Inclusive: e.Inclusive,
			Type:      g.exprType(expr),
			Location:  locFromNode(e),
		}
	case *ast.IndexExpr:
		return &hir.IndexExpr{
			X:        g.lowerExpr(e.X),
			Index:    g.lowerExpr(e.Index),
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.CastExpr:
		return g.lowerCastExpr(e)
	case *ast.CoalescingExpr:
		return &hir.CoalescingExpr{
			Cond:     g.lowerExpr(e.Cond),
			Default:  g.lowerExpr(e.Default),
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.ForkExpr:
		return &hir.ForkExpr{
			Call:     g.lowerExpr(e.Call),
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.ParenExpr:
		return &hir.ParenExpr{
			X:        g.lowerExpr(e.X),
			Type:     g.exprType(expr),
			Location: locFromNode(e),
		}
	case *ast.CompositeLit:
		return g.lowerCompositeLit(e)
	case *ast.KeyValueExpr:
		return &hir.KeyValueExpr{
			Key:      g.lowerExpr(e.Key),
			Value:    g.lowerExpr(e.Value),
			Location: locFromNode(e),
		}
	case *ast.FuncLit:
		return g.lowerFuncLit(e)
	case *ast.Invalid:
		return &hir.Invalid{Location: locFromNode(e)}
	default:
		g.reportUnsupported("expression", expr)
		return &hir.Invalid{Location: locFromNode(expr)}
	}
}

func (g *Generator) lowerDeclStmt(stmt *ast.DeclStmt) *hir.DeclStmt {
	if stmt == nil {
		return nil
	}
	return &hir.DeclStmt{
		Decl:     g.lowerDecl(stmt.Decl),
		Location: locFromNode(stmt),
	}
}

func (g *Generator) lowerDecl(decl ast.Decl) hir.Decl {
	if decl == nil {
		return nil
	}
	switch d := decl.(type) {
	case *ast.VarDecl:
		return g.lowerVarDecl(d)
	case *ast.ConstDecl:
		return g.lowerConstDecl(d)
	case *ast.TypeDecl:
		return g.lowerTypeDecl(d)
	case *ast.FuncDecl:
		return g.lowerFuncDecl(d)
	case *ast.MethodDecl:
		return g.lowerMethodDecl(d)
	default:
		g.reportUnsupported("declaration", decl)
		return &hir.Invalid{Location: locFromNode(decl)}
	}
}

func (g *Generator) lowerVarDecl(decl *ast.VarDecl) *hir.VarDecl {
	if decl == nil {
		return nil
	}
	items := make([]hir.DeclItem, 0, len(decl.Decls))
	for _, item := range decl.Decls {
		items = append(items, g.lowerDeclItem(item))
	}
	return &hir.VarDecl{Decls: items, Location: locFromNode(decl)}
}

func (g *Generator) lowerConstDecl(decl *ast.ConstDecl) *hir.ConstDecl {
	if decl == nil {
		return nil
	}
	items := make([]hir.DeclItem, 0, len(decl.Decls))
	for _, item := range decl.Decls {
		items = append(items, g.lowerDeclItem(item))
	}
	return &hir.ConstDecl{Decls: items, Location: locFromNode(decl)}
}

func (g *Generator) lowerDeclItem(item ast.DeclItem) hir.DeclItem {
	var ident *hir.Ident
	var name string
	if item.Name != nil {
		name = item.Name.Name
	}
	declType := g.resolveDeclType(name, item.Type, item.Value)
	if item.Name != nil {
		ident = g.identForDecl(item.Name, declType)
	} else {
		ident = &hir.Ident{
			Name:     "",
			Type:     declType,
			Location: source.Location{},
		}
	}

	return hir.DeclItem{
		Name:  ident,
		Type:  declType,
		Value: g.lowerExpr(item.Value),
	}
}

func (g *Generator) lowerTypeDecl(decl *ast.TypeDecl) *hir.TypeDecl {
	if decl == nil {
		return nil
	}
	var ident *hir.Ident
	if decl.Name != nil {
		ident = g.identForDecl(decl.Name, g.resolveDeclType(decl.Name.Name, decl.Type, nil))
	}
	return &hir.TypeDecl{
		Name:     ident,
		Type:     g.resolveTypeDecl(decl),
		Location: locFromNode(decl),
	}
}

func (g *Generator) lowerFuncDecl(decl *ast.FuncDecl) *hir.FuncDecl {
	if decl == nil {
		return nil
	}
	var ident *hir.Ident
	if decl.Name != nil {
		ident = g.identForDecl(decl.Name, g.resolveDeclType(decl.Name.Name, nil, nil))
	}

	var body *hir.Block
	g.withScope(decl.Scope, func() {
		body = g.lowerBlock(decl.Body)
	})

	return &hir.FuncDecl{
		Name:     ident,
		Type:     g.resolveFuncType(decl.Name, decl.Type),
		Body:     body,
		Location: locFromNode(decl),
	}
}

func (g *Generator) lowerMethodDecl(decl *ast.MethodDecl) *hir.MethodDecl {
	if decl == nil {
		return nil
	}

	var receiver *hir.Param
	if decl.Receiver != nil {
		receiver = g.lowerParam(decl.Receiver)
	}

	var ident *hir.Ident
	if decl.Name != nil {
		ident = g.identForDecl(decl.Name, g.resolveDeclType(decl.Name.Name, nil, nil))
	}

	var body *hir.Block
	g.withScope(decl.Scope, func() {
		body = g.lowerBlock(decl.Body)
	})

	return &hir.MethodDecl{
		Receiver: receiver,
		Name:     ident,
		Type:     g.resolveFuncType(decl.Name, decl.Type),
		Body:     body,
		Location: locFromNode(decl),
	}
}

func (g *Generator) lowerParam(field *ast.Field) *hir.Param {
	if field == nil {
		return nil
	}

	name := ""
	if field.Name != nil {
		name = field.Name.Name
	}

	return &hir.Param{
		Name:       name,
		Type:       g.typeFromNode(field.Type),
		IsVariadic: field.IsVariadic,
		Location:   field.Location,
	}
}

func (g *Generator) lowerAssignStmt(stmt *ast.AssignStmt) *hir.AssignStmt {
	if stmt == nil {
		return nil
	}
	return &hir.AssignStmt{
		Lhs:      g.lowerExpr(stmt.Lhs),
		Rhs:      g.lowerExpr(stmt.Rhs),
		Op:       stmt.Op,
		Location: locFromNode(stmt),
	}
}

func (g *Generator) lowerReturnStmt(stmt *ast.ReturnStmt) *hir.ReturnStmt {
	if stmt == nil {
		return nil
	}
	return &hir.ReturnStmt{
		Result:   g.lowerExpr(stmt.Result),
		IsError:  stmt.IsError,
		Location: locFromNode(stmt),
	}
}

func (g *Generator) lowerImportStmt(stmt *ast.ImportStmt) *hir.ImportStmt {
	if stmt == nil {
		return nil
	}
	alias := ""
	if stmt.Alias != nil {
		alias = stmt.Alias.Name
	}
	path := ""
	if stmt.Path != nil {
		path = stmt.Path.Value
	}
	return &hir.ImportStmt{
		Path:           path,
		Alias:          alias,
		LocationOnDisk: stmt.LocationOnDisk,
		Location:       locFromNode(stmt),
	}
}

func (g *Generator) lowerExprStmt(stmt *ast.ExprStmt) *hir.ExprStmt {
	if stmt == nil {
		return nil
	}
	return &hir.ExprStmt{
		X:        g.lowerExpr(stmt.X),
		Location: locFromNode(stmt),
	}
}

func (g *Generator) lowerDeferStmt(stmt *ast.DeferStmt) *hir.DeferStmt {
	if stmt == nil {
		return nil
	}
	return &hir.DeferStmt{
		Call:     g.lowerExpr(stmt.Call),
		Location: locFromNode(stmt),
	}
}

func (g *Generator) lowerBlock(block *ast.Block) *hir.Block {
	if block == nil {
		return nil
	}
	hirBlock := &hir.Block{Location: locFromNode(block)}
	g.withScope(block.Scope, func() {
		nodes := make([]hir.Node, 0, len(block.Nodes))
		for _, node := range block.Nodes {
			if lowered := g.lowerNode(node); lowered != nil {
				nodes = append(nodes, lowered)
			}
		}
		hirBlock.Nodes = nodes
	})
	return hirBlock
}

func (g *Generator) lowerIfStmt(stmt *ast.IfStmt) *hir.IfStmt {
	if stmt == nil {
		return nil
	}
	hirStmt := &hir.IfStmt{
		Cond:     g.lowerExpr(stmt.Cond),
		Location: locFromNode(stmt),
	}
	g.withScope(stmt.Scope, func() {
		hirStmt.Body = g.lowerBlock(stmt.Body)
	})
	if stmt.Else != nil {
		hirStmt.Else = g.lowerNode(stmt.Else)
	}
	return hirStmt
}

func (g *Generator) lowerForStmt(stmt *ast.ForStmt) *hir.ForStmt {
	if stmt == nil {
		return nil
	}
	hirStmt := &hir.ForStmt{Location: locFromNode(stmt)}
	g.withScope(stmt.Scope, func() {
		hirStmt.Iterator = g.lowerNode(stmt.Iterator)
		hirStmt.Range = g.lowerExpr(stmt.Range)
		hirStmt.Body = g.lowerBlock(stmt.Body)
	})
	return hirStmt
}

func (g *Generator) lowerWhileStmt(stmt *ast.WhileStmt) *hir.WhileStmt {
	if stmt == nil {
		return nil
	}
	hirStmt := &hir.WhileStmt{Location: locFromNode(stmt)}
	g.withScope(stmt.Scope, func() {
		hirStmt.Cond = g.lowerExpr(stmt.Cond)
		hirStmt.Body = g.lowerBlock(stmt.Body)
	})
	return hirStmt
}

func (g *Generator) lowerMatchStmt(stmt *ast.MatchStmt) *hir.MatchStmt {
	if stmt == nil {
		return nil
	}
	hirStmt := &hir.MatchStmt{
		Expr:     g.lowerExpr(stmt.Expr),
		Location: locFromNode(stmt),
	}
	cases := make([]hir.CaseClause, 0, len(stmt.Cases))
	for _, clause := range stmt.Cases {
		if clause == nil {
			continue
		}
		cases = append(cases, hir.CaseClause{
			Pattern:  g.lowerExpr(clause.Pattern),
			Body:     g.lowerBlock(clause.Body),
			Location: clause.Location,
		})
	}
	hirStmt.Cases = cases
	return hirStmt
}

func (g *Generator) lowerCallExpr(expr *ast.CallExpr) *hir.CallExpr {
	if expr == nil {
		return nil
	}
	args := make([]hir.Expr, 0, len(expr.Args))
	for _, arg := range expr.Args {
		args = append(args, g.lowerExpr(arg))
	}
	return &hir.CallExpr{
		Fun:      g.lowerExpr(expr.Fun),
		Args:     args,
		Catch:    g.lowerCatchClause(expr.Catch),
		Type:     g.exprType(expr),
		Location: locFromNode(expr),
	}
}

func (g *Generator) lowerCatchClause(clause *ast.CatchClause) *hir.CatchClause {
	if clause == nil {
		return nil
	}
	var errIdent *hir.Ident
	if clause.ErrIdent != nil {
		if clause.Handler != nil && clause.Handler.Scope != nil {
			g.withScope(clause.Handler.Scope, func() {
				errIdent = g.identFromExpr(clause.ErrIdent)
			})
		} else {
			errIdent = g.identFromExpr(clause.ErrIdent)
		}
	}
	return &hir.CatchClause{
		ErrIdent: errIdent,
		Handler:  g.lowerBlock(clause.Handler),
		Fallback: g.lowerExpr(clause.Fallback),
		Location: locFromNode(clause),
	}
}

func (g *Generator) lowerSelectorExpr(expr *ast.SelectorExpr) *hir.SelectorExpr {
	if expr == nil {
		return nil
	}
	field := &hir.Ident{
		Name:     "",
		Type:     g.exprType(expr),
		Location: source.Location{},
	}
	if expr.Field != nil {
		field.Name = expr.Field.Name
		field.Location = locFromNode(expr.Field)
	}
	return &hir.SelectorExpr{
		X:        g.lowerExpr(expr.X),
		Field:    field,
		Type:     g.exprType(expr),
		Location: locFromNode(expr),
	}
}

func (g *Generator) lowerScopeResolutionExpr(expr *ast.ScopeResolutionExpr) *hir.ScopeResolutionExpr {
	if expr == nil {
		return nil
	}
	selector := &hir.Ident{
		Name:     "",
		Type:     g.exprType(expr),
		Location: source.Location{},
	}
	if expr.Selector != nil {
		selector.Name = expr.Selector.Name
		selector.Location = locFromNode(expr.Selector)
	}
	return &hir.ScopeResolutionExpr{
		X:        g.lowerExpr(expr.X),
		Selector: selector,
		Type:     g.exprType(expr),
		Location: locFromNode(expr),
	}
}

func (g *Generator) lowerCastExpr(expr *ast.CastExpr) *hir.CastExpr {
	if expr == nil {
		return nil
	}
	targetType := g.typeFromNode(expr.Type)
	if targetType == nil || targetType.Equals(types.TypeUnknown) {
		targetType = g.exprType(expr)
	}
	return &hir.CastExpr{
		X:          g.lowerExpr(expr.X),
		TargetType: targetType,
		Type:       g.exprType(expr),
		Location:   locFromNode(expr),
	}
}

func (g *Generator) lowerCompositeLit(expr *ast.CompositeLit) *hir.CompositeLit {
	if expr == nil {
		return nil
	}
	elts := make([]hir.Expr, 0, len(expr.Elts))
	for _, elt := range expr.Elts {
		elts = append(elts, g.lowerExpr(elt))
	}
	return &hir.CompositeLit{
		Type:     g.exprType(expr),
		Elts:     elts,
		Location: locFromNode(expr),
	}
}

func (g *Generator) lowerFuncLit(expr *ast.FuncLit) *hir.FuncLit {
	if expr == nil {
		return nil
	}
	hirLit := &hir.FuncLit{
		ID:       expr.ID.Name,
		Type:     g.exprType(expr),
		Location: locFromNode(expr),
	}
	g.withScope(expr.Scope, func() {
		hirLit.Body = g.lowerBlock(expr.Body)
	})
	return hirLit
}

func (g *Generator) lowerIdentExpr(expr *ast.IdentifierExpr) *hir.Ident {
	if expr == nil {
		return nil
	}
	return g.identFromExpr(expr)
}

func (g *Generator) identFromExpr(expr *ast.IdentifierExpr) *hir.Ident {
	if expr == nil {
		return nil
	}
	return &hir.Ident{
		Name:     expr.Name,
		Symbol:   g.lookupSymbol(expr.Name),
		Type:     g.exprType(expr),
		Location: locFromNode(expr),
	}
}

func (g *Generator) identForDecl(expr *ast.IdentifierExpr, declType types.SemType) *hir.Ident {
	if expr == nil {
		return nil
	}
	return &hir.Ident{
		Name:     expr.Name,
		Symbol:   g.lookupSymbol(expr.Name),
		Type:     declType,
		Location: locFromNode(expr),
	}
}

func (g *Generator) resolveDeclType(name string, explicitType ast.TypeNode, value ast.Expression) types.SemType {
	if name != "" && name != "_" {
		if sym := g.lookupSymbol(name); sym != nil && sym.Type != nil {
			return sym.Type
		}
	}
	if explicitType != nil {
		return g.typeFromNode(explicitType)
	}
	if value != nil {
		return g.exprType(value)
	}
	return types.TypeUnknown
}

func (g *Generator) resolveTypeDecl(decl *ast.TypeDecl) types.SemType {
	if decl == nil || decl.Name == nil {
		return types.TypeUnknown
	}
	if sym := g.lookupSymbol(decl.Name.Name); sym != nil && sym.Type != nil {
		return sym.Type
	}
	if decl.Type != nil {
		return g.typeFromNode(decl.Type)
	}
	return types.TypeUnknown
}

func (g *Generator) resolveFuncType(name *ast.IdentifierExpr, typeNode *ast.FuncType) *types.FunctionType {
	if name != nil {
		if sym := g.lookupSymbol(name.Name); sym != nil {
			if ft, ok := sym.Type.(*types.FunctionType); ok {
				return ft
			}
			if named, ok := sym.Type.(*types.NamedType); ok {
				if ft, ok := named.Underlying.(*types.FunctionType); ok {
					return ft
				}
			}
		}
	}
	if typeNode == nil {
		return nil
	}
	if sem := g.typeFromNode(typeNode); sem != nil {
		if ft, ok := sem.(*types.FunctionType); ok {
			return ft
		}
	}
	return nil
}

func (g *Generator) exprType(expr ast.Expression) types.SemType {
	if g == nil || g.ctx == nil || g.mod == nil || expr == nil {
		return types.TypeUnknown
	}
	return typechecker.ResolvedExprType(g.ctx, g.mod, expr)
}

func (g *Generator) typeFromNode(node ast.TypeNode) types.SemType {
	if g == nil || g.ctx == nil || g.mod == nil || node == nil {
		return types.TypeUnknown
	}
	return typechecker.TypeFromTypeNodeWithContext(g.ctx, g.mod, node)
}

func (g *Generator) lookupSymbol(name string) *symbols.Symbol {
	if g == nil || g.mod == nil || name == "" {
		return nil
	}
	scope := g.mod.CurrentScope
	if scope == nil {
		scope = g.mod.ModuleScope
	}
	if scope == nil {
		return nil
	}
	if sym, ok := scope.Lookup(name); ok {
		return sym
	}
	return nil
}

func (g *Generator) withScope(scope ast.SymbolTable, fn func()) {
	if fn == nil {
		return
	}
	if g == nil || g.mod == nil {
		fn()
		return
	}
	if scope == nil {
		fn()
		return
	}
	if tableScope, ok := scope.(*table.SymbolTable); ok {
		defer g.mod.EnterScope(tableScope)()
	}
	fn()
}

func (g *Generator) reportUnsupported(kind string, node ast.Node) {
	if g == nil || g.ctx == nil || node == nil {
		return
	}
	g.ctx.ReportError("hir/gen: unsupported "+kind, node.Loc())
}

func lowerLiteralKind(kind ast.LiteralKind) hir.LiteralKind {
	switch kind {
	case ast.INT:
		return hir.LiteralInt
	case ast.FLOAT:
		return hir.LiteralFloat
	case ast.STRING:
		return hir.LiteralString
	case ast.BYTE:
		return hir.LiteralByte
	case ast.BOOL:
		return hir.LiteralBool
	case ast.NONE:
		return hir.LiteralNone
	default:
		return hir.LiteralInt
	}
}

func locFromNode(node ast.Node) source.Location {
	if node == nil || node.Loc() == nil {
		return source.Location{}
	}
	return *node.Loc()
}
