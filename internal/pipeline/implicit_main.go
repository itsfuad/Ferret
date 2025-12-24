package pipeline

import (
	"compiler/internal/frontend/ast"
	"compiler/internal/source"
)

func (p *Pipeline) injectImplicitMain(importPath string, module *ast.Module) bool {
	if p == nil || p.ctx == nil || module == nil {
		return false
	}
	if importPath != p.ctx.EntryModule {
		return false
	}

	for _, node := range module.Nodes {
		if fn, ok := node.(*ast.FuncDecl); ok && fn.Name != nil && fn.Name.Name == "main" {
			return false
		}
	}

	var kept []ast.Node
	var body []ast.Node

	for _, node := range module.Nodes {
		switch node.(type) {
		case *ast.ImportStmt, *ast.TypeDecl, *ast.FuncDecl, *ast.MethodDecl, *ast.VarDecl, *ast.ConstDecl:
			kept = append(kept, node)
		default:
			body = append(body, node)
		}
	}

	mainDecl := buildImplicitMain(module, body)
	kept = append(kept, mainDecl)
	module.Nodes = kept
	return true
}

func buildImplicitMain(module *ast.Module, body []ast.Node) *ast.FuncDecl {
	loc := implicitMainLocation(module, body)
	return &ast.FuncDecl{
		Name: &ast.IdentifierExpr{
			Name:     "main",
			Location: loc,
		},
		Type: &ast.FuncType{
			Params:   nil,
			Result:   nil,
			Location: loc,
		},
		Body: &ast.Block{
			Nodes:    body,
			Location: loc,
		},
		Location: loc,
	}
}

func implicitMainLocation(module *ast.Module, body []ast.Node) source.Location {
	filename := module.FullPath
	if filename == "" && module.Location.Filename != nil {
		filename = *module.Location.Filename
	}
	if filename == "" {
		filename = module.ImportPath
	}

	start, end := spanFromNodes(body)
	if start == nil || end == nil {
		start, end = spanFromNodes(module.Nodes)
	}
	if start == nil {
		start = &source.Position{Line: 1, Column: 1, Index: 0}
	}
	if end == nil {
		end = &source.Position{Line: 1, Column: 1, Index: 0}
	}

	return *source.NewLocation(&filename, start, end)
}

func spanFromNodes(nodes []ast.Node) (*source.Position, *source.Position) {
	var start *source.Position
	var end *source.Position
	for _, node := range nodes {
		if node == nil || node.Loc() == nil {
			continue
		}
		loc := node.Loc()
		if start == nil && loc.Start != nil {
			start = loc.Start
		}
		if loc.End != nil {
			end = loc.End
		}
	}
	return start, end
}
