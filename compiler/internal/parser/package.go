package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/source"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/internal/types"
	"ferret/compiler/report"
	"strings"
)

// parsePackage parses a package declaration
func parsePackage(p *Parser) ast.Node {
	// Check if this is the first statement in the file

	valid := p.tokenNo == 0

	start := p.consume(lexer.PACKAGE_TOKEN, report.EXPECTED_PACKAGE_KEYWORD)

	name := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_PACKAGE_NAME)

	if !valid {
		report.Add(p.filePath, start.Start.Line, name.End.Line, start.Start.Column, name.End.Column, report.INVALID_SCOPE).AddHint("Package declarations must be at the top level of the file").SetLevel(report.SYNTAX_ERROR)
	}

	// check scope
	if p.currentScope.ScopeKind() != symboltable.GLOBAL_SCOPE {
		report.Add(p.filePath, name.Start.Line, name.End.Line, name.Start.Column, name.End.Column, report.INVALID_SCOPE).AddHint("Package declarations must be at the top level of the file").SetLevel(report.SYNTAX_ERROR)
	}

	return &ast.PackageDeclStmt{
		Package: &ast.IdentifierExpr{
			Name:     name.Value,
			Location: *source.NewLocation(&name.Start, &name.End),
		},
		Location: *source.NewLocation(&start.Start, &name.End),
	}
}

// parseImport parses an import statement
func parseImport(p *Parser) ast.Node {
	start := p.consume(lexer.IMPORT_TOKEN, report.EXPECTED_IMPORT_KEYWORD)
	importPath := p.consume(lexer.STRING_TOKEN, report.EXPECTED_IMPORT_PATH)

	// check scope
	if p.currentScope.ScopeKind() != symboltable.GLOBAL_SCOPE {
		report.Add(p.filePath, start.Start.Line, importPath.End.Line, start.Start.Column, importPath.End.Column, report.INVALID_SCOPE).AddHint("Import statements must be at the top level of the file").SetLevel(report.SYNTAX_ERROR)
	}

	// Get module name from import path (last component)
	moduleName := strings.Trim(importPath.Value, "\"")
	if lastSlash := strings.LastIndex(moduleName, "/"); lastSlash >= 0 {
		moduleName = moduleName[lastSlash+1:]
	}

	// Add module to symbol table
	sym := &symboltable.Symbol{
		Name:       moduleName,
		SymbolKind: symboltable.MODULE_SYMBOL,
		Type:       types.MODULE,
		Module:     moduleName,
		Location: symboltable.SymbolLocation{
			File:   p.filePath,
			Line:   start.Start.Line,
			Column: start.Start.Column,
		},
	}

	if !p.currentScope.Define(sym) {
		report.Add(p.filePath, start.Start.Line, importPath.End.Line, start.Start.Column, importPath.End.Column, "Module already imported").SetLevel(report.SEMANTIC_ERROR)
	}

	return &ast.ImportStmt{
		Import: &ast.StringLiteral{
			Value:    importPath.Value,
			Location: *source.NewLocation(&importPath.Start, &importPath.End),
		},
		Location: *source.NewLocation(&start.Start, &importPath.End),
	}
}

func parseScopeResolution(p *Parser, expr ast.Expression) (ast.Expression, bool) {
	// Handle scope resolution operator
	if module, ok := expr.(*ast.IdentifierExpr); ok {
		p.consume(lexer.SCOPE_TOKEN, report.EXPECTED_SCOPE_RESOLUTION_OPERATOR)
		if !p.match(lexer.IDENTIFIER_TOKEN) {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Expected identifier after '::'").SetLevel(report.SYNTAX_ERROR)
			return nil, false
		}
		member := parseIdentifier(p)
		return &ast.ScopeResolutionExpr{
			Module:     module,
			Identifier: member,
			Location:   *source.NewLocation(module.StartPos(), member.EndPos()),
		}, true
	} else {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Left side of '::' must be an identifier").SetLevel(report.SYNTAX_ERROR)
		return nil, false
	}
}
