package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
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
			Name: name.Value,
			Location: ast.Location{
				Start: &name.Start,
				End:   &name.End,
			},
		},
		Location: ast.Location{
			Start: &start.Start,
			End:   &name.End,
		},
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

	return &ast.ImportStmt{
		Import: &ast.StringLiteral{
			Value: importPath.Value,
			Location: ast.Location{
				Start: &importPath.Start,
				End:   &importPath.End,
			},
		},
		Location: ast.Location{
			Start: &start.Start,
			End:   &importPath.End,
		},
	}
}
