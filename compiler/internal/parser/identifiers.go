package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
)

func parseIdentifier(p *Parser) *ast.IdentifierExpr {
	token := p.consume(lexer.IDENTIFIER_TOKEN, "Expected identifier")

	// Create the identifier expression
	identifier := &ast.IdentifierExpr{
		Name: token.Value,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}

	// Check if identifier is defined in current scope
	if _, found := p.currentScope.Resolve(token.Value); !found {
		report.Add(p.filePath,
			token.Start.Line,
			token.End.Line,
			token.Start.Column,
			token.End.Column,
			"Undefined identifier '"+token.Value+"'").SetLevel(report.NORMAL_ERROR)
	}

	return identifier
}
