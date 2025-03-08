package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
)

func parseIdentifier(p *Parser) ast.Expression {
	token := p.consume(lexer.IDENTIFIER_TOKEN, "Expected identifier")
	identifier := &ast.IdentifierExpr{
		Name: token.Value,
		Location: ast.Location{
			Start: token.Start,
			End:   token.End,
		},
	}

	return identifier
}