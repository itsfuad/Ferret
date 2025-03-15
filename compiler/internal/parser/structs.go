package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
)

// parseFieldAccess parses a field access expression like struct.field
func parseFieldAccess(p *Parser, object ast.Expression) ast.Expression {
	p.advance() // consume '.'

	// Parse field name
	if !p.match(lexer.IDENTIFIER_TOKEN) {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			"Expected field name after '.'").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	fieldToken := p.advance()
	field := &ast.IdentifierExpr{
		Name: fieldToken.Value,
		Location: ast.Location{
			Start: &fieldToken.Start,
			End:   &fieldToken.End,
		},
	}

	return &ast.FieldAccessExpr{
		Object: object,
		Field:  field,
		Location: ast.Location{
			Start: object.StartPos(),
			End:   &fieldToken.End,
		},
	}
}
