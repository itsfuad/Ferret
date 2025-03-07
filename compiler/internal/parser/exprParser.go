package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
)

func parseExpression(p *Parser) ast.Expression {
	switch p.peek().Kind {
	case lexer.OPEN_BRACKET:
		return parseArrayLiteral(p)
	case lexer.NUMBER_TOKEN:
		return parseNumberLiteral(p)
	case lexer.STRING_TOKEN:
		return parseStringLiteral(p)
	case lexer.IDENTIFIER_TOKEN:
		return parseIdentifier(p)
	}
	return nil
}

func parseNumberLiteral(p *Parser) ast.Expression {
	number := p.consume(lexer.NUMBER_TOKEN, "Expected number")
	loc := ast.Location{
		Start: number.Start,
		End:   number.End,
	}

	if types.IsInteger(number.Value) {
		return &ast.IntLiteral{
			Value:    number.Value,
			Location: loc,
		}
	} else if types.IsFloat(number.Value) {
		return &ast.FloatLiteral{
			Value:    number.Value,
			Location: loc,
		}
	}

	return nil
}

func parseStringLiteral(p *Parser) ast.Expression {
	stringLiteral := p.consume(lexer.STRING_TOKEN, "Expected string")
	loc := ast.Location{
		Start: stringLiteral.Start,
		End:   stringLiteral.End,
	}

	return &ast.StringLiteral{
		Value:    stringLiteral.Value,
		Location: loc,
	}
}

func parseIdentifier(p *Parser) ast.Expression {
	identifier := p.consume(lexer.IDENTIFIER_TOKEN, "Expected identifier")
	return &ast.IdentifierExpr{
		Name: identifier.Value,
		Location: ast.Location{
			Start: identifier.Start,
			End:   identifier.End,
		},
	}
}

func parseArrayLiteral(p *Parser) ast.Expression {
	start := p.advance().Start // consume '['
	elements := make([]ast.Expression, 0)

	for !p.check(lexer.CLOSE_BRACKET) {
		expr := parseExpression(p)
		if expr == nil {
			return nil
		}
		elements = append(elements, expr)

		if p.peek().Kind == lexer.COMMA_TOKEN {
			p.advance() // consume ','
		}
	}

	end := p.advance().End // consume ']'
	return &ast.ArrayLiteralExpr{
		Elements: elements,
		Location: ast.Location{
			Start: start,
			End:   end,
		},
	}
}
