package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
)

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
