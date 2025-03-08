package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
)

// parseExpression is the entry point for expression parsing
func parseExpression(p *Parser) ast.Expression {
	return parseLogicalOr(p)
}

// parseLogicalOr handles || operator
func parseLogicalOr(p *Parser) ast.Expression {
	expr := parseLogicalAnd(p)

	for p.match(lexer.OR_TOKEN) {
		operator := p.advance()
		right := parseLogicalAnd(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseLogicalAnd handles && operator
func parseLogicalAnd(p *Parser) ast.Expression {
	expr := parseEquality(p)

	for p.match(lexer.AND_TOKEN) {
		operator := p.advance()
		right := parseEquality(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseEquality handles == and != operators
func parseEquality(p *Parser) ast.Expression {
	expr := parseComparison(p)

	for p.match(lexer.DOUBLE_EQUAL_TOKEN, lexer.NOT_EQUAL_TOKEN) {
		operator := p.advance()
		right := parseComparison(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseComparison handles <, >, <=, >= operators
func parseComparison(p *Parser) ast.Expression {
	expr := parseAdditive(p)

	for p.match(lexer.LESS_TOKEN, lexer.GREATER_TOKEN, lexer.LESS_EQUAL_TOKEN, lexer.GREATER_EQUAL_TOKEN) {
		operator := p.advance()
		right := parseAdditive(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseAdditive handles + and - operators
func parseAdditive(p *Parser) ast.Expression {
	expr := parseMultiplicative(p)

	for p.match(lexer.PLUS_TOKEN, lexer.MINUS_TOKEN) {
		operator := p.advance()
		right := parseMultiplicative(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseMultiplicative handles *, /, and % operators
func parseMultiplicative(p *Parser) ast.Expression {
	expr := parseUnary(p)

	for p.match(lexer.MUL_TOKEN, lexer.DIV_TOKEN, lexer.MOD_TOKEN) {
		operator := p.advance()
		right := parseUnary(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseUnary handles unary operators (!, -)
func parseUnary(p *Parser) ast.Expression {
	if p.match(lexer.NOT_TOKEN, lexer.MINUS_TOKEN) {
		operator := p.advance()
		right := parseUnary(p)
		return &ast.UnaryExpr{
			Operator: operator,
			Operand:  right,
			Location: ast.Location{
				Start: operator.Start,
				End:   right.EndPos(),
			},
		}
	}

	return parsePrimary(p)
}

// parsePrimary handles literals, identifiers, and parenthesized expressions
func parsePrimary(p *Parser) ast.Expression {
	switch p.peek().Kind {
	case lexer.OPEN_PAREN:
		p.advance() // consume '('
		expr := parseExpression(p)
		p.consume(lexer.CLOSE_PAREN, "Expected ')' after expression")
		return expr
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
