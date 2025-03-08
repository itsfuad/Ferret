package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
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

// parseUnary handles unary operators (!, -, ++, --)
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

	// Handle prefix operators (++, --)
	if p.match(lexer.PLUS_PLUS_TOKEN, lexer.MINUS_MINUS_TOKEN) {
		operator := p.advance()
		// Check for consecutive operators
		if p.match(lexer.PLUS_PLUS_TOKEN, lexer.MINUS_MINUS_TOKEN) {
			errMsg := report.INVALID_CONSECUTIVE_INCREMENT
			if operator.Kind == lexer.MINUS_MINUS_TOKEN {
				errMsg = report.INVALID_CONSECUTIVE_DECREMENT
			}
			report.Add(p.filePath, operator.Start.Line, operator.End.Line, operator.Start.Column, operator.End.Column, errMsg).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		operand := parseUnary(p)
		if operand == nil {
			errMsg := report.INVALID_INCREMENT_OPERAND
			if operator.Kind == lexer.MINUS_MINUS_TOKEN {
				errMsg = report.INVALID_DECREMENT_OPERAND
			}
			report.Add(p.filePath, operator.Start.Line, operator.End.Line, operator.Start.Column, operator.End.Column, errMsg).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		return &ast.PrefixExpr{
			Operator: operator,
			Operand:  operand,
			Location: ast.Location{
				Start: operator.Start,
				End:   operand.EndPos(),
			},
		}
	}

	return parsePostfix(p)
}

// parsePostfix handles postfix operators (++, --)
func parsePostfix(p *Parser) ast.Expression {
	expr := parsePrimary(p)
	if expr == nil {
		return nil
	}

	// Handle postfix operators (++, --)
	if p.match(lexer.PLUS_PLUS_TOKEN, lexer.MINUS_MINUS_TOKEN) {
		operator := p.advance()
		// Check for consecutive operators
		if p.match(lexer.PLUS_PLUS_TOKEN, lexer.MINUS_MINUS_TOKEN) {
			errMsg := report.INVALID_CONSECUTIVE_INCREMENT
			if operator.Kind == lexer.MINUS_MINUS_TOKEN {
				errMsg = report.INVALID_CONSECUTIVE_DECREMENT
			}
			report.Add(p.filePath, operator.Start.Line, operator.End.Line, operator.Start.Column, operator.End.Column, errMsg).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		return &ast.PostfixExpr{
			Operand:  expr,
			Operator: operator,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   operator.End,
			},
		}
	}

	return expr
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
