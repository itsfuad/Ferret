package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
)

func (p *Parser) expression() ast.Expression {
	return p.equality()
}

func (p *Parser) equality() ast.Expression {
	expr := p.comparison()

	for p.match(lexer.DOUBLE_EQUAL_TOKEN, lexer.NOT_EQUAL_TOKEN) {
		operator := p.previous()
		right := p.comparison()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
		}
	}

	return expr
}

func (p *Parser) comparison() ast.Expression {
	expr := p.term()

	for p.match(
		lexer.GREATER_TOKEN,
		lexer.GREATER_EQUAL_TOKEN,
		lexer.LESS_TOKEN,
		lexer.LESS_EQUAL_TOKEN,
	) {
		operator := p.previous()
		right := p.term()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
		}
	}

	return expr
}

func (p *Parser) term() ast.Expression {
	expr := p.factor()

	for p.match(lexer.PLUS_TOKEN, lexer.MINUS_TOKEN) {
		operator := p.previous()
		right := p.factor()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
		}
	}

	return expr
}

func (p *Parser) factor() ast.Expression {
	expr := p.unary()

	for p.match(lexer.MUL_TOKEN, lexer.DIV_TOKEN, lexer.MOD_TOKEN) {
		operator := p.previous()
		right := p.unary()
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
		}
	}

	return expr
}

func (p *Parser) unary() ast.Expression {
	if p.match(lexer.NOT_TOKEN, lexer.MINUS_TOKEN) {
		operator := p.previous()
		right := p.unary()
		return &ast.UnaryExpr{
			Operator: operator,
			Right:    right,
		}
	}

	return p.primary()
}

func (p *Parser) primary() ast.Expression {
	switch {
	case p.match(lexer.INT32_TOKEN, lexer.FLOAT32_TOKEN, lexer.STR_TOKEN, lexer.BOOL_TOKEN):
		return &ast.LiteralExpr{Value: p.previous()}

	case p.match(lexer.IDENTIFIER_TOKEN):
		return &ast.IdentifierExpr{Name: p.previous()}

	case p.match(lexer.OPEN_PAREN):
		expr := p.expression()
		p.consume(lexer.CLOSE_PAREN, "Expect ')' after expression")
		return expr
	}

	err := report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
		p.peek().Start.Column, p.peek().End.Column, "Expected expression")
	err.SetLevel(report.SYNTAX_ERROR)
	return nil
}
