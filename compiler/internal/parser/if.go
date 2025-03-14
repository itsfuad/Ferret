package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
)

// parseIfStatement parses an if statement with optional else and else-if branches
func parseIfStatement(p *Parser) ast.Statement {
	start := p.advance() // consume 'if'

	// Parse condition (parentheses are optional)
	var condition ast.Expression
	if p.match(lexer.OPEN_PAREN) {
		p.advance() // consume '('
		condition = parseExpression(p)
		p.consume(lexer.CLOSE_PAREN, report.EXPECTED_CLOSE_PAREN)
	} else {
		condition = parseExpression(p)
	}

	if condition == nil {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			"Expected condition after 'if'").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	// Parse if body
	p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE)
	body := parseBlock(p)
	if body == nil {
		return nil
	}

	ifStmt := &ast.IfStmt{
		Condition: condition,
		Body:      &body,
		Location: ast.Location{
			Start: &start.Start,
			End:   body.EndPos(),
		},
	}

	if p.match(lexer.ELSE_TOKEN) {
		elseToken := p.advance() // consume 'else'
		if p.match(lexer.IF_TOKEN) {
			// Parse else-if branch
			p.advance() // consume 'if'
			stmt := parseIfStatement(p)
			ifStmt.Alternative = stmt
		} else {
			// Parse else branch
			stmt := parseBlock(p)
			ifStmt.Alternative = stmt
		}
		ifStmt.Alternative.StartPos().Line = elseToken.Start.Line
		ifStmt.Alternative.StartPos().Column = elseToken.Start.Column
		ifStmt.Alternative.EndPos().Line = elseToken.End.Line
		ifStmt.Alternative.EndPos().Column = elseToken.End.Column
	}

	return ifStmt
}
