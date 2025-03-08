package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
)

func parseAssignment(p *Parser, firstIdent ast.Expression) ast.Statement {

	assignees := ast.ExpressionList{firstIdent}
	expressions := ast.ExpressionList{}

	for p.peek().Kind == lexer.COMMA_TOKEN {
		p.advance()
		assignees = append(assignees, parseExpression(p))
	}

	p.consume(lexer.EQUALS_TOKEN, "Expected '=' in assignment")

	for {
		expressions = append(expressions, parseExpression(p))
		if p.peek().Kind == lexer.COMMA_TOKEN {
			p.advance()
		} else {
			break
		}
	}

	end := p.consume(lexer.SEMI_COLON_TOKEN, "Expected ';' after assignment")

	if len(assignees) != len(expressions) {
		report.Add(p.filePath, end.Start.Line, end.End.Line, end.Start.Column, end.End.Column, "Mismatched number of variables and values").SetLevel(report.SYNTAX_ERROR)
	}

	return &ast.AssignmentStmt{
		Left:  assignees,
		Right: expressions,
		Location: ast.Location{
			Start: assignees[0].StartPos(),
			End:   end.End,
		},
	}
}
