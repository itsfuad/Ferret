package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
	"fmt"
)

type Parser struct {
	tokens   []lexer.Token
	current  int
	filePath string
}

func New(filePath string, debug bool) *Parser {
	tokens := lexer.Tokenize(filePath, debug)
	return &Parser{
		tokens:   tokens,
		current:  0,
		filePath: filePath,
	}
}

// current token
func (p *Parser) peek() lexer.Token {
	return p.tokens[p.current]
}

// previous token
func (p *Parser) previous() lexer.Token {
	return p.tokens[p.current-1]
}

// is at end of file
func (p *Parser) isAtEnd() bool {
	return p.peek().Kind == lexer.EOF_TOKEN
}

// consume the current token and return that token
func (p *Parser) advance() lexer.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

// check if the current token is of the given kind
func (p *Parser) check(kind lexer.TOKEN) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().Kind == kind
}

// consume the current token if it is of the given kind
func (p *Parser) match(kinds ...lexer.TOKEN) bool {
	if p.isAtEnd() {
		return false
	}

	for _, kind := range kinds {
		if p.peek().Kind == kind {
			return true
		}
	}
	return false
}

// consume the current token if it is of the given kind and return that token
// otherwise, report an error
func (p *Parser) consume(kind lexer.TOKEN, message string) lexer.Token {
	if p.check(kind) {
		return p.advance()
	}

	current := p.peek()

	err := report.Add(p.filePath, current.Start.Line, current.End.Line,
		current.Start.Column, current.End.Column, message)
	err.SetLevel(report.SYNTAX_ERROR)
	return p.peek()
}

// Parse is the entry point for parsing
func (p *Parser) Parse() []ast.Node {

	fmt.Println("Parsing program")

	var nodes []ast.Node

	for !p.isAtEnd() {
		if p.match(lexer.LET_TOKEN, lexer.CONST_TOKEN) {
			nodes = append(nodes, parseVarDecl(p))
		} else if p.match(lexer.IDENTIFIER_TOKEN) {
			nodes = append(nodes, parseAssignment(p))
		} else {
			p.advance()
		}
	}

	return nodes
}

func parseAssignment(p *Parser) ast.Node {

	/*
		Syntax:
		identifier = expression;
		assignee1, assignee2 = expression1, expression2;
		assignee1, assignee2, assignee3, ... = expression1, expression2, expression3, ...;
	*/

	assignees := ast.ExpressionList{}
	expressions := ast.ExpressionList{}

	for p.peek().Kind != lexer.EQUALS_TOKEN {
		expr := parseExpression(p)
		assignees = append(assignees, expr)
		if p.peek().Kind == lexer.COMMA_TOKEN {
			p.advance()
		} else {
			break
		}
	}

	// if we have =, this is surely an assignment
	if p.peek().Kind != lexer.EQUALS_TOKEN {
		if len(assignees) == 1 {
			return assignees[0]
		}
		return assignees
	}

	p.advance()

	for p.peek().Kind != lexer.SEMI_COLON_TOKEN {
		expr := parseExpression(p)
		expressions = append(expressions, expr)
		if p.peek().Kind == lexer.COMMA_TOKEN {
			p.advance()
		}
	}

	if len(assignees) != len(expressions) {
		if len(assignees) > len(expressions) {
			report.Add(p.filePath, expressions.StartPos().Line, expressions.EndPos().Line, expressions.StartPos().Column, expressions.EndPos().Column, "Insufficient values for assignment").SetLevel(report.SYNTAX_ERROR)
		} else {
			report.Add(p.filePath, assignees.StartPos().Line, assignees.EndPos().Line, assignees.StartPos().Column, assignees.EndPos().Column, "Too many values for assignment").AddHint("Remove the extra values").SetLevel(report.SYNTAX_ERROR)
		}
	}

	end := p.consume(lexer.SEMI_COLON_TOKEN, "Expected ';' after assignment")

	return &ast.AssignmentExpr{
		Left:  assignees,
		Right: expressions,
		Location: ast.Location{
			Start: assignees[0].StartPos(),
			End:   end.End,
		},
	}
}
