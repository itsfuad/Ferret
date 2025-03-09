package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
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

// matches the current token with any of the given kinds
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

// parseExpressionList parses a comma-separated list of expressions
func (p *Parser) parseExpressionList(first ast.Expression) ast.ExpressionList {
	exprs := ast.ExpressionList{first}
	for p.match(lexer.COMMA_TOKEN) {
		p.advance() // consume comma
		next := parseExpression(p)
		if next == nil {
			report.Add(p.filePath,
				p.peek().Start.Line, p.peek().End.Line,
				p.peek().Start.Column, p.peek().End.Column,
				"Expected expression after comma").SetLevel(report.SYNTAX_ERROR)
			break
		}
		exprs = append(exprs, next)
	}
	return exprs
}

// parseExpressionStatement parses an expression or comma-separated expressions
func (p *Parser) parseExpressionStatement(first ast.Expression) ast.Node {
	exprs := p.parseExpressionList(first)

	// Check for assignment
	if p.match(lexer.EQUALS_TOKEN) {
		return parseAssignment(p, exprs...)
	}

	return &ast.ExpressionStmt{
		Expressions: exprs,
		Location: ast.Location{
			Start: first.StartPos(),
			End:   exprs[len(exprs)-1].EndPos(),
		},
	}
}

// handleUnexpectedToken reports an error for unexpected token and advances
func (p *Parser) handleUnexpectedToken() {
	report.Add(p.filePath,
		p.peek().Start.Line, p.peek().End.Line,
		p.peek().Start.Column, p.peek().End.Column,
		"Unexpected token").SetLevel(report.SYNTAX_ERROR)
	p.advance() // skip the invalid token
}

// Parse is the entry point for parsing
func (p *Parser) Parse() []ast.Node {
	var nodes []ast.Node

	for !p.isAtEnd() {
		var node ast.Node

		// Parse the statement
		if p.match(lexer.LET_TOKEN, lexer.CONST_TOKEN) {
			node = parseVarDecl(p)
		} else if p.match(lexer.TYPE_TOKEN) {
			node = parseTypeDecl(p)
		} else {
			expr := parseExpression(p)
			if expr != nil {
				node = p.parseExpressionStatement(expr)
			} else {
				p.handleUnexpectedToken()
			}
		}

		// Handle statement termination and update locations
		if node != nil {
			end := p.consume(lexer.SEMI_COLON_TOKEN, "Expected ';' after statement")

			node.EndPos().Column = end.End.Column
			node.EndPos().Line = end.End.Line
			nodes = append(nodes, node)
		}
	}

	return nodes
}
