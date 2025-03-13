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

// next returns the next token without consuming it
func (p *Parser) next() lexer.Token {
	if p.current+1 >= len(p.tokens) {
		return lexer.Token{Kind: lexer.EOF_TOKEN}
	}
	return p.tokens[p.current+1]
}

// is at end of file
func (p *Parser) isAtEnd() bool {
	return p.peek().Kind == lexer.EOF_TOKEN
}

// rollback to N
func (p *Parser) rollback(n int) {
	p.current -= n
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
func parseExpressionList(p *Parser, first ast.Expression) ast.ExpressionList {
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

// parseExpressionStatement parses an expression statement
func parseExpressionStatement(p *Parser, first ast.Expression) ast.Statement {
	exprs := parseExpressionList(p, first)

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
func handleUnexpectedToken(p *Parser) ast.Statement {
	report.Add(p.filePath,
		p.peek().Start.Line, p.peek().End.Line,
		p.peek().Start.Column, p.peek().End.Column,
		fmt.Sprintf(report.UNEXPECTED_TOKEN+" `%s`", p.peek().Value)).SetLevel(report.SYNTAX_ERROR)

	p.advance() // skip the invalid token

	return nil
}

// parseBlock parses a block of statements
func parseBlock(p *Parser) *ast.BlockStmt {
	start := p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE).Start
	statements := make([]ast.Statement, 0)

	for !p.isAtEnd() && p.peek().Kind != lexer.CLOSE_CURLY {
		stmt := parseStatement(p)
		if stmt == nil {
			return nil
		}
		statements = append(statements, stmt)
	}

	end := p.consume(lexer.CLOSE_CURLY, report.EXPECTED_CLOSE_BRACE).End

	return &ast.BlockStmt{
		Statements: statements,
		Location: ast.Location{
			Start: &start,
			End:   &end,
		},
	}
}

// parseStatement parses a single statement
func parseStatement(p *Parser) ast.Statement {
	switch p.peek().Kind {
	case lexer.LET_TOKEN, lexer.CONST_TOKEN:
		return parseVarDecl(p)
	case lexer.TYPE_TOKEN:
		return parseTypeDecl(p)
	case lexer.RETURN_TOKEN:
		return parseReturnStmt(p)
	case lexer.IDENTIFIER_TOKEN, lexer.STRUCT_TOKEN:
		// Look ahead to see if this is an assignment
		expr := parseExpression(p)
		if expr != nil {
			// if the expression is valid, parse it as an expression statement
			return parseExpressionStatement(p, expr)
		} else {
			// if the expression is invalid, report an error
			return handleUnexpectedToken(p)
		}
	default:
		return handleUnexpectedToken(p)
	}
}

// parseReturnStmt parses a return statement
func parseReturnStmt(p *Parser) ast.Statement {
	start := p.consume(lexer.RETURN_TOKEN, report.EXPECTED_RETURN_KEYWORD).Start

	// Check if there's a value to return
	var value ast.Expression
	if !p.match(lexer.SEMICOLON_TOKEN) {
		value = parseExpression(p)
		if value == nil {
			return nil
		}
	}

	end := value.EndPos()
	if end == nil {
		end = &start
	}

	return &ast.ReturnStmt{
		Value: value,
		Location: ast.Location{
			Start: &start,
			End:   end,
		},
	}
}

// Parse is the entry point for parsing
func (p *Parser) Parse() []ast.Node {
	var nodes []ast.Node

	//fmt.Printf("Parsing file: %s\nTokens:\n%v\n", p.filePath, p.tokens)

	for !p.isAtEnd() {
		// Parse the statement
		node := parseStatement(p)

		// Handle statement termination and update locations
		if node != nil {

			//if no semicolon, show error on the previous token
			if !p.match(lexer.SEMICOLON_TOKEN) {
				previous := p.previous()
				report.Add(p.filePath,
					previous.Start.Line, previous.End.Line,
					previous.Start.Column, previous.End.Column,
					"Expected ';' after statement").AddHint("Add a semicolon to the end of the statement").SetLevel(report.SYNTAX_ERROR)
			}

			end := p.advance()
			node.EndPos().Column = end.End.Column
			node.EndPos().Line = end.End.Line
			nodes = append(nodes, node)
		}
	}

	return nodes
}
