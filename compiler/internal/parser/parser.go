package parser

import (
	"errors"
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

// Helper methods for token handling
func (p *Parser) peek() lexer.Token {
	return p.tokens[p.current]
}

func (p *Parser) previous() lexer.Token {
	return p.tokens[p.current-1]
}

func (p *Parser) isAtEnd() bool {
	return p.peek().Kind == lexer.EOF_TOKEN
}

func (p *Parser) advance() lexer.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *Parser) check(kind lexer.TOKEN) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().Kind == kind
}

func (p *Parser) match(kinds ...lexer.TOKEN) bool {
	for _, kind := range kinds {
		if p.check(kind) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *Parser) consume(kind lexer.TOKEN, message string) (lexer.Token, error) {
	if p.check(kind) {
		return p.advance(), nil
	}

	err := report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
		p.peek().Start.Column, p.peek().End.Column, message)
	err.SetLevel(report.SYNTAX_ERROR)
	return p.peek(), errors.New(message)
}

// Parse is the entry point for parsing
func (p *Parser) Parse() []ast.Node {
	var nodes []ast.Node

	

	return nodes
}
