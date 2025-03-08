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

// Parse is the entry point for parsing
func (p *Parser) Parse() []ast.Node {

	var nodes []ast.Node

	for !p.isAtEnd() {
		var node ast.Node
		if p.match(lexer.LET_TOKEN, lexer.CONST_TOKEN) {
			node = parseVarDecl(p)
		} else if p.match(lexer.TYPE_TOKEN) {
			node = parseTypeDecl(p)
		} else if p.match(lexer.IDENTIFIER_TOKEN) {
			node = handleIdentifierPath(p)
		} else {
			p.advance()
		}

		// if the node is not nil, add it to the nodes list
		if node != nil {
			nodes = append(nodes, node)
		}
	}

	return nodes
}

// handleIdentifierPath parses either a standalone identifier or an assignment
func handleIdentifierPath(p *Parser) ast.Node {
	iden := parseIdentifier(p)

	// if the identifier is nil, return nil
	if iden == nil {
		return nil
	}

	if p.match(lexer.COMMA_TOKEN, lexer.EQUALS_TOKEN) {
		return parseAssignment(p, iden)
	}
	return iden
}
