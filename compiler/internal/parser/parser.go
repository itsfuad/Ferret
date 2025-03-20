package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"fmt"
	"slices"
)

type Parser struct {
	tokens       []lexer.Token
	current      int
	filePath     string
	symbolTable  *symboltable.SymbolTable
	currentScope *symboltable.SymbolTable
}

func New(filePath string, debug bool) *Parser {
	tokens := lexer.Tokenize(filePath, debug)
	globalScope := symboltable.NewSymbolTable(nil, symboltable.GLOBAL_SCOPE)
	return &Parser{
		tokens:       tokens,
		current:      0,
		filePath:     filePath,
		symbolTable:  globalScope,
		currentScope: globalScope,
	}
}

// enterScope creates a new scope of the given kind and makes it current
func (p *Parser) enterScope(kind symboltable.ScopeKind) *symboltable.SymbolTable {
	p.currentScope = p.currentScope.EnterScope(kind)
	return p.currentScope
}

// exitScope returns to the parent scope
func (p *Parser) exitScope() {
	p.currentScope = p.currentScope.ExitScope()
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

	return slices.Contains(kinds, p.peek().Kind)
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
func parseBlock(p *Parser) ast.BlockConstruct {
	start := p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE).Start

	// Create a new block scope
	p.enterScope(symboltable.BLOCK_SCOPE)
	defer p.exitScope() // Ensure we exit the scope even if there's an error

	nodes := make([]ast.Node, 0)

	for !p.isAtEnd() && p.peek().Kind != lexer.CLOSE_CURLY {
		node := parseNode(p)
		if node != nil {
			nodes = append(nodes, node)
		}
	}

	end := p.consume(lexer.CLOSE_CURLY, report.EXPECTED_CLOSE_BRACE).End

	return &ast.Block{
		Nodes: nodes,
		Location: ast.Location{
			Start: &start,
			End:   &end,
		},
	}
}

// parseReturnStmt parses a return statement
func parseReturnStmt(p *Parser) ast.Statement {

	start := p.consume(lexer.RETURN_TOKEN, report.EXPECTED_RETURN_KEYWORD).Start
	end := start
	// Check if there's a values to return
	var values ast.ExpressionList
	if !p.match(lexer.SEMICOLON_TOKEN) {
		values = parseExpressionList(p, parseExpression(p))
		if values == nil {
			token := p.peek()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.INVALID_EXPRESSION).AddHint("Add an expression after the return keyword").SetLevel(report.SYNTAX_ERROR)
		}
		end = *values.EndPos()
	}

	return &ast.ReturnStmt{
		Values: values,
		Location: ast.Location{
			Start: &start,
			End:   &end,
		},
	}
}

// parseNode parses a single statement or expression
func parseNode(p *Parser) ast.Node {
	var node ast.Node
	switch p.peek().Kind {
	case lexer.LET_TOKEN, lexer.CONST_TOKEN:
		node = parseVarDecl(p)
	case lexer.TYPE_TOKEN:
		node = parseTypeDecl(p)
	case lexer.RETURN_TOKEN:
		node = parseReturnStmt(p)
	case lexer.FUNCTION_TOKEN:
		node = parseFunctionLike(p)
	case lexer.IF_TOKEN:
		node = parseIfStatement(p)
	case lexer.AT_TOKEN:
		node = parseStructLiteral(p)
	case lexer.IDENTIFIER_TOKEN:
		// Look ahead to see if this is an assignment
		expr := parseExpression(p)
		if expr != nil {
			// if the expression is valid, parse it as an expression statement
			node = parseExpressionStatement(p, expr)
		} else {
			fmt.Printf("Invalid expression: %+v\n", expr)
			// if the expression is invalid, report an error
			node = handleUnexpectedToken(p)
		}
	default:
		fmt.Printf("Invalid token: %+v\n", p.peek())
		node = handleUnexpectedToken(p)
	}

	// Handle statement termination and update locations
	if _, ok := node.(ast.Statement); ok {
		//if no semicolon, show error on the previous token
		if !p.match(lexer.SEMICOLON_TOKEN) {
			token := p.peek()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, "Expected ';' after statement").AddHint("Add a semicolon to the end of the statement").SetLevel(report.SYNTAX_ERROR)
		}
		end := p.advance()
		node.EndPos().Column = end.End.Column
		node.EndPos().Line = end.End.Line
	}

	return node
}

// Parse is the entry point for parsing
func (p *Parser) Parse() []ast.Node {
	var nodes []ast.Node

	for !p.isAtEnd() {
		// Parse the statement
		node := parseNode(p)
		if node != nil {
			nodes = append(nodes, node)
		} else {
			handleUnexpectedToken(p)
			break
		}
	}

	return nodes
}
