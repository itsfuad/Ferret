package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
)

func (p *Parser) declaration() ast.Node {
	if p.match(lexer.LET_TOKEN) {
		return p.varDeclaration()
	}
	return p.statement()
}

func (p *Parser) varDeclaration() ast.Node {
	name, err := p.consume(lexer.IDENTIFIER_TOKEN, "Expect variable name")
	if err != nil {
		return nil
	}

	// Check for type annotation
	var typeName lexer.Token
	if p.match(lexer.COLON_TOKEN) {
		typeToken, err := p.consume(lexer.IDENTIFIER_TOKEN, "Expect type after ':'")
		if err != nil {
			return nil
		}
		typeName = typeToken
	}

	// Check for initializer
	var initializer ast.Expression
	if p.match(lexer.EQUALS_TOKEN) {
		initializer = p.expression()
	}

	_, err = p.consume(lexer.SEMI_COLON_TOKEN, "Expect ';' after variable declaration")
	if err != nil {
		return nil
	}

	return &ast.VarDeclStmt{
		Name:        name,
		Type:        typeName,
		Initializer: initializer,
	}
}

func (p *Parser) statement() ast.Node {
	expr := p.expression()
	if expr == nil {
		return nil
	}

	_, err := p.consume(lexer.SEMI_COLON_TOKEN, "Expect ';' after expression")
	if err != nil {
		return nil
	}

	return &ast.ExpressionStmt{
		Expression: expr,
	}
}
