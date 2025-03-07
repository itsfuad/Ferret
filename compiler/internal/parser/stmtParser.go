package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
	"fmt"
)

func parseIdentifiers(p *Parser) ([]*ast.VariableDecl, int) {
	variables := make([]*ast.VariableDecl, 0)
	varCount := 0
	isConst := p.previous().Kind == lexer.CONST_TOKEN

	for {
		if !p.check(lexer.IDENTIFIER_TOKEN) {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Expected variable name").SetLevel(report.SYNTAX_ERROR)
			return nil, 0
		}
		identifierName := p.advance()
		identifier := &ast.IdentifierExpr{
			Name: identifierName.Value,
			Location: ast.Location{
				Start: identifierName.Start,
				End:   identifierName.End,
			},
		}
		variables = append(variables, &ast.VariableDecl{
			Identifier: identifier,
			IsConst:    isConst,
			Location:   identifier.Location,
		})
		varCount++

		if p.peek().Kind != lexer.COMMA_TOKEN {
			break
		}
		p.advance()
	}
	return variables, varCount
}

func parseTypeAnnotations(p *Parser) ([]ast.DataType, bool) {
	if p.peek().Kind != lexer.COLON_TOKEN {
		return nil, true
	}

	p.advance()
	types := make([]ast.DataType, 0)
	for {
		typeNode := parseType(p)
		if typeNode == nil {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Expected type after ':'").SetLevel(report.SYNTAX_ERROR)
			return nil, false
		}
		types = append(types, typeNode)

		if p.peek().Kind != lexer.COMMA_TOKEN {
			break
		}
		p.advance()
	}
	return types, true
}

func parseInitializers(p *Parser) ([]ast.Expression, bool) {
	if p.peek().Kind != lexer.EQUALS_TOKEN {
		return nil, true
	}

	p.advance()
	values := make([]ast.Expression, 0)
	for {
		value := parseExpression(p)
		if value == nil {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Expected value after '='").SetLevel(report.SYNTAX_ERROR)
			return nil, false
		}
		values = append(values, value)

		if p.peek().Kind != lexer.COMMA_TOKEN {
			break
		}
		p.advance()
	}
	return values, true
}

func assignTypes(p *Parser, variables []*ast.VariableDecl, types []ast.DataType, varCount int) bool {
	if len(types) == 0 {
		return true
	}
	if len(types) == 1 {
		for i := range variables {
			variables[i].Type = types[0]
		}
		return true
	}
	if len(types) == varCount {
		for i := range variables {
			variables[i].Type = types[i]
		}
		return true
	}
	report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Mismatched variable and type count").SetLevel(report.SYNTAX_ERROR)
	return false
}

func assignValues(p *Parser, variables []*ast.VariableDecl, values []ast.Expression, varCount int) bool {
	if len(values) == 0 {
		return true
	}
	if len(values) == 1 && varCount > 1 {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Single value cannot be assigned to multiple variables").SetLevel(report.SYNTAX_ERROR)
		return false
	}
	if len(values) != varCount {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, fmt.Sprintf("Expected %d values, got %d", varCount, len(values))).SetLevel(report.SYNTAX_ERROR)
		return false
	}
	for i := range variables {
		variables[i].Initializer = values[i]
	}
	return true
}

func parseVarDecl(p *Parser) ast.Node {
	fmt.Println("Parsing variable declaration")
	p.advance() // consume let/const

	variables, varCount := parseIdentifiers(p)
	if variables == nil {
		return nil
	}

	types, ok := parseTypeAnnotations(p)
	if !ok || !assignTypes(p, variables, types, varCount) {
		return nil
	}

	values, ok := parseInitializers(p)
	if !ok || !assignValues(p, variables, values, varCount) {
		return nil
	}

	p.consume(lexer.SEMI_COLON_TOKEN, "Expected ';' after variable declaration")

	return &ast.VarDeclStmt{
		Variables: variables,
	}
}
