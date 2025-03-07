package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
	"fmt"
)

func parseVarDecl(p *Parser) ast.Node {
	fmt.Println("Parsing variable declaration")
	kind := p.advance()
	isConst := kind.Kind == lexer.CONST_TOKEN

	variables := make([]*ast.VariableDecl, 0)
	types := make([]ast.DataType, 0)
	values := make([]ast.Expression, 0)
	varCount := 0

	// Parse variable names
	for {
		identifierName := p.consume(lexer.IDENTIFIER_TOKEN, "Expected variable name")
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

	// Parse types if present
	if p.peek().Kind == lexer.COLON_TOKEN {
		p.advance()
		for {
			typeNode := parseType(p)
			if typeNode == nil {
				report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Expected type after ':'").SetLevel(report.SYNTAX_ERROR)
				return nil
			}
			types = append(types, typeNode)

			if p.peek().Kind != lexer.COMMA_TOKEN {
				break
			}
			p.advance()
		}
	}

	// Assign types to variables
	if len(types) == 1 {
		for i := range variables {
			variables[i].Type = types[0]
		}
	} else if len(types) == varCount {
		for i := range variables {
			variables[i].Type = types[i]
		}
	} else if len(types) > 0 {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Mismatched variable and type count").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	// Parse assignment if '=' is found
	if p.peek().Kind == lexer.EQUALS_TOKEN {
		p.advance()
		for {
			value := parseExpression(p)
			if value == nil {
				report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Expected value after '='").SetLevel(report.SYNTAX_ERROR)
				return nil
			}
			values = append(values, value)

			if p.peek().Kind != lexer.COMMA_TOKEN {
				break
			}
			p.advance()
		}
	}

	// Check if variable count matches value count
	if len(values) == 1 && varCount > 1 {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Mismatched variable and value count: Single value cannot be assigned to multiple variables").SetLevel(report.SYNTAX_ERROR)
		return nil
	} else if len(values) != 0 && len(values) != varCount {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, fmt.Sprintf("Mismatched variable and value count: Expected %d values, got %d", varCount, len(values))).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	// Assign values to variables
	if len(values) == varCount {
		for i := range variables {
			variables[i].Initializer = values[i]
		}
	}

	return &ast.VarDeclStmt{
		Variables: variables,
	}
}

func parseExpression(p *Parser) ast.Expression {
	switch p.peek().Kind {
	case lexer.OPEN_BRACKET:
		return parseArrayLiteral(p)
	case lexer.NUMBER_TOKEN:
		return parseNumberLiteral(p)
	}
	return nil
}

func parseArrayLiteral(p *Parser) ast.Expression {
	start := p.advance().Start // consume '['
	elements := make([]ast.Expression, 0)

	for !p.check(lexer.CLOSE_BRACKET) {
		expr := parseExpression(p)
		if expr == nil {
			return nil
		}
		elements = append(elements, expr)

		if p.peek().Kind == lexer.COMMA_TOKEN {
			p.advance() // consume ','
		}
	}

	end := p.advance().End // consume ']'
	return &ast.ArrayLiteralExpr{
		Elements: elements,
		Location: ast.Location{
			Start: start,
			End:   end,
		},
	}
}
