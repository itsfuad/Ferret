package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
	"fmt"
	//"ferret/compiler/internal/lexer"
)

func parseFunctionDecl(p *Parser) ast.Expression {

	// consume the function token
	start := p.advance()

	name := ast.IdentifierExpr{}

	isAnonymous := false

	if p.match(lexer.IDENTIFIER_TOKEN) {
		token := p.advance()
		location := ast.Location{
			Start: &token.Start,
			End:   &token.End,
		}
		name = ast.IdentifierExpr{
			Name:     token.Value,
			Location: location,
		}
	} else {
		isAnonymous = true
	}

	var returnType ast.DataType
	params := []ast.IdentifierExpr{}
	paramTypes := []ast.DataType{}

	if !p.match(lexer.OPEN_PAREN) {
		token := p.peek()
		report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_OPEN_PAREN).AddHint("Add an opening parenthesis after the function name").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	p.advance() // consume the open parenthesis

	for !p.match(lexer.CLOSE_PAREN) {
		if p.match(lexer.IDENTIFIER_TOKEN) {
			token := p.advance()
			params = append(params, ast.IdentifierExpr{Name: token.Value, Location: ast.Location{Start: &token.Start, End: &token.End}})
			p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)
			paramType := parseType(p)
			if paramType == nil {
				token := p.peek()
				report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_PARAMETER_TYPE).AddHint("Add a type after the colon").SetLevel(report.SYNTAX_ERROR)
				return nil
			}
			paramTypes = append(paramTypes, paramType)
		} else {
			token := p.peek()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_PARAMETER_NAME).AddHint("Add an identifier after the function name").SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		if p.match(lexer.COMMA_TOKEN) {
			p.advance() // consume the comma
			// check if the next token is a close parenthesis
			if p.match(lexer.CLOSE_PAREN) {
				report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.SYNTAX_ERROR)
				return nil
			}
		}
	}

	p.consume(lexer.CLOSE_PAREN, report.EXPECTED_CLOSE_PAREN)

	// Parse return type if present
	if p.match(lexer.ARROW_TOKEN) {
		p.advance() // consume '->'
		returnType = parseType(p)
		if returnType == nil {
			token := p.peek()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_RETURN_TYPE).AddHint("Add a return type after the arrow").SetLevel(report.SYNTAX_ERROR)
			return nil
		}
	}

	fmt.Printf("Function name: %+v\n", name)
	fmt.Printf("Return type: %+v\n", returnType)
	fmt.Printf("Params: %+v\n", params)
	fmt.Printf("Param types: %+v\n", paramTypes)

	// Parse function body
	body := parseBlock(p)

	return &ast.FunctionDeclExpr{
		Identifier:  name,
		IsAnonymous: isAnonymous,
		Params:      params,
		ParamTypes:  paramTypes,
		ReturnType:  returnType,
		Body:        body,
		Location: ast.Location{
			Start: &start.Start,
			End:   body.EndPos(),
		},
	}
}
