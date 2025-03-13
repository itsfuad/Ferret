package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
)

func parseParameters(p *Parser) []ast.Parameter {

	params := []ast.Parameter{}

	p.consume(lexer.OPEN_PAREN, report.EXPECTED_OPEN_PAREN)

	for !p.match(lexer.CLOSE_PAREN) {
		if p.match(lexer.IDENTIFIER_TOKEN) {
			token := p.advance()

			location := ast.Location{
				Start: &token.Start,
				End:   &token.End,
			}

			params = append(params, ast.Parameter{
				Identifier: &ast.IdentifierExpr{Name: token.Value, Location: location},
			})

			p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)
			paramType, ok := parseType(p)
			if !ok {
				token := p.peek()
				report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_PARAMETER_TYPE).AddHint("Add a type after the colon").SetLevel(report.SYNTAX_ERROR)
				return nil
			}

			params[len(params)-1].Type = paramType

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

	return params
}

func parseSignature(p *Parser) ([]ast.Parameter, ast.DataType) {

	if !p.match(lexer.OPEN_PAREN) {
		token := p.peek()
		report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_OPEN_PAREN).AddHint("Add an opening parenthesis after the function name").SetLevel(report.SYNTAX_ERROR)
		return nil, nil
	}

	params := parseParameters(p)

	// Parse return type if present
	if p.match(lexer.ARROW_TOKEN) {
		p.advance() // consume '->'
		returnType, ok := parseType(p)
		if !ok {
			token := p.previous()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_RETURN_TYPE).AddHint("Add a return type after the arrow").SetLevel(report.SYNTAX_ERROR)
			return nil, nil
		}
		return params, returnType
	}

	return params, nil
}

func parseFunctionLiteral(p *Parser, start *lexer.Position) *ast.FunctionLiteral {

	var returnType ast.DataType

	params, returnType := parseSignature(p)

	block := parseBlock(p)

	location := ast.Location{
		Start: start,
		End:   block.EndPos(),
	}

	return &ast.FunctionLiteral{
		Params:     params,
		ReturnType: returnType,
		Body:       &block,
		Location:   location,
	}
}

func parseFunctionDecl(p *Parser) ast.BlockConstruct {

	// consume the function token
	start := p.advance()

	name := ast.IdentifierExpr{}

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
	}

	function := parseFunctionLiteral(p, &start.Start)

	return &ast.FunctionDeclExpr{
		Identifier: name,
		Function:   function,
		Location: ast.Location{
			Start: &start.Start,
			End:   function.EndPos(),
		},
	}
}
