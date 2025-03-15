package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/internal/types"
	"ferret/compiler/report"
	"fmt"
)

// detect if it's a function or a method
//
// function: fn NAME (PARAMS) {BODY} // named
//
// function: fn (PARAMS) {BODY} // anonymous
//
// method: fn (r Receiver) NAME (PARAMS) {BODY}
//
// method: fn (r Receiver, others...) NAME (PARAMS) {BODY} // invalid, but we can still parse it and report an error
func parseFunctionLike(p *Parser) ast.Node {

	start := p.peek()

	var params []ast.Parameter

	if p.next().Kind == lexer.OPEN_PAREN {
		p.advance()
		// either a method or anonymous function
		// fn (PARAMS) {BODY} // anonymous
		// fn (PARAMS) NAME (PARAMS) {BODY} // method
		params = parseParameters(p)
		// if identifier, it's a method
		if p.match(lexer.IDENTIFIER_TOKEN) {
			fmt.Printf("Parsing as method\n")
			return parseMethodDeclaration(p, &start.Start, params)
		}

		fmt.Printf("Parsing as anonymous function\n")
		// anonymous function
		return parseFunctionLiteral(p, &start.Start, false, params...)
	} else {
		fmt.Printf("Parsing as named function\n")
		// named function
		return parseFunctionDecl(p)
	}
}

func parseParameters(p *Parser) []ast.Parameter {

	params := []ast.Parameter{}

	p.consume(lexer.OPEN_PAREN, report.EXPECTED_OPEN_PAREN)

	for !p.match(lexer.CLOSE_PAREN) {
		if p.match(lexer.IDENTIFIER_TOKEN) {
			token := p.advance() // consume the identifier

			location := ast.Location{
				Start: &token.Start,
				End:   &token.End,
			}

			iden := &ast.IdentifierExpr{Name: token.Value, Location: location}

			params = append(params, ast.Parameter{
				Identifier: iden,
			})

			p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)

			if paramType, ok := parseType(p); ok {
				params[len(params)-1].Type = paramType
			} else {
				token := p.peek()
				report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_PARAMETER_TYPE).AddHint("Add a type after the colon").SetLevel(report.SYNTAX_ERROR)
				return nil
			}
		} else {
			token := p.peek()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_PARAMETER_NAME).AddHint("Add an identifier after the function name").SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		if p.match(lexer.CLOSE_PAREN) {
			break
		} else if p.match(lexer.COMMA_TOKEN) && p.next().Kind != lexer.CLOSE_PAREN {
			p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_PAREN)
		} else {
			token := p.peek()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.SYNTAX_ERROR)
			return nil
		}
	}

	p.consume(lexer.CLOSE_PAREN, report.EXPECTED_CLOSE_PAREN)

	return params
}

func parseReturnTypes(p *Parser) []ast.DataType {
	p.advance()
	// Check for multiple return types in parentheses
	if p.peek().Kind == lexer.OPEN_PAREN {
		p.advance() // consume '('
		returnTypes := make([]ast.DataType, 0)

		for !p.match(lexer.CLOSE_PAREN) {
			returnType, ok := parseType(p)
			if !ok {
				token := p.previous()
				report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_RETURN_TYPE).AddHint("Add a return type after the arrow").SetLevel(report.SYNTAX_ERROR)
				return nil
			}
			returnTypes = append(returnTypes, returnType)

			if p.match(lexer.CLOSE_PAREN) {
				break
			} else if p.match(lexer.COMMA_TOKEN) && p.next().Kind != lexer.CLOSE_PAREN {
				p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_PAREN)
			} else {
				token := p.peek()
				report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_COMMA_OR_CLOSE_PAREN).AddHint("Add a comma after the return type").SetLevel(report.SYNTAX_ERROR)
				return nil
			}
		}

		p.consume(lexer.CLOSE_PAREN, report.EXPECTED_CLOSE_PAREN)
		return returnTypes
	} else {
		// Single return type
		returnType, ok := parseType(p)
		if !ok {
			token := p.previous()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_RETURN_TYPE).AddHint("Add a return type after the arrow").SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		return []ast.DataType{returnType}
	}
}

func parseSignature(p *Parser, parseParams bool, params ...ast.Parameter) ([]ast.Parameter, []ast.DataType) {

	if len(params) == 0 && parseParams {
		params = parseParameters(p)
	}

	// Add parameters to function scope
	for _, param := range params {
		sym := &symboltable.Symbol{
			Name:       param.Identifier.Name,
			SymbolKind: symboltable.PARAMETER_SYMBOL,
			Type:       param.Type.Type(),
			IsMutable:  true, // Parameters are mutable by default
			Location: symboltable.SymbolLocation{
				File:   p.filePath,
				Line:   param.Identifier.StartPos().Line,
				Column: param.Identifier.StartPos().Column,
			},
		}

		if !p.currentScope.Define(sym) {
			report.Add(p.filePath,
				param.Identifier.StartPos().Line,
				param.Identifier.EndPos().Line,
				param.Identifier.StartPos().Column,
				param.Identifier.EndPos().Column,
				"Parameter name already used").SetLevel(report.SEMANTIC_ERROR)
			return nil, nil
		}
	}

	// Parse return type if present
	if p.match(lexer.ARROW_TOKEN) {
		returnTypes := parseReturnTypes(p)
		return params, returnTypes
	}

	return params, nil
}

func parseFunctionLiteral(p *Parser, start *lexer.Position, parseParams bool, params ...ast.Parameter) *ast.FunctionLiteral {

	//create new scope
	p.enterScope(symboltable.FUNCTION_SCOPE)
	defer p.exitScope()

	params, returnTypes := parseSignature(p, parseParams, params...)

	block := parseBlock(p)

	location := ast.Location{
		Start: start,
		End:   block.EndPos(),
	}

	return &ast.FunctionLiteral{
		Params:     params,
		ReturnType: returnTypes,
		Body:       &block,
		Location:   location,
	}
}

func parseFunctionDecl(p *Parser) ast.BlockConstruct {

	// consume the function token
	start := p.advance()

	var name *ast.IdentifierExpr

	if p.match(lexer.IDENTIFIER_TOKEN) {
		token := p.advance()
		location := ast.Location{
			Start: &token.Start,
			End:   &token.End,
		}
		name = &ast.IdentifierExpr{
			Name:     token.Value,
			Location: location,
		}

		// Add function to current scope's symbol table
		sym := &symboltable.Symbol{
			Name:       name.Name,
			SymbolKind: symboltable.FUNCTION_SYMBOL,
			Type:       types.FUNCTION,
			IsMutable:  false,
			Location: symboltable.SymbolLocation{
				File:   p.filePath,
				Line:   name.StartPos().Line,
				Column: name.StartPos().Column,
			},
		}

		if !p.currentScope.Define(sym) {
			report.ShowRedeclarationError(
				name.Name,
				p.filePath,
				p.currentScope,
				name.StartPos().Line,
				name.EndPos().Line,
				name.StartPos().Column,
				name.EndPos().Column,
			)
			return nil
		}
	}

	function := parseFunctionLiteral(p, &start.Start, true)

	return &ast.FunctionDecl{
		Identifier: *name,
		Function:   function,
		Location: ast.Location{
			Start: &start.Start,
			End:   function.EndPos(),
		},
	}
}
