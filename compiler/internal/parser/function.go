package parser

import (
	"ferret/compiler/colors"
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/internal/types"
	"ferret/compiler/internal/utils"
	"ferret/compiler/report"
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
			return parseMethodDeclaration(p, &start.Start, params)
		}
		// anonymous function
		return parseFunctionLiteral(p, &start.Start, true, false, params...)
	} else {
		// named function
		return parseFunctionDecl(p)
	}
}

func parseParameters(p *Parser) []ast.Parameter {

	params := []ast.Parameter{}

	p.consume(lexer.OPEN_PAREN, report.EXPECTED_OPEN_PAREN)

	for !p.match(lexer.CLOSE_PAREN) {

		identifier := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_PARAMETER_NAME)

		location := ast.Location{
			Start: &identifier.Start,
			End:   &identifier.End,
		}

		paramName := &ast.IdentifierExpr{Name: identifier.Value, Location: location}

		p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)

		paramType, ok := parseType(p)
		if !ok {
			token := p.peek()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_PARAMETER_TYPE).AddHint("Add a type after the colon").SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		param := ast.Parameter{
			Identifier: paramName,
			Type:       paramType,
		}

		//check if the parameter is already defined
		if utils.Has(params, param, func(p ast.Parameter, b ast.Parameter) bool {
			return p.Identifier.Name == b.Identifier.Name
		}) {
			report.Add(p.filePath, param.Identifier.StartPos().Line, param.Identifier.EndPos().Line, param.Identifier.StartPos().Column, param.Identifier.EndPos().Column, report.PARAMETER_REDEFINITION).AddHint("Parameter name already used").SetLevel(report.SEMANTIC_ERROR)
			return nil
		}

		params = append(params, param)

		//add to current scope
		paramSym := &symboltable.Symbol{
			Name:       param.Identifier.Name,
			SymbolKind: symboltable.PARAMETER_SYMBOL,
			Type:       param.Type.Type(),
			IsMutable:  false,
			Location: symboltable.SymbolLocation{
				File:   p.filePath,
				Line:   param.Identifier.StartPos().Line,
				Column: param.Identifier.StartPos().Column,
			},
		}

		if !p.currentScope.Define(paramSym) {
			report.ShowRedeclarationError(
				param.Identifier.Name,
				p.filePath,
				p.currentScope,
				param.Identifier.StartPos().Line,
				param.Identifier.EndPos().Line,
				param.Identifier.StartPos().Column,
				param.Identifier.EndPos().Column,
			)
			return nil
		}

		if p.match(lexer.CLOSE_PAREN) {
			break
		} else {
			comma := p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_PAREN)
			if p.match(lexer.CLOSE_PAREN) {
				report.Add(p.filePath, comma.Start.Line, comma.End.Line, comma.Start.Column, comma.End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.SYNTAX_ERROR)
				break
			}
		}
	}

	p.consume(lexer.CLOSE_PAREN, report.EXPECTED_CLOSE_PAREN)

	return params
}

func parseReturnTypes(p *Parser) []ast.DataType {
	p.advance()
	// Check for multiple return types in parentheses
	if p.peek().Kind != lexer.OPEN_PAREN {
		// Single return type
		returnType, ok := parseType(p)
		if !ok {
			token := p.previous()
			report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_RETURN_TYPE).AddHint("Add a return type after the arrow").SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		return []ast.DataType{returnType}
	}

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
			comma := p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_PAREN)
			if p.match(lexer.CLOSE_PAREN) {
				report.Add(p.filePath, comma.Start.Line, comma.End.Line, comma.Start.Column, comma.End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.WARNING)
				break
			}
		}
	}

	p.consume(lexer.CLOSE_PAREN, report.EXPECTED_CLOSE_PAREN)
	return returnTypes
}

func parseSignature(p *Parser, parseNewParams bool, params ...ast.Parameter) ([]ast.Parameter, []ast.DataType) {

	//scope must be a function scope
	if p.currentScope.ScopeKind() != symboltable.FUNCTION_SCOPE {
		report.Add(p.filePath, p.previous().Start.Line, p.previous().End.Line, p.previous().Start.Column, p.previous().End.Column, report.SCOPE_MISMATCH+": expected function scope").SetLevel(report.SYNTAX_ERROR)
		return nil, nil
	}

	if len(params) == 0 && parseNewParams {
		params = parseParameters(p)
	}

	// Parse return type if present
	if p.match(lexer.ARROW_TOKEN) {
		returnTypes := parseReturnTypes(p)
		return params, returnTypes
	}

	return params, nil
}

func parseFunctionLiteral(p *Parser, start *lexer.Position, isAnonymous, parseNewParams bool, params ...ast.Parameter) *ast.FunctionLiteral {

	if isAnonymous {
		p.enterScope(symboltable.FUNCTION_SCOPE)
		defer p.exitScope()
	}

	params, returnTypes := parseSignature(p, parseNewParams, params...)

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

func declareFunction(p *Parser) *ast.IdentifierExpr {

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

	return name
}

func parseFunctionDecl(p *Parser) ast.BlockConstruct {

	colors.BLUE.Println("Parsing function declaration")

	// consume the function token
	start := p.consume(lexer.FUNCTION_TOKEN, report.EXPECTED_FUNCTION_KEYWORD)

	name := declareFunction(p)

	//start a new scope
	p.enterScope(symboltable.FUNCTION_SCOPE)
	defer p.exitScope()

	function := parseFunctionLiteral(p, &start.Start, false, true)

	return &ast.FunctionDecl{
		Identifier: *name,
		Function:   function,
		Location: ast.Location{
			Start: &start.Start,
			End:   function.EndPos(),
		},
	}
}
