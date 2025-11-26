package parser

import (
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/source"
	"compiler/internal/utils"
)

// parseVarDecl: let x := 10; or let x: i32 = 10;
func (p *Parser) parseVarDecl() *ast.VarDecl {
	start := p.advance().Start
	decls, location := p.parseVariableDeclaration(start, false)
	return &ast.VarDecl{
		Decls:    decls,
		Location: location,
	}
}

// parseConstDecl: const pi := 3.14;
func (p *Parser) parseConstDecl() *ast.ConstDecl {
	start := p.peek().Start
	p.expect(lexer.CONST_TOKEN)

	decls, location := p.parseVariableDeclaration(start, true)

	return &ast.ConstDecl{
		Decls:    decls,
		Location: location,
	}
}

// parseVariableDeclaration: unified parser for let/const declarations
// Returns the declarations and location
func (p *Parser) parseVariableDeclaration(start source.Position, isConst bool) ([]ast.DeclItem, source.Location) {
	var decls []ast.DeclItem

	// can be let a : <type> = <expr>; or let a := <expr>; or let a: <type>;
	// or list of variables, let a : i32, b := 10, c: str;
	// for const, initializer is mandatory

	for !p.isAtEnd() && !p.match(lexer.SEMICOLON_TOKEN) {
		decl := p.parseDeclItem(isConst)
		decls = append(decls, decl)

		if !p.match(lexer.COMMA_TOKEN) {
			break
		}
		p.advance()
	}

	p.expect(lexer.SEMICOLON_TOKEN)

	return decls, p.makeLocation(start)
}

// parseDeclItem parses a single declaration item (name, optional type, initializer)
// extracted to reduce cognitive complexity in parseVariableDeclaration.
func (p *Parser) parseDeclItem(isConst bool) ast.DeclItem {
	var decl ast.DeclItem

	decl.Name = p.parseIdentifier()

	if p.match(lexer.COLON_TOKEN) {
		p.advance()
		decl.Type = p.parseType()

		// Check for initializer
		if p.match(lexer.EQUALS_TOKEN) {
			p.advance()
			decl.Value = p.parseExpr()
		} else if isConst {
			// Constants MUST have an initializer
			start := decl.Name.Start
			end := decl.Name.End

			start.Line -= 2
			start.Column = 1
			end.Line -= 2
			end.Column = 1
			peek := p.peek()
			p.diagnostics.Add(
				diagnostics.NewError("missing values for constant").
					WithCode(diagnostics.ErrUnexpectedToken).
					WithNote("Constants must have an initializer").
					WithPrimaryLabel(p.filepath, source.NewLocation(&peek.Start, &peek.End), "add initial value").
					WithSecondaryLabel(p.filepath, source.NewLocation(start, end), "Hehe"),
			)
			// Continue parsing to gather more errors
		}
		// For variables, no initializer is OK (decl.Value remains nil)
		return decl
	}

	if p.match(lexer.WALRUS_TOKEN) {
		p.advance()
		decl.Value = p.parseExpr()
		return decl
	}

	if p.match(lexer.EQUALS_TOKEN) {
		// Allow the parsing but report an error
		loc := p.makeLocation(p.advance().Start)
		decl.Value = p.parseExpr()
		p.diagnostics.Add(
			diagnostics.NewError("expected ':=' for inferred symbols with values").
				WithCode(diagnostics.ErrUnexpectedToken).
				WithPrimaryLabel(p.filepath, &loc, "add a `:` before `=`"),
		)
		return decl
	}

	if isConst {
		p.error("expected ':' or ':=' in constant declaration")
	} else {
		p.error("expected ':' or ':=' in variable declaration")
	}

	return decl
}

// parseTypeDecl: type Point struct { .x: i32 };
func (p *Parser) parseTypeDecl() *ast.TypeDecl {

	start := p.expect(lexer.TYPE_TOKEN).Start

	name := p.parseIdentifier()
	typ := p.parseType()

	p.expect(lexer.SEMICOLON_TOKEN)

	return &ast.TypeDecl{
		Name:     name,
		Type:     typ,
		Location: *source.NewLocation(&start, typ.Loc().End),
	}
}

// parseFuncDecl: fn add(a: i32) -> i32 { return a; }
// This is the entry point for all function-like declarations:
// - Named functions: fn add(a: i32) -> i32 { ... }
// - Anonymous functions: fn (a: i32) -> i32 { ... }
// - Methods: fn (r Rect) area() -> f64 { ... }
func (p *Parser) parseFuncDecl() ast.Node {

	start := p.expect(lexer.FUNCTION_TOKEN).Start

	// Lookahead to determine what kind of function this is
	if p.match(lexer.OPEN_PAREN) {
		// Could be: fn (params) { body } -- anonymous function
		// Or:       fn (receiver) methodName(params) { body } -- method

		params := p.parseFunctionParams()

		p.expect(lexer.CLOSE_PAREN)

		// If next token is identifier, it's a method
		if p.match(lexer.IDENTIFIER_TOKEN) {
			return p.parseMethodDecl(start, params)
		}

		// Otherwise, it's an anonymous function (function literal)
		return p.parseFuncLit(start, params)
	}

	// Named function: fn name(params) -> return { body }
	return p.parseNamedFuncDecl(start)
}

// parseFunctionParams parses function parameters: name: type, name: type, ...
// Used for both regular parameters and method receivers
func (p *Parser) parseFunctionParams() []ast.Field {

	p.advance() // consume '('

	params := []ast.Field{}

	if p.match(lexer.CLOSE_PAREN) {
		return params
	}

	for !(p.match(lexer.CLOSE_PAREN) || p.isAtEnd()) {

		// Parse parameter name
		name := p.parseIdentifier()

		// Expect colon
		if !p.match(lexer.COLON_TOKEN) {
			p.error("expected ':' after parameter name")
			break
		}

		p.expect(lexer.COLON_TOKEN)

		// Check for variadic parameter (...)
		isVariadic := false
		if p.match(lexer.THREE_DOT_TOKEN) {
			isVariadic = true
			p.advance() // consume '...'
		}

		// Parse parameter type
		paramType := p.parseType()

		// Handle nil paramType
		var endPos *source.Position
		if paramType != nil && paramType.Loc() != nil {
			endPos = paramType.Loc().End
		} else {
			endPos = name.End
		}

		param := ast.Field{
			Name:       name,
			Type:       paramType,
			IsVariadic: isVariadic,
			Location:   *source.NewLocation(name.Start, endPos),
		}

		params = append(params, param)

		if p.match(lexer.CLOSE_PAREN) {
			break
		}

		if p.checkTrailing(lexer.COMMA_TOKEN, lexer.CLOSE_PAREN, "function parameters") {
			p.advance() // skip the token
			break
		}

		p.expect(lexer.COMMA_TOKEN)
	}

	// Note: Don't consume CLOSE_PAREN here - let the caller handle it
	// This keeps the behavior consistent with the early return case (line 157)

	return params
}

// parseNamedFuncDecl parses a named function declaration: fn name(params) -> return { body }
func (p *Parser) parseNamedFuncDecl(start source.Position) *ast.FuncDecl {
	name := p.parseIdentifier()

	// Parse function type (parameters and return type)
	funcType := p.parseFuncType(start)
	// Parse body
	var body *ast.Block
	if p.match(lexer.OPEN_CURLY) {
		body = p.parseBlock()
	}

	return &ast.FuncDecl{
		Name:     name,
		Type:     funcType,
		Body:     body,
		Location: *source.NewLocation(&start, body.End),
	}
}

// parseFuncLit parses an anonymous function: fn (params) -> return { body }
func (p *Parser) parseFuncLit(start source.Position, params []ast.Field) *ast.FuncLit {
	// Create FieldList from params
	//paramList := []ast.Field{*params}
	paramList := params

	// Parse return type if present
	var result ast.TypeNode
	if p.match(lexer.ARROW_TOKEN) {
		p.advance()
		typenode := p.parseType()
		result = typenode
	}

	funcType := &ast.FuncType{
		Params:   paramList,
		Result:   result,
		Location: p.makeLocation(start),
	}

	// Parse body
	body := p.parseBlock()

	return &ast.FuncLit{
		ID: ast.IdentifierExpr{
			Name: utils.GenerateFuncLitID(),
			Location: source.Location{
				Start: &start,
				End:   &start,
			},
		},
		Type:     funcType,
		Body:     body,
		Location: p.makeLocation(start),
	}
}

// parseMethodDecl parses a method declaration: fn (receiver) methodName(params) -> return { body }
func (p *Parser) parseMethodDecl(start source.Position, receivers []ast.Field) *ast.MethodDecl {
	// Method name
	methodName := p.parseIdentifier()

	// Validate receiver count
	if len(receivers) == 0 {
		p.diagnostics.Add(
			diagnostics.NewError("method must have a receiver").
				WithCode(diagnostics.ErrUnexpectedToken).
				WithPrimaryLabel(p.filepath, methodName.Loc(), "method declared without receiver"),
		)
		return nil
	}

	if len(receivers) > 1 {
		secondReceiver := receivers[1]
		p.diagnostics.Add(
			diagnostics.NewError("method can only have one receiver").
				WithCode(diagnostics.ErrUnexpectedToken).
				WithPrimaryLabel(p.filepath, secondReceiver.Loc(), "extra receiver"),
		)
	}

	// Parse method signature (parameters and return type)
	funcType := p.parseFuncType(start)

	// Parse body
	body := p.parseBlock()

	return &ast.MethodDecl{
		Receiver: &receivers[0],
		Name:     methodName,
		Type:     funcType,
		Body:     body,
		Location: *source.NewLocation(&start, body.End),
	}
}
