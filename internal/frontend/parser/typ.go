package parser

import (
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/source"
	"compiler/internal/tokens"
	"compiler/internal/utils"
	"fmt"
)

// parseType parses a type expression
func (p *Parser) parseType() ast.TypeNode {
	tok := p.peek()

	// Check for reference type &T
	if p.match(tokens.BIT_AND_TOKEN) {
		ampersand := p.advance()
		baseType := p.parseType()
		// Handle case where baseType might be Invalid with potential nil location issues
		var endPos *source.Position
		if baseType != nil && baseType.Loc() != nil && baseType.Loc().End != nil {
			endPos = baseType.Loc().End
		} else {
			endPos = &ampersand.End
		}
		return &ast.ReferenceType{
			Base:     baseType,
			Location: *source.NewLocation(&p.filepath, &ampersand.Start, endPos),
		}
	}

	var t ast.TypeNode

	switch tok.Kind {
	case tokens.IDENTIFIER_TOKEN:
		// Type identifier - convert IdentifierExpr to support both Expr() and TypeExpr()
		ident := p.parseIdentifier()
		t = ident
		// Check for scope resolution (module::Type)
		if p.match(tokens.SCOPE_TOKEN) {
			// IdentifierExpr implements both Expression and TypeNode
			t = p.parseScopeResolutionExpr(ident)
		}

	case tokens.OPEN_BRACKET:
		t = p.parseArrayType()

	case tokens.STRUCT_TOKEN:
		t = p.parseStructType()

	case tokens.INTERFACE_TOKEN:
		t = p.parseInterfaceType()

	case tokens.ENUM_TOKEN:
		t = p.parseEnumType()

	case tokens.MAP_TOKEN:
		t = p.parseMapType()

	case tokens.FUNCTION_TOKEN:
		start := p.advance().Start
		t = p.parseFuncType(start)

	default:
		//p.error(fmt.Sprintf("expected type, got %s", tok.Value))
		p.diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("expected type, got %s", tok.Value)).
				WithCode(diagnostics.ErrMissingType).
				WithPrimaryLabel(source.NewLocation(&p.filepath, &tok.Start, &tok.End), fmt.Sprintf("expected type here, got `%s`", tok.Value)),
		)
		// Return a placeholder identifier type instead of recursing
		// This prevents stack overflow on invalid syntax
		return &ast.Invalid{
			Location: p.makeLocation(tok.Start),
		}
	}

	// Check for optional type T?
	if p.match(tokens.QUESTION_TOKEN) {
		p.advance()
		t = &ast.OptionalType{
			Base:     t,
			Location: *t.Loc(),
		}
	}

	// Check for error type T ! E or T!
	// Supports two syntaxes:
	//   1. T ! E - explicit error type (e.g., i32 ! str, User ! HttpError)
	//   2. T!     - default error type (defaults to 'str')
	// The default can be changed later to Error struct or same type as success type
	// Note: ErrorType always has exactly two types: Value (success) and Error
	if p.match(tokens.NOT_TOKEN) {
		notToken := p.advance() // consume '!'

		var resultType ast.TypeNode
		var endPos *source.Position

		// Check if error type is provided (T ! E) or use default (T!)
		// Default error type is 'str' for now, but this can be changed later
		// to Error struct or same type as success type
		if !p.match(tokens.SEMICOLON_TOKEN, tokens.CLOSE_PAREN, tokens.COMMA_TOKEN, tokens.OPEN_CURLY) {
			// Error type is explicitly provided
			resultType = p.parseType()
			if resultType != nil && resultType.Loc() != nil {
				endPos = resultType.Loc().End
			} else {
				endPos = &notToken.End
			}
		} else {
			// No error type provided, use default 'str'
			resultType = &ast.IdentifierExpr{
				Name:     "str",
				Location: *source.NewLocation(&p.filepath, &notToken.End, &notToken.End),
			}
			endPos = &notToken.End
		}

		t = &ast.ResultType{
			Value:    t,
			Error:    resultType,
			Location: *source.NewLocation(&p.filepath, t.Loc().Start, endPos),
		}
	}

	return t
}

func (p *Parser) parseArrayType() *ast.ArrayType {

	tok := p.expect(tokens.OPEN_BRACKET)

	var size ast.Expression
	if !p.match(tokens.CLOSE_BRACKET) {
		size = p.parseExpr()
	}

	p.expect(tokens.CLOSE_BRACKET)

	elem := p.parseType()

	return &ast.ArrayType{
		Len:      size, // nil for dynamic arrays []T
		ElType:   elem,
		Location: *source.NewLocation(&p.filepath, &tok.Start, elem.Loc().End),
	}
}

func (p *Parser) parseStructType() *ast.StructType {

	tok := p.expect(tokens.STRUCT_TOKEN)
	p.expect(tokens.OPEN_CURLY)

	fields := []ast.Field{}

	for !(p.match(tokens.CLOSE_CURLY) || p.isAtEnd()) {
		// Error recovery: Check if we have a dot token
		if !p.match(tokens.DOT_TOKEN) {
			p.error(fmt.Sprintf("expected . for struct field, got %s", p.peek().Value))
			p.advance() // Advance to prevent infinite loop
			continue
		}

		p.expect(tokens.DOT_TOKEN)
		name := p.parseIdentifier()
		p.expect(tokens.COLON_TOKEN)
		typ := p.parseType()

		fields = append(fields, ast.Field{
			Name: name,
			Type: typ,
		})

		if p.match(tokens.CLOSE_CURLY) {
			break
		}

		if p.checkTrailing(tokens.COMMA_TOKEN, tokens.CLOSE_CURLY, "struct type") {
			p.advance() // skip the token
			break
		}

		p.expect(tokens.COMMA_TOKEN)
	}

	end := p.expect(tokens.CLOSE_CURLY)

	structType := &ast.StructType{
		Fields:   fields,
		ID: utils.GenerateStructLitID(),
		Location: *source.NewLocation(&p.filepath, &tok.Start, &end.End),
	}

	return structType
}

func (p *Parser) parseFuncType(start source.Position) *ast.FuncType {

	p.expect(tokens.OPEN_PAREN)

	var params []ast.Field

	if p.match(tokens.CLOSE_PAREN) {
		p.advance()
	} else {
		params = parseParams(p)
	}

	var result ast.TypeNode
	if p.match(tokens.ARROW_TOKEN) {
		p.advance()
		result = p.parseType()
	}

	return &ast.FuncType{
		Params:   params,
		Result:   result,
		Location: p.makeLocation(start),
	}
}

func parseParams(p *Parser) []ast.Field {

	params := []ast.Field{}

	for !(p.match(tokens.CLOSE_PAREN) || p.isAtEnd()) {

		name := p.parseIdentifier()
		p.expect(tokens.COLON_TOKEN)

		// Check for variadic parameter (...)
		isVariadic := false
		if p.match(tokens.THREE_DOT_TOKEN) {
			isVariadic = true
			p.advance() // consume '...'
		}

		typ := p.parseType()

		// Handle nil type
		var endPos *source.Position
		if typ != nil && typ.Loc() != nil {
			endPos = typ.Loc().End
		} else {
			endPos = name.End
		}

		params = append(params, ast.Field{
			Name:       name,
			Type:       typ,
			IsVariadic: isVariadic,
			Location:   *source.NewLocation(&p.filepath, name.Start, endPos),
		})

		if p.match(tokens.CLOSE_PAREN) {
			break
		}

		if p.checkTrailing(tokens.COMMA_TOKEN, tokens.CLOSE_PAREN, "function parameters") {
			p.advance() // skip the token
			break
		}

		p.expect(tokens.COMMA_TOKEN)
	}

	p.expect(tokens.CLOSE_PAREN)

	return params
}

func (p *Parser) parseInterfaceType() *ast.InterfaceType {

	tok := p.advance()

	p.expect(tokens.OPEN_CURLY)

	methods := []ast.Field{}

	for !(p.match(tokens.CLOSE_CURLY) || p.isAtEnd()) {

		// funcname (...) -> ...

		name := p.parseIdentifier()

		functype := p.parseFuncType(*name.Start)

		methods = append(methods, ast.Field{
			Name:     name,
			Type:     functype,
			Location: *source.NewLocation(&p.filepath, name.Start, functype.End),
		})

		if p.match(tokens.CLOSE_CURLY) {
			break
		}

		if p.checkTrailing(tokens.SEMICOLON_TOKEN, tokens.CLOSE_CURLY, "interface type") {
			p.advance() // skip the token
			break
		}

		p.expect(tokens.COMMA_TOKEN)
	}

	end := p.expectError(tokens.CLOSE_CURLY, fmt.Sprintf("expected '}' to close interface type, found %s", p.peek().Kind)).End

	return &ast.InterfaceType{
		Methods:  methods,
		ID: utils.GenerateInterfaceLitID(),
		Location: *source.NewLocation(&p.filepath, &tok.Start, &end),
	}
}

func (p *Parser) parseEnumType() *ast.EnumType {

	tok := p.expect(tokens.ENUM_TOKEN)

	p.expect(tokens.OPEN_CURLY)

	fields := []ast.Field{}

	for !(p.match(tokens.CLOSE_CURLY) || p.isAtEnd()) {
		// Error recovery: Check if we have an identifier
		if !p.match(tokens.IDENTIFIER_TOKEN) {
			p.error(fmt.Sprintf("expected enum variant name, got %s", p.peek().Value))
			p.advance() // Advance to prevent infinite loop
			continue
		}

		name := p.parseIdentifier()
		fields = append(fields, ast.Field{
			Name: name,
		})

		if p.match(tokens.CLOSE_CURLY) {
			break
		}

		// Check for trailing comma before closing brace
		if p.checkTrailing(tokens.COMMA_TOKEN, tokens.CLOSE_CURLY, "enum type") {
			p.advance() // skip the token
			break
		}

		p.expect(tokens.COMMA_TOKEN)
	}

	p.expectError(tokens.CLOSE_CURLY, fmt.Sprintf("expected '}' to close enum type, found %s", p.peek().Kind))

	return &ast.EnumType{
		Variants: fields,
		ID: utils.GenerateEnumLitID(),
		Location: p.makeLocation(tok.Start),
	}
}

func (p *Parser) parseMapType() *ast.MapType {

	tok := p.advance()

	p.expect(tokens.OPEN_BRACKET)

	keyType := p.parseType()

	p.expect(tokens.CLOSE_BRACKET)

	valueType := p.parseType()

	return &ast.MapType{
		Key:      keyType,
		Value:    valueType,
		Location: p.makeLocation(tok.Start),
	}
}
