package parser

import (
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/source"
	"fmt"
)

// The Parser builds an AST from a token stream.
// The ParseFile function is in the pipeline package to avoid import cycles.

// Parser holds temporary state during parsing of a single file.
// This is created on-the-fly, not stored persistently.
type Parser struct {
	tokens        []lexer.Token
	current       int // current position in tokens
	diagnostics   *diagnostics.DiagnosticBag
	filepath      string
	seenNonImport bool // Track if we've seen any non-import declaration
}

// Parse is the internal parsing function called by the pipeline.
// It takes tokens and diagnostics as parameters to avoid import cycles.
func Parse(tokens []lexer.Token, filepath string, diag *diagnostics.DiagnosticBag) *ast.Module {
	parser := &Parser{
		tokens:        tokens,
		current:       0,
		diagnostics:   diag,
		filepath:      filepath,
		seenNonImport: false,
	}

	return parser.parseModule()
}

// parseModule parses the entire module (all top-level declarations)
func (p *Parser) parseModule() *ast.Module {
	module := &ast.Module{
		FullPath: p.filepath,
		Nodes:    []ast.Node{},
	}

	for !p.isAtEnd() {
		node := p.parseTopLevel()
		if node != nil {
			module.Nodes = append(module.Nodes, node)
		}
	}

	return module
}

// parseTopLevel parses a single top-level declaration
func (p *Parser) parseTopLevel() ast.Node {
	tok := p.peek()
	switch tok.Kind {
	case lexer.IMPORT_TOKEN:
		imp := p.parseImport()
		// Validate: imports must come before other declarations
		if p.seenNonImport {
			p.diagnostics.Add(
				diagnostics.NewError("import statements must appear before other declarations").
				WithPrimaryLabel(p.filepath, source.NewLocation(imp.Location.Start, imp.Location.End), "cannot import here"),
			)
			return nil
		}
		return imp
	case lexer.LET_TOKEN:
		p.seenNonImport = true
		return p.parseVarDecl()
	case lexer.CONST_TOKEN:
		p.seenNonImport = true
		return p.parseConstDecl()
	case lexer.TYPE_TOKEN:
		p.seenNonImport = true
		return p.parseTypeDecl()
	case lexer.IF_TOKEN:
		p.seenNonImport = true
		return p.parseIfStmt()
	case lexer.FUNCTION_TOKEN:
		p.seenNonImport = true
		return p.parseFuncDecl()
	case lexer.IDENTIFIER_TOKEN:
		p.seenNonImport = true
		return p.parseExprOrAssign()
	// Allow anonymous type declarations at top-level, e.g.:
	// struct { .name: str, .age: i32, };
	// interface { method1(...), ...}
	// enum { A, B, C }
	// We'll parse the type and return a TypeDecl with a nil Name.
	case lexer.STRUCT_TOKEN, lexer.INTERFACE_TOKEN, lexer.ENUM_TOKEN:
		p.seenNonImport = true
		return p.parseAnnonType()
	default:
		p.error(fmt.Sprintf("unexpected token at top level: %s", tok.Value))
		p.advance()
		return nil
	}
}

func (p *Parser) parseAnnonType() *ast.TypeDecl {
	
	typ := p.parseType()

	p.diagnostics.Add(
		diagnostics.NewWarning("annonymous type defined").
		WithPrimaryLabel(p.filepath, typ.Loc(), "remove this type").
		WithNote("types has to be declared with a name to be used"),
	)

	p.expect(lexer.SEMICOLON_TOKEN)


	return &ast.TypeDecl{
		Name:     &ast.IdentifierExpr{Name: "<anonymous>"},
		Type:     typ,
		Location: *source.NewLocation(typ.Loc().Start, typ.Loc().End),
	}
}

// parseImport: import "path";
func (p *Parser) parseImport() *ast.ImportStmt {
	start := p.peek().Start

	p.expect(lexer.IMPORT_TOKEN)

	if !p.match(lexer.STRING_TOKEN) {
		p.error("expected string literal after 'import'")
		return nil
	}

	pathTok := p.advance()

	path := &ast.BasicLit{
		Kind:     ast.STRING,
		Value:    pathTok.Value,
		Location: *source.NewLocation(&pathTok.Start, &pathTok.End),
	}

	p.expect(lexer.SEMICOLON_TOKEN)

	return &ast.ImportStmt{
		Path:     path,
		Location: p.makeLocation(start),
	}
}

// parseStmt parses a statement
func (p *Parser) parseStmt() ast.Node {
	tok := p.peek()
	switch tok.Kind {
	case lexer.LET_TOKEN:
		return p.parseVarDecl()
	case lexer.CONST_TOKEN:
		return p.parseConstDecl()
	case lexer.RETURN_TOKEN:
		return p.parseReturnStmt()
	case lexer.IF_TOKEN:
		return p.parseIfStmt()
	case lexer.OPEN_CURLY:
		return p.parseBlock()
	default:
		return p.parseExprOrAssign()
	}
}

// parseReturnStmt: return expr;
func (p *Parser) parseReturnStmt() *ast.ReturnStmt {

	start := p.expect(lexer.RETURN_TOKEN).Start

	var result ast.Expression
	var isError bool

	if !p.match(lexer.SEMICOLON_TOKEN) {
		result = p.parseExpr()
		if p.match(lexer.NOT_TOKEN) {
			p.advance()
			isError = true
		}
	}

	p.expect(lexer.SEMICOLON_TOKEN)

	return &ast.ReturnStmt{
		Result:   result,
		IsError:  isError,
		Location: *source.NewLocation(&start, result.Loc().End),
	}
}

// parseIfStmt: if cond { } else { }
func (p *Parser) parseIfStmt() *ast.IfStmt {

	start := p.advance().Start

	cond := p.parseExpr()
	body := p.parseBlock()

	var elseNode ast.Node
	if p.match(lexer.ELSE_TOKEN) {
		p.advance()
		if p.match(lexer.IF_TOKEN) {
			elseNode = p.parseIfStmt()
		} else {
			elseNode = p.parseBlock()
		}
	}

	return &ast.IfStmt{
		Cond:     cond,
		Body:     body,
		Else:     elseNode,
		Location: p.makeLocation(start),
	}
}

// parseExprOrAssign: expr; or expr = expr;
func (p *Parser) parseExprOrAssign() ast.Node {

	lhs := p.parseExpr()

	// Check for assignment
	if p.match(lexer.EQUALS_TOKEN) {
		p.advance()
		rhs := p.parseExpr()
		p.expect(lexer.SEMICOLON_TOKEN)

		return &ast.AssignStmt{
			Lhs:      lhs,
			Rhs:      rhs,
			Location: *source.NewLocation(lhs.Loc().Start, rhs.Loc().End),
		}
	}

	//defer p.expect(lexer.SEMICOLON_TOKEN) // consume semicolon at end of expression statement

	return &ast.ExprStmt{
		X:        lhs,
		Location: p.makeLocation(*lhs.Loc().Start),
	}
}

// parseExpr parses an expression
func (p *Parser) parseExpr() ast.Expression {
	return p.parseElvis()
}

// parseElvis parses elvis operator (a ?: b)
// Elvis has lower precedence than logical operators and is right-associative
func (p *Parser) parseElvis() ast.Expression {
	left := p.parseLogicalOr()

	if p.match(lexer.ELVIS_TOKEN) {
		p.advance()             // consume ?:
		right := p.parseElvis() // Right-associative: recursively parse elvis on the right
		return &ast.ElvisExpr{
			Cond:     left,
			Default:  right,
			Location: *source.NewLocation(left.Loc().Start, right.Loc().End),
		}
	}

	return left
}

func (p *Parser) parseLogicalOr() ast.Expression {
	left := p.parseLogicalAnd()

	for p.match(lexer.OR_TOKEN) {
		op := p.advance()
		right := p.parseLogicalAnd()
		left = &ast.BinaryExpr{
			X:        left,
			Op:       op,
			Y:        right,
			Location: *source.NewLocation(left.Loc().Start, right.Loc().End),
		}
	}

	return left
}

func (p *Parser) parseLogicalAnd() ast.Expression {
	left := p.parseEquality()

	for p.match(lexer.AND_TOKEN) {
		op := p.advance()
		right := p.parseEquality()
		left = &ast.BinaryExpr{
			X:        left,
			Op:       op,
			Y:        right,
			Location: *source.NewLocation(left.Loc().Start, right.Loc().End),
		}
	}

	return left
}

func (p *Parser) parseEquality() ast.Expression {
	left := p.parseComparison()

	for p.match(lexer.DOUBLE_EQUAL_TOKEN, lexer.NOT_EQUAL_TOKEN) {
		op := p.advance()
		right := p.parseComparison()
		left = &ast.BinaryExpr{
			X:        left,
			Op:       op,
			Y:        right,
			Location: *source.NewLocation(left.Loc().Start, right.Loc().End),
		}
	}

	return left
}

func (p *Parser) parseComparison() ast.Expression {
	left := p.parseAdditive()

	for p.match(lexer.LESS_TOKEN, lexer.LESS_EQUAL_TOKEN, lexer.GREATER_TOKEN, lexer.GREATER_EQUAL_TOKEN) {
		op := p.advance()
		right := p.parseAdditive()
		left = &ast.BinaryExpr{
			X:        left,
			Op:       op,
			Y:        right,
			Location: *source.NewLocation(left.Loc().Start, right.Loc().End),
		}
	}

	return left
}

func (p *Parser) parseAdditive() ast.Expression {
	left := p.parseMultiplicative()

	for p.match(lexer.PLUS_TOKEN, lexer.MINUS_TOKEN) {
		op := p.advance()
		right := p.parseMultiplicative()
		left = &ast.BinaryExpr{
			X:        left,
			Op:       op,
			Y:        right,
			Location: *source.NewLocation(left.Loc().Start, right.Loc().End),
		}
	}

	return left
}

func (p *Parser) parseMultiplicative() ast.Expression {
	left := p.parseUnary()

	for p.match(lexer.MUL_TOKEN, lexer.DIV_TOKEN, lexer.MOD_TOKEN) {
		op := p.advance()
		right := p.parseUnary()
		left = &ast.BinaryExpr{
			X:        left,
			Op:       op,
			Y:        right,
			Location: *source.NewLocation(left.Loc().Start, right.Loc().End),
		}
	}

	return left
}

func (p *Parser) parseUnary() ast.Expression {
	return p.parseUnaryDepth(0)
}

// parseUnaryDepth parses unary expressions with depth tracking to prevent stack overflow
func (p *Parser) parseUnaryDepth(depth int) ast.Expression {
	// Limit recursion depth to prevent stack overflow on pathological inputs like "------...x"
	const maxUnaryDepth = 100
	if depth >= maxUnaryDepth {
		tok := p.peek()
		p.diagnostics.Add(
			diagnostics.NewError("too many nested unary operators (maximum 100)").
				WithCode("P0002").
				WithPrimaryLabel(p.filepath, source.NewLocation(&tok.Start, &tok.End), "excessive nesting"),
		)
		return p.parsePostfix()
	}

	if p.match(lexer.NOT_TOKEN, lexer.MINUS_TOKEN) {
		op := p.advance()
		expr := p.parseUnaryDepth(depth + 1)
		return &ast.UnaryExpr{
			Op:       op,
			X:        expr,
			Location: *source.NewLocation(&op.Start, expr.Loc().End),
		}
	}

	return p.parsePostfix()
}

func (p *Parser) parsePostfix() ast.Expression {
	expr := p.parsePrimary()

	// If parsePrimary returned nil due to error, return nil to prevent nil pointer issues
	if expr == nil {
		return nil
	}

	for !p.isAtEnd() {
		if p.match(lexer.OPEN_PAREN) {
			expr = p.parseCallExpr(expr)
		} else if p.match(lexer.OPEN_BRACKET) {
			expr = p.parseIndexExpr(expr)
		} else if p.match(lexer.DOT_TOKEN) {
			expr = p.parseSelectorExpr(expr)
		} else if p.match(lexer.SCOPE_TOKEN) {
			expr = p.parseScopeResolutionExpr(expr)
		} else if p.match(lexer.AS_TOKEN) {
			// Type casting: expr as Type
			expr = p.parseCastExpr(expr)
		} else {
			break
		}
	}

	return expr
}

func (p *Parser) parseCallExpr(fun ast.Expression) *ast.CallExpr {

	p.advance() // consume '('
	args := []ast.Expression{}
	if !p.match(lexer.CLOSE_PAREN) {
		args = append(args, p.parseExpr())
		for p.match(lexer.COMMA_TOKEN) {
			p.advance()
			args = append(args, p.parseExpr())
		}
	}

	end := p.expect(lexer.CLOSE_PAREN).End

	// Check for catch clause
	var catchClause *ast.CatchClause
	if p.match(lexer.CATCH_TOKEN) {
		catchClause = p.parseCatchClause()
		end = *catchClause.End
	}

	return &ast.CallExpr{
		Fun:      fun,
		Args:     args,
		Catch:    catchClause,
		Location: *source.NewLocation(fun.Loc().Start, &end),
	}
}

// parseCatchClause parses a catch clause: catch [ident] [block] [fallback]
// Examples:
//   - catch 0                    -> just fallback
//   - catch err { ... } 0        -> error handler + fallback
//   - catch err { return 0; }    -> error handler only
func (p *Parser) parseCatchClause() *ast.CatchClause {

	start := p.expect(lexer.CATCH_TOKEN).Start // consume 'catch'

	var errIdent *ast.IdentifierExpr
	var handler *ast.Block
	var fallback ast.Expression

	// Check if we have an error identifier
	if p.match(lexer.IDENTIFIER_TOKEN) {
		errIdent = p.parseIdentifier()
	}

	// Check if we have a handler block
	if p.match(lexer.OPEN_CURLY) {
		handler = p.parseBlock()
	}

	// Check if we have a fallback value
	// Fallback is not present if we're at statement-ending tokens
	if !p.match(lexer.SEMICOLON_TOKEN, lexer.COMMA_TOKEN, lexer.CLOSE_PAREN, lexer.CLOSE_BRACKET, lexer.CLOSE_CURLY) && !p.isAtEnd() {
		fallback = p.parseExpr()
	}

	return &ast.CatchClause{
		ErrIdent: errIdent,
		Handler:  handler,
		Fallback: fallback,
		Location: p.makeLocation(start),
	}
}

func (p *Parser) parseIndexExpr(x ast.Expression) *ast.IndexExpr {
	start := p.advance().Start // consume '['
	index := p.parseExpr()
	end := p.expect(lexer.CLOSE_BRACKET).End

	return &ast.IndexExpr{
		X:        x,
		Index:    index,
		Location: *source.NewLocation(&start, &end),
	}
}

func (p *Parser) parseSelectorExpr(x ast.Expression) *ast.SelectorExpr {
	p.advance() // consume '.'
	sel := p.parseIdentifier()
	return &ast.SelectorExpr{
		X:        x,
		Sel:      sel,
		Location: *source.NewLocation(x.Loc().Start, sel.Loc().End),
	}
}

func (p *Parser) parseScopeResolutionExpr(x ast.Expression) *ast.ScopeResolutionExpr {
	p.advance() // consume '::'
	member := p.parseIdentifier()
	return &ast.ScopeResolutionExpr{
		X:        x,
		Selector: member,
		Location: *source.NewLocation(x.Loc().Start, member.Loc().End),
	}
}

func (p *Parser) parseCastExpr(x ast.Expression) *ast.CastExpr {
	p.advance() // consume 'as'
	targetType := p.parseType()
	return &ast.CastExpr{
		X:        x,
		Type:     targetType,
		Location: *source.NewLocation(x.Loc().Start, targetType.Loc().End),
	}
}

func (p *Parser) isCompositeLiteral() bool {

	savedPos := p.current
	p.advance() // consume the {

	// Empty {} or starts with .field = val (struct literal)
	if p.match(lexer.DOT_TOKEN, lexer.CLOSE_CURLY) {
		p.current = savedPos
		return true
	}

	// Check for key => val (map literal)
	// We need to parse ahead to see if there's a =>
	if p.match(lexer.IDENTIFIER_TOKEN, lexer.NUMBER_TOKEN, lexer.STRING_TOKEN, lexer.OPEN_PAREN, lexer.OPEN_BRACKET) {
		// Try to skip past a potential key expression
		p.advance()
		if p.match(lexer.FAT_ARROW_TOKEN) {
			p.current = savedPos
			return true
		}
	}

	p.current = savedPos
	return false
}

func (p *Parser) parseCompositeLiteralElements() []ast.Expression {
	elts := []ast.Expression{}

	if p.match(lexer.CLOSE_CURLY) {
		return elts
	}

	for !(p.match(lexer.CLOSE_CURLY) || p.isAtEnd()) {
		elt := p.parseCompositeLiteralElement()
		if elt != nil {
			elts = append(elts, elt)
		}

		if p.match(lexer.CLOSE_CURLY) {
			break
		}

		if p.checkTrailing(lexer.COMMA_TOKEN, lexer.CLOSE_CURLY, "composite literal") {
			p.advance() // skip the token
			break
		}

		p.expect(lexer.COMMA_TOKEN)
	}

	return elts
}

func (p *Parser) parseCompositeLiteralElement() ast.Expression {
	if p.match(lexer.DOT_TOKEN) {
		p.advance()
		name := p.parseIdentifier()
		p.expect(lexer.EQUALS_TOKEN)
		val := p.parseExpr()
		return &ast.KeyValueExpr{
			Key:      name,
			Value:    val,
			Location: *source.NewLocation(name.Start, val.Loc().End),
		}
	}

	key := p.parseExpr()
	if p.match(lexer.FAT_ARROW_TOKEN) {
		p.advance()
		val := p.parseExpr()
		return &ast.KeyValueExpr{
			Key:      key,
			Value:    val,
			Location: *source.NewLocation(key.Loc().Start, val.Loc().End),
		}
	}

	return key
}

func (p *Parser) parsePrimary() ast.Expression {
	tok := p.peek()

	switch tok.Kind {
	case lexer.NUMBER_TOKEN:
		p.advance()
		// Determine if int or float
		kind := ast.INT
		for _, ch := range tok.Value {
			if ch == '.' {
				kind = ast.FLOAT
				break
			}
		}
		return &ast.BasicLit{
			Kind:     kind,
			Value:    tok.Value,
			Location: *source.NewLocation(&tok.Start, &tok.End),
		}

	case lexer.STRING_TOKEN:
		p.advance()
		return &ast.BasicLit{
			Kind:     ast.STRING,
			Value:    tok.Value,
			Location: *source.NewLocation(&tok.Start, &tok.End),
		}

	case lexer.BYTE_TOKEN:
		p.advance()
		return &ast.BasicLit{
			Kind:     ast.BYTE,
			Value:    tok.Value,
			Location: *source.NewLocation(&tok.Start, &tok.End),
		}

	case lexer.IDENTIFIER_TOKEN:
		return p.parseIdentifier()

	case lexer.OPEN_PAREN:
		p.advance()
		expr := p.parseExpr()
		p.expect(lexer.CLOSE_PAREN)
		return expr

	case lexer.OPEN_BRACKET:
		return p.parseArrayLiteral()

	case lexer.MAP_TOKEN:
		// Map type in expression context (for composite literals)
		// e.g., map[str]i32{...}
		mapType := p.parseType()
		// MapType implements both TypeExpr() and Expr()
		if expr, ok := mapType.(ast.Expression); ok {
			return expr
		}
		p.error("map type cannot be used as expression")
		return nil

	case lexer.FUNCTION_TOKEN:
		// Anonymous function: fn (params) -> return { body }
		start := p.peek().Start
		p.advance() // consume 'fn'

		// Must have parameters for anonymous function
		if !p.match(lexer.OPEN_PAREN) {
			p.error("anonymous function must have parameters or ()")
			return nil
		}

		// parseFunctionParams will consume the '(' itself
		params := p.parseFunctionParams()
		p.expect(lexer.CLOSE_PAREN)

		// Parse as function literal
		return p.parseFuncLit(start, params)

	case lexer.OPEN_CURLY:
		// Anonymous struct/map literal: { .field = value } or { key => value }
		if !p.isCompositeLiteral() {
			p.error("unexpected '{' in expression context")
			return nil
		}
		start := p.advance().Start // consume '{'
		elems := p.parseCompositeLiteralElements()
		end := p.expect(lexer.CLOSE_CURLY).End

		// Create a composite literal with nil type (anonymous)
		return &ast.CompositeLit{
			Type:     nil, // Anonymous struct/map literal
			Elts:     elems,
			Location: *source.NewLocation(&start, &end),
		}

	default:

		tok := p.peek()

		// Create a detailed error message with helpful labels
		diag := diagnostics.NewError(fmt.Sprintf("unexpected token '%s' in expression", tok.Value)).
			WithCode("P0001").
			WithPrimaryLabel(p.filepath, source.NewLocation(&tok.Start, &tok.End),
				fmt.Sprintf("cannot use '%s' here", tok.Value))

		// Add context-specific help messages
		if tok.Kind == lexer.SEMICOLON_TOKEN {
			diag = diag.WithHelp("Unexpected semicolon - check if previous statement is complete")
		} else {
			diag = diag.WithHelp("Expected a value, identifier, literal, or expression here")
		}

		p.diagnostics.Add(diag)

		end := p.advance().End // Skip only the invalid token

		// Try to continue parsing - maybe the next token starts a valid expression
		// This allows recovery like: let x := . {.a = 1}
		if !p.isAtEnd() {
			// Recursively try to parse what follows as primary expression
			return p.parsePrimary()
		}

		// If at end, return invalid
		return &ast.Invalid{
			Location: *source.NewLocation(&tok.Start, &end),
		}
	}
}

func (p *Parser) parseArrayLiteral() *ast.CompositeLit {

	start := p.expect(lexer.OPEN_BRACKET).Start

	elems := []ast.Expression{}
	if !p.match(lexer.CLOSE_BRACKET) {
		elem := p.parseExpr()
		if elem != nil {
			elems = append(elems, elem)
		}
		for p.match(lexer.COMMA_TOKEN) {
			p.advance()
			if p.match(lexer.CLOSE_BRACKET) {
				break
			}
			elem := p.parseExpr()
			if elem != nil {
				elems = append(elems, elem)
			}
		}
	}

	end := p.expect(lexer.CLOSE_BRACKET).End

	return &ast.CompositeLit{
		Elts:     elems,
		Location: *source.NewLocation(&start, &end),
	}
}

func (p *Parser) parseIdentifier() *ast.IdentifierExpr {
	if !p.match(lexer.IDENTIFIER_TOKEN) {
		p.error(fmt.Sprintf("expected identifier, got %s", p.peek().Value))
		return &ast.IdentifierExpr{Name: "<error>"}
	}

	tok := p.advance()
	return &ast.IdentifierExpr{
		Name:     tok.Value,
		Location: *source.NewLocation(&tok.Start, &tok.End),
	}
}

// Helper methods

func (p *Parser) isAtEnd() bool {
	if p.current >= len(p.tokens) {
		return true
	}
	return p.tokens[p.current].Kind == lexer.EOF_TOKEN
}

func (p *Parser) peek() lexer.Token {
	if p.current >= len(p.tokens) {
		return p.tokens[len(p.tokens)-1]
	}
	return p.tokens[p.current]
}

func (p *Parser) previous() lexer.Token {
	return p.tokens[p.current-1]
}

func (p *Parser) next() lexer.Token {
	if p.current+1 >= len(p.tokens) {
		return p.tokens[len(p.tokens)-1]
	}
	return p.tokens[p.current+1]
}

func (p *Parser) advance() lexer.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *Parser) match(kinds ...lexer.TOKEN) bool {
	for _, kind := range kinds {
		if p.peek().Kind == kind {
			return true
		}
	}
	return false
}

func (p *Parser) expect(kind lexer.TOKEN) lexer.Token {
	return p.expectError(kind, fmt.Sprintf("unexpected token %v, expected %s", p.peek().Value, kind))
}

func (p *Parser) expectError(kind lexer.TOKEN, msg string) lexer.Token {

	if p.match(kind) {
		return p.advance()
	}

	if kind == lexer.SEMICOLON_TOKEN {
		//tok := p.peek()
		// p.diagnostics.Add(
		// 	diagnostics.NewError(fmt.Sprintf("expected ';' at end of statement, got %s", tok.Value)).
		// 		WithCode(diagnostics.ErrUnterminatedExpr).
		// 		WithPrimaryLabel(p.filepath, source.NewLocation(&tok.Start, &tok.End), "missing semicolon").
		// 		WithNote("Every statement must end with a semicolon"),
		// )

		// highlight the end of the previous token as the likely location
		prevTok := p.previous()
		loc := source.NewLocation(&prevTok.End, &prevTok.End)
		p.diagnostics.Add(
			diagnostics.NewError("expected ';' at end of statement").
				WithCode(diagnostics.ErrUnterminatedExpr).
				WithPrimaryLabel(p.filepath, loc, "add semicolon here").
				WithNote("Every statement must end with a semicolon"),
		)
		
		return p.peek()
	}

	p.error(msg)
	return p.advance()
}

// error reports a parsing error to the diagnostics
func (p *Parser) error(msg string) {
	tok := p.peek()
	loc := source.NewLocation(&tok.Start, &tok.End)
	p.diagnostics.Add(
		diagnostics.NewError(msg).
			WithCode("P0001").
			WithPrimaryLabel(p.filepath, loc, ""),
	)
}

// checkTrailing checks if the current position is at a closing delimiter
// after a comma was just consumed, and emits an info diagnostic if so.
// This provides a consistent message across all composite structures.
//
// This function should be called AFTER consuming a comma/separator token.
//
// Parameters:
//   - closingToken: The expected closing token (e.g., CLOSE_CURLY, CLOSE_PAREN)
//   - contextName: Name of the context for the diagnostic message (e.g., "struct literal", "map literal")
//
// Returns true if we're at the closing delimiter (indicating a trailing comma was found).
func (p *Parser) checkTrailing(target, closingToken lexer.TOKEN, contextName string) bool {

	if p.match(target) && p.next().Kind == closingToken {
		// We just consumed a comma and we're at the closing delimiter
		// This means there was a trailing comma
		token := p.peek()
		loc := source.NewLocation(&token.Start, &token.End)
		p.diagnostics.Add(
			diagnostics.NewInfo(fmt.Sprintf("trailing %s in %s", target, contextName)).
				WithCode(diagnostics.InfoTrailingComma).
				WithPrimaryLabel(p.filepath, loc, "remove this trailing comma").
				WithNote(fmt.Sprintf("Trailing %s are allowed but excluding it keeps code clean ans consistent", target)),
		)
		return true
	}
	return false
}

// makeLocation creates a source location from start to current position
func (p *Parser) makeLocation(start source.Position) source.Location {
	end := p.previous().End
	return *source.NewLocation(&start, &end)
}
