package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
)

// parseExpression is the entry point for expression parsing
func parseExpression(p *Parser) ast.Expression {
	return parseLogicalOr(p)
}

// parseLogicalOr handles || operator
func parseLogicalOr(p *Parser) ast.Expression {
	expr := parseLogicalAnd(p)

	for p.match(lexer.OR_TOKEN) {
		operator := p.advance()
		right := parseLogicalAnd(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseLogicalAnd handles && operator
func parseLogicalAnd(p *Parser) ast.Expression {
	expr := parseEquality(p)

	for p.match(lexer.AND_TOKEN) {
		operator := p.advance()
		right := parseEquality(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseEquality handles == and != operators
func parseEquality(p *Parser) ast.Expression {
	expr := parseComparison(p)

	for p.match(lexer.DOUBLE_EQUAL_TOKEN, lexer.NOT_EQUAL_TOKEN) {
		operator := p.advance()
		right := parseComparison(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseComparison handles <, >, <=, >= operators
func parseComparison(p *Parser) ast.Expression {
	expr := parseAdditive(p)

	for p.match(lexer.LESS_TOKEN, lexer.GREATER_TOKEN, lexer.LESS_EQUAL_TOKEN, lexer.GREATER_EQUAL_TOKEN) {
		operator := p.advance()
		right := parseAdditive(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseAdditive handles + and - operators
func parseAdditive(p *Parser) ast.Expression {
	expr := parseMultiplicative(p)

	for p.match(lexer.PLUS_TOKEN, lexer.MINUS_TOKEN) {
		operator := p.advance()
		right := parseMultiplicative(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseMultiplicative handles *, /, and % operators
func parseMultiplicative(p *Parser) ast.Expression {
	expr := parseUnary(p)

	for p.match(lexer.MUL_TOKEN, lexer.DIV_TOKEN, lexer.MOD_TOKEN) {
		operator := p.advance()
		right := parseUnary(p)
		expr = &ast.BinaryExpr{
			Left:     expr,
			Operator: operator,
			Right:    right,
			Location: ast.Location{
				Start: expr.StartPos(),
				End:   right.EndPos(),
			},
		}
	}

	return expr
}

// parseUnary handles unary operators (!, -, ++, --)
func parseUnary(p *Parser) ast.Expression {
	if p.match(lexer.NOT_TOKEN, lexer.MINUS_TOKEN) {
		operator := p.advance()
		right := parseUnary(p)
		return &ast.UnaryExpr{
			Operator: operator,
			Operand:  right,
			Location: ast.Location{
				Start: &operator.Start,
				End:   right.EndPos(),
			},
		}
	}

	// Handle prefix operators (++, --)
	if p.match(lexer.PLUS_PLUS_TOKEN, lexer.MINUS_MINUS_TOKEN) {
		operator := p.advance()
		// Check for consecutive operators
		if p.match(lexer.PLUS_PLUS_TOKEN, lexer.MINUS_MINUS_TOKEN) {
			errMsg := report.INVALID_CONSECUTIVE_INCREMENT
			if operator.Kind == lexer.MINUS_MINUS_TOKEN {
				errMsg = report.INVALID_CONSECUTIVE_DECREMENT
			}
			report.Add(p.filePath, operator.Start.Line, operator.End.Line, operator.Start.Column, operator.End.Column, errMsg).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		operand := parseUnary(p)
		if operand == nil {
			errMsg := report.INVALID_INCREMENT_OPERAND
			if operator.Kind == lexer.MINUS_MINUS_TOKEN {
				errMsg = report.INVALID_DECREMENT_OPERAND
			}
			report.Add(p.filePath, operator.Start.Line, operator.End.Line, operator.Start.Column, operator.End.Column, errMsg).SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		// Check if operand already has a postfix operator
		if _, ok := operand.(*ast.PostfixExpr); ok {
			report.Add(p.filePath, operator.Start.Line, operator.End.Line, operator.Start.Column, operator.End.Column, "Cannot mix prefix and postfix operators").SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		return &ast.PrefixExpr{
			Operator: operator,
			Operand:  operand,
			Location: ast.Location{
				Start: &operator.Start,
				End:   operand.EndPos(),
			},
		}
	}

	return parsePostfix(p)
}

// parseIndexing handles array/map indexing operations
func parseIndexing(p *Parser, expr ast.Expression) ast.Expression {
	start := expr.StartPos()
	p.advance() // consume '['

	index := parseExpression(p)
	if index == nil {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, report.MISSING_INDEX_EXPRESSION).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	end := p.consume(lexer.CLOSE_BRACKET, report.EXPECTED_CLOSE_BRACKET)
	return &ast.IndexableExpr{
		Indexable: expr,
		Index:     index,
		Location: ast.Location{
			Start: start,
			End:   &end.End,
		},
	}
}

// parseIncDec handles postfix increment/decrement
func parseIncDec(p *Parser, expr ast.Expression) ast.Expression {
	operator := p.advance()
	if p.match(lexer.PLUS_PLUS_TOKEN, lexer.MINUS_MINUS_TOKEN) {
		errMsg := report.INVALID_CONSECUTIVE_INCREMENT
		if operator.Kind == lexer.MINUS_MINUS_TOKEN {
			errMsg = report.INVALID_CONSECUTIVE_DECREMENT
		}
		report.Add(p.filePath, operator.Start.Line, operator.End.Line, operator.Start.Column, operator.End.Column, errMsg).SetLevel(report.SYNTAX_ERROR)
		return nil
	}
	return &ast.PostfixExpr{
		Operand:  expr,
		Operator: operator,
		Location: ast.Location{
			Start: expr.StartPos(),
			End:   &operator.End,
		},
	}
}

// parsePostfix handles postfix operators (++, --, [], ., (), {})
func parsePostfix(p *Parser) ast.Expression {
	expr := parsePrimary(p)
	if expr == nil {
		return nil
	}

	for {

		if p.match(lexer.PLUS_PLUS_TOKEN, lexer.MINUS_MINUS_TOKEN) {
			// Check if expression already has a prefix operator
			if _, ok := expr.(*ast.PrefixExpr); ok {
				current := p.peek()
				report.Add(p.filePath, current.Start.Line, current.End.Line, current.Start.Column, current.End.Column, "Cannot mix prefix and postfix operators").SetLevel(report.SYNTAX_ERROR)
				return nil
			}

			if result := parseIncDec(p, expr); result != nil {
				expr = result
				continue
			}
			return nil
		}

		if p.match(lexer.DOT_TOKEN) {
			if result := parseFieldAccess(p, expr); result != nil {
				expr = result
				continue
			}
			return nil
		}

		if p.match(lexer.OPEN_PAREN) {
			if result := parseFunctionCall(p, expr); result != nil {
				expr = result
				continue
			}
			return nil
		}

		if p.match(lexer.OPEN_BRACKET) {
			if result := parseIndexing(p, expr); result != nil {
				expr = result
				continue
			}
			return nil
		}

		break
	}

	return expr
}

// parseGrouping handles parenthesized expressions
func parseGrouping(p *Parser) ast.Expression {
	p.advance() // consume '('
	expr := parseExpression(p)
	p.consume(lexer.CLOSE_PAREN, "Expected ')' after expression")
	return expr
}

// parseStructLiteral parses a struct literal expression like Point{x: 10, y: 20}
func parseStructLiteral(p *Parser) ast.Expression {

	start := p.consume(lexer.AT_TOKEN, report.EXPECTED_AT_TOKEN).Start

	var typeName *ast.IdentifierExpr

	if p.match(lexer.IDENTIFIER_TOKEN, lexer.STRUCT_TOKEN) {
		token := p.advance()
		iden := &ast.IdentifierExpr{
			Name: token.Value,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
		typeName = iden
	} else {
		token := p.peek()
		report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_TYPE_NAME).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	isAnonymous := lexer.TOKEN(typeName.Name) == lexer.STRUCT_TOKEN

	if !isAnonymous {
		//must be defined
		if symbol, ok := p.currentScope.Resolve(typeName.Name); ok {
			if symbol.SymbolKind != symboltable.TYPE_SYMBOL {
				report.Add(p.filePath, typeName.StartPos().Line, typeName.EndPos().Line, typeName.StartPos().Column, typeName.EndPos().Column, "Expected type name after '@'").SetLevel(report.SYNTAX_ERROR)
				return nil
			}
		} else {
			report.Add(p.filePath, typeName.StartPos().Line, typeName.EndPos().Line, typeName.StartPos().Column, typeName.EndPos().Column, "Undefined type '"+typeName.Name+"'").SetLevel(report.SEMANTIC_ERROR)
			return nil
		}
	}

	p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE)

	// Check for empty struct
	if p.peek().Kind == lexer.CLOSE_CURLY {
		token := p.peek()
		report.Add(p.filePath, token.Start.Line, token.End.Line,
			token.Start.Column, token.End.Column,
			report.EMPTY_STRUCT_NOT_ALLOWED).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	fieldNames := make(map[string]bool)

	// Parse field initializers
	fields := make([]ast.StructField, 0)
	for {
		// Parse field name
		fieldName := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_FIELD_NAME)
		if fieldNames[fieldName.Value] {
			report.Add(p.filePath, fieldName.Start.Line, fieldName.End.Line, fieldName.Start.Column, fieldName.End.Column, report.DUPLICATE_FIELD_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		fieldNames[fieldName.Value] = true
		p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)

		// Parse field value
		value := parseExpression(p)
		if value == nil {
			report.Add(p.filePath, fieldName.Start.Line, fieldName.End.Line, fieldName.Start.Column, fieldName.End.Column, report.EXPECTED_FIELD_VALUE).AddHint("Add an expression after the colon").SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		fields = append(fields, ast.StructField{
			Field: ast.IdentifierExpr{
				Name: fieldName.Value,
				Location: ast.Location{
					Start: &fieldName.Start,
					End:   &fieldName.End,
				},
			},
			Value: value,
			Location: ast.Location{
				Start: &fieldName.Start,
				End:   value.EndPos(),
			},
		})

		if p.match(lexer.CLOSE_CURLY) {
			break
		} else {
			p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_BRACE)
		}
	}

	end := p.consume(lexer.CLOSE_CURLY, report.EXPECTED_CLOSE_BRACE).End

	return &ast.StructLiteralExpr{
		TypeName:    *typeName,
		Fields:      fields,
		IsAnonymous: isAnonymous,
		Location: ast.Location{
			Start: &start,
			End:   &end,
		},
	}
}

// parseFunctionCall parses a function call expression
func parseFunctionCall(p *Parser, caller ast.Expression) ast.Expression {
	start := caller.StartPos()
	p.advance() // consume '('

	arguments := make([]ast.Expression, 0)

	// Handle empty argument list
	if p.peek().Kind == lexer.CLOSE_PAREN {
		end := p.advance() // consume ')'
		return &ast.FunctionCallExpr{
			Caller:    caller,
			Arguments: arguments,
			Location: ast.Location{
				Start: start,
				End:   &end.End,
			},
		}
	}

	// Parse arguments
	for {
		arg := parseExpression(p)
		if arg == nil {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
				p.peek().Start.Column, p.peek().End.Column,
				"Expected function argument").SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		arguments = append(arguments, arg)

		if p.match(lexer.CLOSE_PAREN) {
			break
		} else {
			p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_PAREN)
		}
	}

	end := p.consume(lexer.CLOSE_PAREN, report.EXPECTED_CLOSE_PAREN)
	return &ast.FunctionCallExpr{
		Caller:    caller,
		Arguments: arguments,
		Location: ast.Location{
			Start: start,
			End:   &end.End,
		},
	}
}

// parsePrimary handles literals, identifiers, and parenthesized expressions
func parsePrimary(p *Parser) ast.Expression {
	switch p.peek().Kind {
	case lexer.OPEN_PAREN:
		return parseGrouping(p)
	case lexer.OPEN_BRACKET:
		return parseArrayLiteral(p)
	case lexer.NUMBER_TOKEN:
		return parseNumberLiteral(p)
	case lexer.STRING_TOKEN:
		return parseStringLiteral(p)
	case lexer.FUNCTION_TOKEN:
		start := p.advance()
		return parseFunctionLiteral(p, &start.Start, true)
	case lexer.AT_TOKEN:
		return parseStructLiteral(p)
	case lexer.IDENTIFIER_TOKEN:
		return parseIdentifier(p)
	}
	handleUnexpectedToken(p)
	return nil
}
