package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
)

// validateStructType validates the struct type and returns the type name
func validateStructType(p *Parser) (*ast.IdentifierExpr, bool) {
	if !p.match(lexer.IDENTIFIER_TOKEN, lexer.STRUCT_TOKEN) {
		token := p.peek()
		report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.EXPECTED_TYPE_NAME).SetLevel(report.SYNTAX_ERROR)
		return nil, false
	}

	token := p.advance()
	typeName := &ast.IdentifierExpr{
		Name: token.Value,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}

	return typeName, true
}

// parseStructFields parses the fields of a struct literal
func parseStructFields(p *Parser) ([]ast.StructField, bool) {
	fieldNames := make(map[string]bool)
	fields := make([]ast.StructField, 0)

	for !p.match(lexer.CLOSE_CURLY) {
		fieldName := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_FIELD_NAME)
		if fieldNames[fieldName.Value] {
			report.Add(p.filePath, fieldName.Start.Line, fieldName.End.Line, fieldName.Start.Column, fieldName.End.Column, report.DUPLICATE_FIELD_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil, false
		}
		fieldNames[fieldName.Value] = true
		p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)

		value := parseExpression(p)
		if value == nil {
			report.Add(p.filePath, fieldName.Start.Line, fieldName.End.Line, fieldName.Start.Column, fieldName.End.Column, report.EXPECTED_FIELD_VALUE).AddHint("Add an expression after the colon").SetLevel(report.SYNTAX_ERROR)
			return nil, false
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
			comma := p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_BRACE)
			if p.match(lexer.CLOSE_CURLY) {
				report.Add(p.filePath, comma.Start.Line, comma.End.Line, comma.Start.Column, comma.End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.WARNING)
				break
			}
		}
	}

	return fields, true
}

// parseStructLiteral parses a struct literal expression like Point{x: 10, y: 20}
func parseStructLiteral(p *Parser) ast.Expression {
	start := p.consume(lexer.AT_TOKEN, report.EXPECTED_AT_TOKEN).Start

	typeName, ok := validateStructType(p)
	if !ok {
		return nil
	}

	p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE)

	if p.peek().Kind == lexer.CLOSE_CURLY {
		token := p.peek()
		report.Add(p.filePath, token.Start.Line, token.End.Line,
			token.Start.Column, token.End.Column,
			report.EMPTY_STRUCT_NOT_ALLOWED).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	fields, ok := parseStructFields(p)
	if !ok {
		return nil
	}

	end := p.consume(lexer.CLOSE_CURLY, report.EXPECTED_CLOSE_BRACE).End

	return &ast.StructLiteralExpr{
		TypeName:    *typeName,
		Fields:      fields,
		IsAnonymous: lexer.TOKEN(typeName.Name) == lexer.STRUCT_TOKEN,
		Location: ast.Location{
			Start: &start,
			End:   &end,
		},
	}
}

// parseFieldAccess parses a field access expression like struct.field
func parseFieldAccess(p *Parser, object ast.Expression) ast.Expression {
	p.advance() // consume '.'

	// Parse field name
	if !p.match(lexer.IDENTIFIER_TOKEN) {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			"Expected field name after '.'").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	fieldToken := p.advance()
	field := &ast.IdentifierExpr{
		Name: fieldToken.Value,
		Location: ast.Location{
			Start: &fieldToken.Start,
			End:   &fieldToken.End,
		},
	}

	return &ast.FieldAccessExpr{
		Object: object,
		Field:  field,
		Location: ast.Location{
			Start: object.StartPos(),
			End:   &fieldToken.End,
		},
	}
}
