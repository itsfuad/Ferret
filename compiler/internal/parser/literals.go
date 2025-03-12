package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
	"ferret/compiler/report"
	"strings"
)

func parseNumberLiteral(p *Parser) ast.Expression {
	number := p.consume(lexer.NUMBER_TOKEN, report.EXPECTED_NUMBER)
	raw := number.Value
	value := strings.ReplaceAll(raw, "_", "") // Remove underscores
	loc := ast.Location{
		Start: &number.Start,
		End:   &number.End,
	}

	// Try parsing as integer first
	if types.ValidateHexadecimal(value) {
		intVal, err := types.ParseInteger(value)
		if err != nil {
			report.Add(p.filePath, number.Start.Line, number.End.Line, number.Start.Column, number.End.Column, report.INT_OUT_OF_RANGE).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		return &ast.IntLiteral{
			Value:    intVal,
			Raw:      raw,
			Base:     16,
			Location: loc,
		}
	}

	if types.ValidateOctal(value) {
		intVal, err := types.ParseInteger(value)
		if err != nil {
			report.Add(p.filePath, number.Start.Line, number.End.Line, number.Start.Column, number.End.Column, report.INT_OUT_OF_RANGE).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		return &ast.IntLiteral{
			Value:    intVal,
			Raw:      raw,
			Base:     8,
			Location: loc,
		}
	}

	if types.ValidateBinary(value) {
		intVal, err := types.ParseInteger(value)
		if err != nil {
			report.Add(p.filePath, number.Start.Line, number.End.Line, number.Start.Column, number.End.Column, report.INT_OUT_OF_RANGE).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		return &ast.IntLiteral{
			Value:    intVal,
			Raw:      raw,
			Base:     2,
			Location: loc,
		}
	}

	// Try as decimal integer
	if types.ValidateDecimal(value) {
		intVal, err := types.ParseInteger(value)
		if err != nil {
			report.Add(p.filePath, number.Start.Line, number.End.Line, number.Start.Column, number.End.Column, report.INT_OUT_OF_RANGE).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		return &ast.IntLiteral{
			Value:    intVal,
			Raw:      raw,
			Base:     10,
			Location: loc,
		}
	}

	// Then try as float (including scientific notation)
	if types.ValidateFloat(value) {
		floatVal, err := types.ParseFloat(value)
		if err != nil {
			report.Add(p.filePath, number.Start.Line, number.End.Line, number.Start.Column, number.End.Column, report.FLOAT_OUT_OF_RANGE).SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		return &ast.FloatLiteral{
			Value:    floatVal,
			Raw:      raw,
			Location: loc,
		}
	}

	// If neither, it's an invalid number format
	report.Add(p.filePath, number.Start.Line, number.End.Line, number.Start.Column, number.End.Column, report.INVALID_NUMBER).SetLevel(report.SYNTAX_ERROR)
	return nil
}

func parseStringLiteral(p *Parser) ast.Expression {
	stringLiteral := p.consume(lexer.STRING_TOKEN, report.EXPECTED_STRING)
	loc := ast.Location{
		Start: &stringLiteral.Start,
		End:   &stringLiteral.End,
	}

	return &ast.StringLiteral{
		Value:    stringLiteral.Value,
		Location: loc,
	}
}

func parseArrayLiteral(p *Parser) ast.Expression {
	start := p.advance().Start // consume '['
	elements := make([]ast.Expression, 0)

	// Must have at least one element
	expr := parseExpression(p)
	if expr == nil {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, report.ARRAY_EMPTY).SetLevel(report.SYNTAX_ERROR)
		return nil
	}
	elements = append(elements, expr)

	// Parse additional elements
	for p.peek().Kind == lexer.COMMA_TOKEN {
		p.advance() // consume ','

		// Check for trailing comma
		if p.peek().Kind == lexer.CLOSE_BRACKET {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, report.ARRAY_TRAILING_COMMA).SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		expr = parseExpression(p)
		if expr == nil {
			return nil
		}
		elements = append(elements, expr)
	}

	end := p.consume(lexer.CLOSE_BRACKET, report.EXPECTED_CLOSE_BRACKET)

	return &ast.ArrayLiteralExpr{
		Elements: elements,
		Location: ast.Location{
			Start: &start,
			End:   &end.End,
		},
	}
}

// parseObjectType parses an object type definition like { name: str, age: i32 }
func parseObjectType(p *Parser) ast.DataType {
	start := p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE).Start
	fields := make([]ast.ObjectField, 0)
	fieldNames := make(map[string]bool)

	// Check for empty object type
	if p.peek().Kind == lexer.CLOSE_CURLY {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			report.EMPTY_OBJECT_NOT_ALLOWED).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	for p.peek().Kind != lexer.CLOSE_CURLY {

		// Parse field name
		nameToken := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_OBJECT_FIELD_NAME)
		fieldName := nameToken.Value

		// Check for duplicate field names
		if fieldNames[fieldName] {
			report.Add(p.filePath, nameToken.Start.Line, nameToken.End.Line,
				nameToken.Start.Column, nameToken.End.Column,
				report.DUPLICATE_FIELD_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		fieldNames[fieldName] = true

		// Expect colon
		p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)

		// Parse field type
		fieldType := parseType(p)
		if fieldType == nil {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
				p.peek().Start.Column, p.peek().End.Column,
				report.EXPECTED_FIELD_TYPE).SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		fields = append(fields, ast.ObjectField{
			Name: fieldName,
			Type: fieldType,
			Location: ast.Location{
				Start: &nameToken.Start,
				End:   fieldType.EndPos(),
			},
		})

		// if not closing brace, expect comma
		if !p.match(lexer.CLOSE_CURLY) {
			p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_BRACE)
		} else {
			break
		}
	}

	end := p.consume(lexer.CLOSE_CURLY, report.EXPECTED_CLOSE_BRACE).End

	return &ast.ObjectType{
		Fields:   fields,
		TypeName: types.OBJECT,
		Location: ast.Location{
			Start: &start,
			End:   &end,
		},
	}
}

// parseObjectLiteral parses an object literal like { name: "John", age: 20 }
func parseObjectLiteral(p *Parser) ast.Expression {
	start := p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE).Start
	fields := make([]ast.ObjectField, 0)
	fieldNames := make(map[string]bool)

	// Check for empty object
	if p.peek().Kind == lexer.CLOSE_CURLY {
		// empty object, add error
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			report.EMPTY_OBJECT_NOT_ALLOWED).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	for p.peek().Kind != lexer.CLOSE_CURLY {

		// Parse field name
		nameToken := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_OBJECT_FIELD_NAME)
		fieldName := nameToken.Value

		// Check for duplicate field names
		if fieldNames[fieldName] {
			report.Add(p.filePath, nameToken.Start.Line, nameToken.End.Line,
				nameToken.Start.Column, nameToken.End.Column,
				report.DUPLICATE_FIELD_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		fieldNames[fieldName] = true

		// Expect colon
		p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)

		// Parse field value
		fieldValue := parseExpression(p)
		if fieldValue == nil {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
				p.peek().Start.Column, p.peek().End.Column,
				report.EXPECTED_FIELD_VALUE).SetLevel(report.SYNTAX_ERROR)
			return nil
		}

		fields = append(fields, ast.ObjectField{
			Name:  fieldName,
			Value: fieldValue,
			Location: ast.Location{
				Start: &nameToken.Start,
				End:   fieldValue.EndPos(),
			},
		})

		//if closing brace, break
		if p.match(lexer.CLOSE_CURLY) {
			break
		}

		//if not closing brace, expect comma
		if !p.match(lexer.CLOSE_CURLY) {
			p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_BRACE)
		}
	}

	end := p.consume(lexer.CLOSE_CURLY, report.EXPECTED_CLOSE_BRACE).End

	return &ast.ObjectLiteralExpr{
		Fields: fields,
		Location: ast.Location{
			Start: &start,
			End:   &end,
		},
	}
}
