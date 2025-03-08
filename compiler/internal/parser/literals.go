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
		Start: number.Start,
		End:   number.End,
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
		Start: stringLiteral.Start,
		End:   stringLiteral.End,
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
			Start: start,
			End:   end.End,
		},
	}
}
