package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/source"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/internal/types"
	"ferret/compiler/report"
	"fmt"
)

func parseIdentifiers(p *Parser) ([]*ast.VariableToDeclare, int) {

	variables := make([]*ast.VariableToDeclare, 0)
	varCount := 0

	for {
		if !p.check(lexer.IDENTIFIER_TOKEN) {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, report.MISSING_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil, 0
		}
		identifierName := p.advance()
		identifier := &ast.VariableToDeclare{
			Identifier: &ast.IdentifierExpr{
				Name:     identifierName.Value,
				Location: *source.NewLocation(&identifierName.Start, &identifierName.End),
			},
		}
		variables = append(variables, identifier)
		varCount++

		if p.peek().Kind != lexer.COMMA_TOKEN {
			break
		}
		p.advance()
	}
	return variables, varCount
}

// parseTypeAnnotations parses the type annotations for the variables
// it returns a list of types and a boolean indicating if the parsing was successful
func parseTypeAnnotations(p *Parser) ([]ast.DataType, bool) {
	if p.peek().Kind != lexer.COLON_TOKEN {
		return nil, true
	}

	p.advance()
	types := make([]ast.DataType, 0)
	for {
		typeNode, ok := parseType(p)
		if !ok {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, report.MISSING_TYPE_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil, false
		}
		types = append(types, typeNode)

		if p.peek().Kind != lexer.COMMA_TOKEN {
			break
		}
		p.advance()
	}
	return types, true
}

func parseInitializers(p *Parser) ([]ast.Expression, bool) {

	values := make([]ast.Expression, 0)

	if p.match(lexer.EQUALS_TOKEN) {
		p.advance()
		for {
			value := parseExpression(p)
			if value == nil {
				report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "Expected value after '=', got invalid expression").SetLevel(report.SYNTAX_ERROR)
				return nil, false
			}
			values = append(values, value)

			if p.peek().Kind != lexer.COMMA_TOKEN {
				break
			}
			p.advance()
		}
	}

	return values, true
}

func assignTypes(p *Parser, variables []*ast.VariableToDeclare, types []ast.DataType, varCount int) bool {
	if len(types) == 0 {
		return true
	}
	if len(types) == 1 {
		for i := range variables {
			variables[i].ExplicitType = types[0]
		}
		return true
	}
	if len(types) == varCount {
		for i := range variables {
			variables[i].ExplicitType = types[i]
		}
		return true
	}
	report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, report.MISMATCHED_VARIABLE_AND_TYPE_COUNT+fmt.Sprintf(": Expected %d types, got %d", varCount, len(types))).SetLevel(report.SYNTAX_ERROR)
	return false
}

func parseVarDecl(p *Parser) ast.Statement {
	token := p.advance() // consume let/const

	isConst := token.Kind == lexer.CONST_TOKEN

	variables, varCount := parseIdentifiers(p)
	if variables == nil {
		pos := p.peek()
		report.Add(p.filePath,
			pos.Start.Line, pos.End.Line,
			pos.Start.Column, pos.End.Column,
			"no variables found").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	parsedTypes, ok := parseTypeAnnotations(p)
	if !ok || !assignTypes(p, variables, parsedTypes, varCount) {
		return nil
	}

	values, ok := parseInitializers(p)
	if !ok {
		return nil
	}

	// when no types provides, we must initialize
	if len(parsedTypes) == 0 {
		if len(values) == 0 {
			report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "cannot infer types without initializers").AddHint("👈😃 Add initializers to the variables").SetLevel(report.NORMAL_ERROR)
			return nil
		}
	}

	if len(values) > varCount {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line, p.peek().Start.Column, p.peek().End.Column, "values cannot be more than the number of variables").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	// Add variables to symbol table
	for _, v := range variables {

		var typename types.TYPE_NAME
		if v.ExplicitType != nil {
			typename = v.ExplicitType.Type()
		}

		sym := &symboltable.Symbol{
			Name:       v.Identifier.Name,
			SymbolKind: symboltable.VARIABLE_SYMBOL,
			Type:       typename,
			IsMutable:  !isConst,
			Location: symboltable.SymbolLocation{
				File:   p.filePath,
				Line:   v.Identifier.StartPos().Line,
				Column: v.Identifier.StartPos().Column,
			},
		}

		if !p.currentScope.Define(sym) {
			report.ShowRedeclarationError(
				v.Identifier.Name,
				p.filePath,
				p.currentScope,
				v.Identifier.StartPos().Line,
				v.Identifier.EndPos().Line,
				v.Identifier.StartPos().Column,
				v.Identifier.EndPos().Column,
			)
		}
	}

	return &ast.VarDeclStmt{
		Variables:    variables,
		Initializers: values,
		IsConst:      isConst,
		Location:     *source.NewLocation(&token.Start, variables[len(variables)-1].Identifier.EndPos()),
	}
}
