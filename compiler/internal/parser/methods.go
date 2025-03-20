package parser

import (
	"ferret/compiler/colors"
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
)

func parseMethodDeclaration(p *Parser, startPos *lexer.Position, receivers []ast.Parameter) *ast.MethodDecl {
	colors.BLUE.Println("Parsing ")
	name := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_METHOD_NAME)

	//start a new scope
	p.enterScope(symboltable.FUNCTION_SCOPE)
	defer p.exitScope()

	iden := ast.IdentifierExpr{
		Name: name.Value,
		Location: ast.Location{
			Start: &name.Start,
			End:   &name.End,
		},
	}

	if len(receivers) == 0 {
		report.Add(p.filePath, name.Start.Line, name.End.Line, name.Start.Column, name.End.Column, "Expected receiver").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	if len(receivers) > 1 {
		receiver := receivers[1]
		report.Add(p.filePath, receiver.Identifier.StartPos().Line, receiver.Identifier.EndPos().Line, receiver.Identifier.StartPos().Column, receiver.Identifier.EndPos().Column, "expected only one receiver").SetLevel(report.NORMAL_ERROR)
	}

	receiver := receivers[0]

	funcLit := parseFunctionLiteral(p, &name.Start, false, true)

	return &ast.MethodDecl{
		Method:   iden,
		Receiver: &receiver,
		Function: funcLit,
		Location: ast.Location{
			Start: startPos,
			End:   funcLit.EndPos(),
		},
	}
}
