package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/report"
)

func parseMethodDeclaration(p *Parser, startPos *lexer.Position, receivers []ast.Parameter) *ast.MethodDecl {

	name := p.consume(lexer.IDENTIFIER_TOKEN, "Expected method name")

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
		report.Add(p.filePath, name.Start.Line, name.End.Line, name.Start.Column, name.End.Column, "Expected only one receiver").SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	receiver := receivers[0]

	funcLit := parseFunctionLiteral(p, &name.Start, true)

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
