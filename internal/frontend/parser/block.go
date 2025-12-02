package parser

import (
	"compiler/internal/frontend/ast"
	"compiler/internal/frontend/lexer"
	"compiler/internal/source"
)

func (p *Parser) parseBlock() *ast.Block {

	start := p.expect(lexer.OPEN_CURLY).Start

	nodes := []ast.Node{}
	for !(p.match(lexer.CLOSE_CURLY) || p.isAtEnd()) {
		node := p.parseStmt()
		if node == nil {
			// parseStmt() returned nil - this is an error case
			// Advance to prevent infinite loop and try to recover
			p.error("unexpected token in block")
			p.advance()
			continue
		}
		nodes = append(nodes, node)
	}

	end := p.expect(lexer.CLOSE_CURLY).End

	return &ast.Block{
		Nodes:    nodes,
		Location: *source.NewLocation(&start, &end),
	}
}
