package resolver

import (
	"ferret/compiler/colors"
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
)

func ASTNodeToAnalyzerNode(node ast.Node, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	// Check if the node is a type
	if t := checkTypeNames(node, table); t != nil {
		return t
	}

	// Check if the node is a literal
	if lit := getLiteralType(node, table); lit != nil {
		return lit
	}

	switch n := node.(type) {
	case *ast.IdentifierExpr:
		return ckeckIdentifierNode(n, table)
	case *ast.ExpressionStmt:
		return checkExpressionStmtNode(n, table)
	case *ast.BinaryExpr:
		return checkBinaryExprNode(n, table)
	case *ast.VarDeclStmt:
		return resolveVarDeclStmtNode(n, table)
	case *ast.TypeDeclStmt:
		return checkTypeDeclStmtNode(n, table)
	case *ast.FunctionDecl:
		return checkFunctionDeclNode(n, table)
	case *ast.FunctionCallExpr:
		return checkFunctionCallExpr(n, table)
	case *ast.FieldAccessExpr:
		return checkFieldAccessExpr(n, table)
	default:
		report.Add(table.Filepath, n.Loc(), colors.BROWN.Sprintf("typechecker: <%T> node is not implemented\n", n)).SetLevel(report.CRITICAL_ERROR)
		return nil
	}
}
