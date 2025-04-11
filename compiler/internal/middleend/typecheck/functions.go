package typecheck

import (
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"ferret/compiler/types"
	"fmt"
)

func checkFunctionLiteral(functionLiteral *ast.FunctionLiteral, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	//params := functionLiteral.Params
	//body := functionLiteral.Body

	return &symboltable.FunctionType{
		TypeName: types.FUNCTION,
	}
}

func checkFunctionDeclNode(functionDecl *ast.FunctionDecl, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	scope := table.EnterScope(symboltable.FUNCTION_SCOPE)
	defer scope.ExitScope()
	//check function params
	for _, param := range functionDecl.Function.Params {
		paramType := ASTNodeToAnalyzerNode(param.Type, scope)
		scope.Define(&symboltable.Symbol{
			Name:       param.Identifier.Name,
			SymbolKind: symboltable.VARIABLE_SYMBOL,
			IsMutable:  true,
			FilePath:   table.Filepath,
			Location:   param.Identifier.Loc(),
			SymbolType: paramType,
		})
	}

	//check return types
	returnTypes := make([]*symboltable.AnalyzerNode, 0)

	for _, returnType := range functionDecl.Function.ReturnType {
		returnType := ASTNodeToAnalyzerNode(returnType, scope)
		returnTypes = append(returnTypes, &returnType)
	}

	fmt.Printf("Return types: %#v\n", returnTypes)

	//body
	for _, stmt := range functionDecl.Function.Body.Nodes {
		ASTNodeToAnalyzerNode(stmt, scope)
	}

	funcSymbol := &symboltable.Symbol{
		Name:       functionDecl.Identifier.Name,
		SymbolKind: symboltable.VARIABLE_SYMBOL,
		IsMutable:  false,
		FilePath:   table.Filepath,
		Location:   functionDecl.Loc(),
		SymbolType: &symboltable.FunctionType{
			TypeName: types.FUNCTION,
		},
	}

	scope.Define(funcSymbol)

	return nil
}

func checkFunctionCallExpr(functionCallExpr *ast.FunctionCallExpr, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	caller := ASTNodeToAnalyzerNode(functionCallExpr.Caller, table)

	if caller.ANode() != types.FUNCTION {
		report.Add(table.Filepath, functionCallExpr.Loc(), fmt.Sprintf("expression is `%s` not a function", caller.ANode())).SetLevel(report.CRITICAL_ERROR)
		return nil
	}

	return caller
}
