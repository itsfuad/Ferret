package typecheck

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"ferret/compiler/types"
	"fmt"
)

func checkFunctionLiteral(functionLiteral *ast.FunctionLiteral, table *symboltable.SymbolTable) *symboltable.Symbol {

	//params := functionLiteral.Params
	//body := functionLiteral.Body

	return &symboltable.Symbol{
		Name:       string(types.FUNCTION),
		SymbolKind: symboltable.TYPE_SYMBOL,
		IsMutable:  false,
		FilePath:   table.Filepath,
		Location:   functionLiteral.Loc(),
		SymbolType: &symboltable.FunctionType{
			TypeName: types.FUNCTION,
		},
	}
}



func checkFunctionDeclNode(functionDecl *ast.FunctionDecl, table *symboltable.SymbolTable) *symboltable.Symbol {

	fmt.Printf("Function decl: %s\n", functionDecl.Identifier.Name)

	table.EnterScope(symboltable.FUNCTION_SCOPE)
	//check function params
	for _, param := range functionDecl.Function.Params {

		paramType := ASTNodeToAnalyzerNode(param.Type, table)
		table.Define(&symboltable.Symbol{
			Name:       param.Identifier.Name,
			SymbolKind: symboltable.VARIABLE_SYMBOL,
			IsMutable:  true,
			FilePath:   table.Filepath,
			Location:   param.Identifier.Loc(),
			SymbolType: paramType.SymbolType,
		})
	}


	return nil
}

func checkFunctionCallExpr(functionCallExpr *ast.FunctionCallExpr, table *symboltable.SymbolTable) *symboltable.Symbol {

	caller := ASTNodeToAnalyzerNode(functionCallExpr.Caller, table)

	if caller.SymbolType.ANode() != types.FUNCTION {
		report.Add(table.Filepath, functionCallExpr.Loc(), fmt.Sprintf("expression is `%s` not a function", caller.SymbolType.ToString())).SetLevel(report.CRITICAL_ERROR)
		return nil
	}

	return caller
}