package collector

import (
	"ferret/compiler/colors"
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"fmt"
)

func CollectSymbols(nodes *[]ast.Node, filepath string) *symboltable.SymbolTable {

	//collect declarations for types and variables

	//create a new symbol table for the module
	st := symboltable.NewSymbolTable(nil, symboltable.MODULE_SCOPE, filepath)

	for _, node := range *nodes {
		//visit the node
		switch n := node.(type) {
		case *ast.VarDeclStmt:
			//handle variable declaration
			handleVarDeclStmt(n, st)
		case *ast.TypeDeclStmt:
			//handle type declaration
			handleTypeDeclStmt(n, st)
		}
	}

	colors.BROWN.Println("End of the tree. Collecting symbols done.")
	return st
}

func handleVarDeclStmt(node *ast.VarDeclStmt, st *symboltable.SymbolTable) {
	for _, variable := range node.Variables {
		symbol := &symboltable.Symbol{
			Name:       variable.Identifier.Name,
			SymbolKind: symboltable.VARIABLE_SYMBOL,
			IsMutable:  !node.IsConst,
			FilePath:   st.Filepath,
			Location:   &variable.Identifier.Location,
		}

		if !st.Define(symbol) {
			//error: variable already defined
			report.Add(st.Filepath, &variable.Identifier.Location, fmt.Sprintf("Variable '%s' already defined", variable.Identifier.Name))
		}
	}
}

func handleTypeDeclStmt(node *ast.TypeDeclStmt, st *symboltable.SymbolTable) {

	//create a new symbol for the type
	symbol := &symboltable.Symbol{
		Name:       node.Alias.Name,
		SymbolKind: symboltable.TYPE_SYMBOL,
		IsMutable:  false,
		FilePath:   st.Filepath,
		Location:   &node.Location,
	}

	if !st.Define(symbol) {
		//error: type already defined
		report.Add(st.Filepath, &node.Location, fmt.Sprintf("Type '%s' already defined", node.Alias.Name))
	}
}
