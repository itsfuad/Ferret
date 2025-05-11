package resolver

import (
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/middleend"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
)

func handleImportStmt(node *ast.ImportStmt, st *symboltable.SymbolTable) {
	//start lexer, parser and collector for the imported file
	//module.CompileModule(node.ImportPath.Value, true, true)
	filepath := node.ImportPath.Value

	_, err := middleend.CompileModule(filepath, true, true)

	if err != nil {
		report.Add(st.Filepath, &node.ImportPath.Location, err.Error())
		return
	}

	// TODO
	// Implement resolver
}
