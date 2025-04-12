package resolver

import (
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/middleend"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
)

func Resolve(program *ast.Program, st *symboltable.SymbolTable) {
	for _, node := range program.Nodes {
		switch n := node.(type) {
		case *ast.VarDeclStmt:
			//handle variable declaration
			resolveVarDeclStmtNode(n, st)
		case *ast.ImportStmt:
			//handle import statement
			handleImportStmt(n, st)
		}
	}
}

func handleImportStmt(node *ast.ImportStmt, st *symboltable.SymbolTable) {
	//start lexer, parser and collector for the imported file
	//module.CompileModule(node.ImportPath.Value, true, true)
	filepath := node.ImportPath.Value

	tree, importedSt, err := middleend.CompileModule(filepath, true, true)

	if err != nil {
		report.Add(st.Filepath, &node.ImportPath.Location, err.Error())
		return
	}

	Resolve(tree, importedSt)
}
