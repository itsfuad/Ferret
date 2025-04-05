package typecheck

import (
	"fmt"
	"ferret/compiler/colors"
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
)

func checkTypeDeclStmtNode(typeDeclStmt *ast.TypeDeclStmt, table *symboltable.SymbolTable) *symboltable.Symbol {

	alias := typeDeclStmt.Alias

	baseType := ASTNodeToAnalyzerNode(typeDeclStmt.BaseType, table)

	symbol := &symboltable.Symbol{
		Name:       alias.Name,
		SymbolKind: symboltable.TYPE_SYMBOL,
		IsMutable:  false,
		FilePath:   table.Filepath,
		Location:   alias.Loc(),
		SymbolType: baseType.SymbolType,
	}

	if !table.Define(symbol) {
		report.Add(table.Filepath, alias.Loc(), fmt.Sprintf("couldn't define type %s", alias.Name)).SetLevel(report.NORMAL_ERROR)
	}

	return nil
}

func checkVarDeclStmtNode(varDeclStmt *ast.VarDeclStmt, table *symboltable.SymbolTable) *symboltable.Symbol {

	initializerNodes := varDeclStmt.Initializers

	//first get the type of the initializers
	var inits []*symboltable.Symbol
	for _, initializer := range initializerNodes {
		var node ast.Node = initializer
		inits = append(inits, ASTNodeToAnalyzerNode(node, table))
	}

	// if explicit type is provided, check if it matches the type of the initializer, otherwise set the type of initializer to the table
	for i, variableNode := range varDeclStmt.Variables {
		if variableNode.ExplicitType != nil {
			// match the type of the initializer to the explicit type
			init := inits[i]
			fmt.Printf("Explicit type: %T\n", variableNode.ExplicitType)
			explicitType := ASTNodeToAnalyzerNode(variableNode.ExplicitType.INode(), table)

			colors.PURPLE.Printf("Got explicit type: %T\n", explicitType)
			colors.PURPLE.Printf("Got initializer type: %T\n", init)

			if ok, err := isCompatible(explicitType, init); !ok {
				report.Add(table.Filepath, initializerNodes[i].Loc(), err.Error()).SetLevel(report.NORMAL_ERROR)
			}
		}
	}

	//all ok now update the table
	for i, variableNode := range varDeclStmt.Variables {
		symbol := &symboltable.Symbol{
			Name:       variableNode.Identifier.Name,
			SymbolKind: symboltable.VARIABLE_SYMBOL,
			IsMutable:  !varDeclStmt.IsConst,
			FilePath:   table.Filepath,
			Location:   variableNode.Identifier.Loc(),
			SymbolType: inits[i].SymbolType,
		}

		if !table.Define(symbol) {
			report.Add(table.Filepath, variableNode.Identifier.Loc(), fmt.Sprintf("couldn't define variable %s", variableNode.Identifier.Name)).SetLevel(report.NORMAL_ERROR)
		}
	}

	return nil
}