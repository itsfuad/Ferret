package typecheck

import (
	"ferret/compiler/colors"
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"ferret/compiler/types"
	"fmt"
)

func checkTypeDeclStmtNode(typeDeclStmt *ast.TypeDeclStmt, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	alias := typeDeclStmt.Alias

	baseType := ASTNodeToAnalyzerNode(typeDeclStmt.BaseType, table)

	symbol := &symboltable.Symbol{
		Name:       alias.Name,
		SymbolKind: symboltable.TYPE_SYMBOL,
		IsMutable:  false,
		FilePath:   table.Filepath,
		Location:   alias.Loc(),
		SymbolType: &symboltable.UserDefType{
			TypeName:       types.TYPE_NAME(alias.Name),
			UnderlyingType: baseType,
		},
	}

	if !table.Define(symbol) {
		report.Add(table.Filepath, alias.Loc(), fmt.Sprintf("couldn't define type %s", alias.Name)).SetLevel(report.NORMAL_ERROR)
	}

	return nil
}

func checkVarDeclStmtNode(varDeclStmt *ast.VarDeclStmt, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	initializerNodes := varDeclStmt.Initializers

	//first get the type of the initializers
	var inits []symboltable.AnalyzerNode
	for _, initializer := range initializerNodes {
		inits = append(inits, ASTNodeToAnalyzerNode(initializer, table))
	}

	// if explicit type is provided, check if it matches the type of the initializer, otherwise set the type of initializer to the table
	for i, variableNode := range varDeclStmt.Variables {
		if variableNode.ExplicitType != nil {
			// match the type of the initializer to the explicit type
			fmt.Printf("Explicit type: %T\n", variableNode.ExplicitType)
			explicitType := ASTNodeToAnalyzerNode(variableNode.ExplicitType, table)

			colors.PURPLE.Printf("Got explicit type: %T\n", explicitType)
			colors.PURPLE.Printf("Got initializer type: %T\n", inits[i])

			if ok, err := isCompatible(explicitType, inits[i]); !ok {
				report.Add(table.Filepath, initializerNodes[i].Loc(), err.Error()).SetLevel(report.NORMAL_ERROR)
			}

			//when explicit type is provided, the initializer must use that type
			//so we update the initializer type to the explicit type
			colors.BLUE.Printf("Updated initializer type: %T to %T\n", inits[i], explicitType)
			inits[i] = explicitType
		}
	}

	//all ok now update the table
	for i, variableNode := range varDeclStmt.Variables {
		fmt.Printf("Defining variable: %s with type: %s\n", variableNode.Identifier.Name, inits[i].ToString())
		symbol := &symboltable.Symbol{
			Name:       variableNode.Identifier.Name,
			SymbolKind: symboltable.VARIABLE_SYMBOL,
			IsMutable:  !varDeclStmt.IsConst,
			FilePath:   table.Filepath,
			Location:   variableNode.Identifier.Loc(),
			SymbolType: inits[i],
		}

		if !table.Define(symbol) {
			report.Add(table.Filepath, variableNode.Identifier.Loc(), fmt.Sprintf("couldn't define variable %s", variableNode.Identifier.Name)).SetLevel(report.NORMAL_ERROR)
		}
	}

	return nil
}
