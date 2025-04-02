package typecheck

import (
	"ferret/compiler/colors"
	"ferret/compiler/internal/analyzer"
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"ferret/compiler/types"
	"fmt"
)

func checkTypeNames(node ast.Node, table *symboltable.SymbolTable) analyzer.AnalyzerNode {
	switch n := node.(type) {
	case *ast.IntType:
		return &analyzer.IntegerType{
			TypeName:   n.TypeName,
			BitSize:    n.BitSize,
			IsUnsigned: n.IsUnsigned,
		}
	case *ast.FloatType:
		return &analyzer.FloatType{
			TypeName: n.TypeName,
			BitSize:  n.BitSize,
		}
	case *ast.StringType:
		return &analyzer.StringType{
			TypeName: n.TypeName,
		}
	case *ast.BoolType:
		return &analyzer.BoolType{
			TypeName: n.TypeName,
		}
	case *ast.ByteType:
		return &analyzer.ByteType{
			TypeName: n.TypeName,
		}
	case *ast.ArrayType:
		return &analyzer.ArrayType{
			TypeName: n.TypeName,
		}
	case *ast.StructType:
		return checkStructType(n, table)
	default:
		return nil
	}
}

func checkStructType(structType *ast.StructType, table *symboltable.SymbolTable) analyzer.AnalyzerNode {

	var fields []analyzer.StructField

	for _, structField := range structType.Fields {
		fieldType := ASTNodeToAnalyzerNode(structField.FieldType, table)
		field := analyzer.StructField{
			Name: structField.FieldIdentifier.Name,
			Type: fieldType,
		}
		fields = append(fields, field)
	}

	return &analyzer.StructType{
		TypeName: structType.TypeName,
		Fields:   fields,
	}
}

func getLiteralType(lit ast.Node, table *symboltable.SymbolTable) analyzer.AnalyzerNode {
	switch n := lit.(type) {
	case *ast.IntLiteral:
		return &analyzer.IntegerType{
			TypeName:   types.INT64,
			BitSize:    types.GetNumberBitSize(types.INT64),
			IsUnsigned: true,
		}
	case *ast.FloatLiteral:
		return &analyzer.FloatType{
			TypeName: types.FLOAT64,
			BitSize:  types.GetNumberBitSize(types.FLOAT64),
		}
	case *ast.StringLiteral:
		return &analyzer.StringType{
			TypeName: types.STRING,
		}
	case *ast.BoolLiteral:
		return &analyzer.BoolType{
			TypeName: types.BOOL,
		}
	case *ast.ByteLiteral:
		return &analyzer.ByteType{
			TypeName: types.BYTE,
		}
	case *ast.StructLiteralExpr:
		return checkStructLiteral(n, table)
	}

	return nil
}

func checkStructLiteral(structLiteral *ast.StructLiteralExpr, table *symboltable.SymbolTable) analyzer.AnalyzerNode {

	var fields []analyzer.StructField

	for _, field := range structLiteral.Fields {
		fieldType := ASTNodeToAnalyzerNode(field.FieldValue, table)
		field := analyzer.StructField{
			Name: field.FieldIdentifier.Name,
			Type: fieldType,
		}
		fields = append(fields, field)
	}

	return &analyzer.StructType{
		TypeName: types.STRUCT,
		Fields:   fields,
	}
}

func ASTNodeToAnalyzerNode(node ast.Node, table *symboltable.SymbolTable) analyzer.AnalyzerNode {

	fmt.Printf("Node: %T\n", node)

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
	case *ast.VarDeclStmt:
		return checkVarDeclStmtNode(n, table)
	default:
		report.Add(table.Filepath, n.Loc(), colors.BROWN.Sprintf("typechecker: <%T> node is not implemented\n", n)).SetLevel(report.NORMAL_ERROR)
		return nil
	}
}

func checkVarDeclStmtNode(varDeclStmt *ast.VarDeclStmt, table *symboltable.SymbolTable) analyzer.AnalyzerNode {

	initializerNodes := varDeclStmt.Initializers

	//first get the type of the initializers
	var inits []analyzer.AnalyzerNode
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
		if ok := table.UpdateSymbolType(variableNode.Identifier.Name, inits[i]); !ok {
			report.Add(table.Filepath, variableNode.Identifier.Loc(), fmt.Sprintf("couldn't update symbol type for variable %s", variableNode.Identifier.Name)).SetLevel(report.NORMAL_ERROR)
		}
	}

	return nil
}

func checkExpressionStmtNode(expressionStmt *ast.ExpressionStmt, table *symboltable.SymbolTable) analyzer.AnalyzerNode {
	for _, expr := range *expressionStmt.Expressions {
		ASTNodeToAnalyzerNode(expr, table)
	}
	return nil
}

func ckeckIdentifierNode(identifier *ast.IdentifierExpr, table *symboltable.SymbolTable) analyzer.AnalyzerNode {

	symbol, ok := table.Resolve(identifier.Name)

	if !ok {
		report.Add(table.Filepath, identifier.Loc(), "Identifier "+identifier.Name+" not found").AddHint("Did you forget to declare this variable?").SetLevel(report.NORMAL_ERROR)
		return nil
	}

	if symbol.SymbolKind != symboltable.VARIABLE_SYMBOL {
		report.Add(table.Filepath, symbol.Location, "Identifier "+identifier.Name+" is not a variable").SetLevel(report.NORMAL_ERROR)
		return nil
	}

	if symbol.SymbolType == nil {
		panic(fmt.Sprintf("Symbol %s is nil. Implement table update", identifier.Name))
	}

	return symbol.SymbolType
}
