package resolver

import (
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"ferret/compiler/types"
	"fmt"
)

func checkBinaryExprNode(binaryExpr *ast.BinaryExpr, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	left := ASTNodeToAnalyzerNode(binaryExpr.Left, table)
	right := ASTNodeToAnalyzerNode(binaryExpr.Right, table)

	if ok, err := isCompatible(left, right); !ok {
		report.Add(table.Filepath, binaryExpr.Loc(), fmt.Sprintf("cannot perform binary %s operation between %s and %s: %s", binaryExpr.Operator.Value, left.ANode(), right.ANode(), err.Error())).SetLevel(report.CRITICAL_ERROR)
		return nil
	} else {
		return left
	}
}

func checkFieldAccessExpr(fieldAccessExpr *ast.FieldAccessExpr, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	//check if the object is a struct

	objType := ASTNodeToAnalyzerNode(fieldAccessExpr.Object, table)

	unwrappedObjType := symboltable.UnwrapUserDefType(&objType)

	if unwrappedObjType.ANode() != types.STRUCT {
		report.Add(table.Filepath, fieldAccessExpr.Loc(), fmt.Sprintf("expression is `%s` not a struct", objType.ANode())).SetLevel(report.CRITICAL_ERROR)
		return nil
	}

	//check if the field is defined
	structType := unwrappedObjType.(*symboltable.StructType)

	fieldType, found := structType.Scope.ResolveLocal(fieldAccessExpr.Field.Name)

	if !found {
		report.Add(table.Filepath, fieldAccessExpr.Loc(), fmt.Sprintf("field %s is not defined in %s", fieldAccessExpr.Field.Name, objType.ToString())).SetLevel(report.CRITICAL_ERROR)
		return nil
	}

	return fieldType.SymbolType
}

func checkExpressionStmtNode(expressionStmt *ast.ExpressionStmt, table *symboltable.SymbolTable) symboltable.AnalyzerNode {
	for _, expr := range *expressionStmt.Expressions {
		ASTNodeToAnalyzerNode(expr, table)
	}
	return nil
}

func ckeckIdentifierNode(identifier *ast.IdentifierExpr, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	symbol, ok := table.Resolve(identifier.Name)

	if !ok {
		report.Add(table.Filepath, identifier.Loc(), identifier.Name+" not found").AddHint("Did you forget to declare this variable?").SetLevel(report.NORMAL_ERROR)
		return nil
	}

	if symbol.SymbolKind != symboltable.VARIABLE_SYMBOL {
		report.Add(table.Filepath, symbol.Location, identifier.Name+" is not a variable").SetLevel(report.NORMAL_ERROR)
		return nil
	}

	if symbol.SymbolType == nil {
		panic(fmt.Sprintf("Symbol %s is nil. Implement table update", identifier.Name))
	}

	return symbol.SymbolType
}
