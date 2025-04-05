package typecheck

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"ferret/compiler/types"
	"fmt"
)

func checkBinaryExprNode(binaryExpr *ast.BinaryExpr, table *symboltable.SymbolTable) *symboltable.Symbol {

	left := ASTNodeToAnalyzerNode(binaryExpr.Left, table)
	right := ASTNodeToAnalyzerNode(binaryExpr.Right, table)

	if ok, err := isCompatible(left, right); !ok {
		report.Add(table.Filepath, binaryExpr.Loc(), fmt.Sprintf("cannot perform binary %s operation between %s and %s: %s", binaryExpr.Operator.Value, left.SymbolType.ToString(), right.SymbolType.ToString(), err.Error())).SetLevel(report.CRITICAL_ERROR)
		return nil
	} else {
		return left
	}
}

func checkFieldAccessExpr(fieldAccessExpr *ast.FieldAccessExpr, table *symboltable.SymbolTable) *symboltable.Symbol {

	//check if the object is a struct

	objType := ASTNodeToAnalyzerNode(fieldAccessExpr.Object, table)

	fmt.Printf("Field access expr: %s\n", objType.SymbolType.ToString())

	if objType.SymbolType.ANode() != types.STRUCT {
		report.Add(table.Filepath, fieldAccessExpr.Loc(), fmt.Sprintf("expression is `%s` not a struct", objType.SymbolType.ToString())).SetLevel(report.CRITICAL_ERROR)
		return nil
	}

	//check if the field is defined
	structType := objType.SymbolType.(*symboltable.StructType)

	fieldType, found := structType.Scope.ResolveLocal(fieldAccessExpr.Field.Name)

	if !found {
		report.Add(table.Filepath, fieldAccessExpr.Loc(), fmt.Sprintf("field %s is not defined in `%s`", fieldAccessExpr.Field.Name, structType.ToString())).SetLevel(report.CRITICAL_ERROR)
		return nil
	}

	return fieldType
}


func checkExpressionStmtNode(expressionStmt *ast.ExpressionStmt, table *symboltable.SymbolTable) *symboltable.Symbol {
	for _, expr := range *expressionStmt.Expressions {
		ASTNodeToAnalyzerNode(expr, table)
	}
	return nil
}

func ckeckIdentifierNode(identifier *ast.IdentifierExpr, table *symboltable.SymbolTable) *symboltable.Symbol {

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

	return symbol
}
