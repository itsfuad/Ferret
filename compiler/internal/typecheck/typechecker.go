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

func builtinTypes(node *ast.Node) analyzer.AnalyzerNode {
	switch n := (*node).(type) {
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
	default:
		return nil
	}
}

func ASTNodeToAnalyzerNode(table *symboltable.SymbolTable, node *ast.Node) analyzer.AnalyzerNode {

	// Check if the node is a builtin type
	if t := builtinTypes(node); t != nil {
		return t
	}

	switch n := (*node).(type) {
	case *ast.IdentifierExpr:
		return getIdentifierNode(n, table)
	default:
		report.Add(table.Filepath, n.Loc(), colors.BROWN.Sprintf("typechecker: <%T> node is not implemented\n", n)).SetLevel(report.NORMAL_ERROR)
		return nil
	}
}

func getIdentifierNode(identifier *ast.IdentifierExpr, table *symboltable.SymbolTable) analyzer.AnalyzerNode {

	symbol, ok := table.Resolve(identifier.Name)

	if !ok {
		fmt.Printf("Identifier %s not found\n", identifier.Name)
		return nil
	}

	if symbol.SymbolKind != symboltable.VARIABLE_SYMBOL {
		report.Add(table.Filepath, symbol.Location, "Identifier "+identifier.Name+" is not a variable").SetLevel(report.NORMAL_ERROR)
		return nil
	}

	return symbol.SymbolType
}

func numericTypeNameToAnalyzerNode(typeName types.TYPE_NAME) analyzer.AnalyzerNode {
	switch typeName {
	case types.INT8, types.INT16, types.INT32, types.INT64, types.UINT8, types.UINT16, types.UINT32, types.UINT64:
		return &analyzer.IntegerType{
			TypeName:   typeName,
			BitSize:    types.GetNumberBitSize(typeName),
			IsUnsigned: types.IsUnsigned(typeName),
		}
	case types.FLOAT32, types.FLOAT64:
		return &analyzer.FloatType{
			TypeName: typeName,
			BitSize:  types.GetNumberBitSize(typeName),
		}
	}
	return nil
}
