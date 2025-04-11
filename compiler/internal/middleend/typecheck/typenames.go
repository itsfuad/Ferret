package typecheck

import (
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/types"
	"fmt"
)

func getLiteralType(lit ast.Node, table *symboltable.SymbolTable) symboltable.AnalyzerNode {
	switch n := lit.(type) {
	case *ast.IntLiteral:
		return &symboltable.IntegerType{
			TypeName:   types.INT64,
			BitSize:    types.GetNumberBitSize(types.INT64),
			IsUnsigned: true,
		}
	case *ast.FloatLiteral:
		return &symboltable.FloatType{
			TypeName: types.FLOAT64,
			BitSize:  types.GetNumberBitSize(types.FLOAT64),
		}
	case *ast.StringLiteral:
		return &symboltable.StringType{
			TypeName: types.STRING,
		}
	case *ast.BoolLiteral:
		return &symboltable.BoolType{
			TypeName: types.BOOL,
		}
	case *ast.ByteLiteral:
		return &symboltable.ByteType{
			TypeName: types.BYTE,
		}
	case *ast.StructLiteralExpr:
		return checkStructLiteral(n, table)
	case *ast.FunctionLiteral:
		return checkFunctionLiteral(n, table)
	}

	return nil
}

func checkTypeNames(node ast.Node, table *symboltable.SymbolTable) symboltable.AnalyzerNode {
	switch n := node.(type) {
	case *ast.IntType:
		return &symboltable.IntegerType{
			TypeName:   n.TypeName,
			BitSize:    n.BitSize,
			IsUnsigned: n.IsUnsigned,
		}
	case *ast.FloatType:
		return &symboltable.FloatType{
			TypeName: n.TypeName,
			BitSize:  n.BitSize,
		}
	case *ast.StringType:
		return &symboltable.StringType{
			TypeName: n.TypeName,
		}
	case *ast.BoolType:
		return &symboltable.BoolType{
			TypeName: n.TypeName,
		}
	case *ast.ByteType:
		return &symboltable.ByteType{
			TypeName: n.TypeName,
		}
	case *ast.ArrayType:
		return &symboltable.ByteType{
			TypeName: n.TypeName,
		}
	case *ast.StructType:
		fmt.Printf("Struct type: %s\n", n.TypeName)
		return checkStructType(n, table)
	case *ast.UserDefinedType:
		fmt.Printf("User defined type: %s\n", n.TypeName)
		return checkUserDefinedType(n, table)
	default:
		return nil
	}
}
