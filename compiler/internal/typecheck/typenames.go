package typecheck

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/types"
)

func getLiteralType(lit ast.Node, table *symboltable.SymbolTable) *symboltable.Symbol {
	switch n := lit.(type) {
	case *ast.IntLiteral:
		return &symboltable.Symbol{
			Name:       string(types.INT64),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.IntegerType{
				TypeName:   types.INT64,
				BitSize:    types.GetNumberBitSize(types.INT64),
				IsUnsigned: true,
			},
		}
	case *ast.FloatLiteral:
		return &symboltable.Symbol{
			Name:       string(types.FLOAT64),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.FloatType{
				TypeName: types.FLOAT64,
				BitSize:  types.GetNumberBitSize(types.FLOAT64),
			},
		}
	case *ast.StringLiteral:
		return &symboltable.Symbol{
			Name:       string(types.STRING),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.StringType{
				TypeName: types.STRING,
			},
		}
	case *ast.BoolLiteral:
		return &symboltable.Symbol{
			Name:       string(types.BOOL),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.BoolType{
				TypeName: types.BOOL,
			},
		}
	case *ast.ByteLiteral:
		return &symboltable.Symbol{
			Name:       string(types.BYTE),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.ByteType{
				TypeName: types.BYTE,
			},
		}
	case *ast.StructLiteralExpr:
		return checkStructLiteral(n, table)
	case *ast.FunctionLiteral:
		return checkFunctionLiteral(n, table)
	}

	return nil
}

func checkTypeNames(node ast.Node, table *symboltable.SymbolTable) *symboltable.Symbol {
	switch n := node.(type) {
	case *ast.IntType:
		return &symboltable.Symbol{
			Name:       string(types.INT64),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.IntegerType{
				TypeName:   n.TypeName,
				BitSize:    n.BitSize,
				IsUnsigned: n.IsUnsigned,
			},
		}
	case *ast.FloatType:
		return &symboltable.Symbol{
			Name:       string(types.FLOAT64),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.FloatType{
				TypeName: n.TypeName,
				BitSize:  n.BitSize,
			},
		}
	case *ast.StringType:
		return &symboltable.Symbol{
			Name:       string(types.STRING),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.StringType{
				TypeName: n.TypeName,
			},
		}
	case *ast.BoolType:
		return &symboltable.Symbol{
			Name:       string(types.BOOL),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.BoolType{
				TypeName: n.TypeName,
			},
		}
	case *ast.ByteType:
		return &symboltable.Symbol{
			Name:       string(types.BYTE),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.ByteType{
				TypeName: n.TypeName,
			},
		}
	case *ast.ArrayType:
		return &symboltable.Symbol{
			Name:       string(types.ARRAY),
			SymbolKind: symboltable.TYPE_SYMBOL,
			IsMutable:  false,
			FilePath:   table.Filepath,
			Location:   n.Loc(),
			SymbolType: &symboltable.ArrayType{
				TypeName: n.TypeName,
			},
		}
	case *ast.StructType:
		return checkStructType(n, table)
	case *ast.UserDefinedType:
		return checkUserDefinedType(n, table)
	default:
		return nil
	}
}
