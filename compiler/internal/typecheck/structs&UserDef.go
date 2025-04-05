package typecheck

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/source"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"ferret/compiler/types"
	"fmt"
)

func checkUserDefinedType(userDefinedType *ast.UserDefinedType, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	sym, ok := table.Resolve(string(userDefinedType.TypeName))

	fmt.Printf("User defined type: %s\n", string(sym.Name))

	if !ok {
		report.Add(table.Filepath, userDefinedType.Loc(), "type "+string(userDefinedType.TypeName)+" not found").SetLevel(report.NORMAL_ERROR)
		return nil
	}

	if sym.SymbolKind != symboltable.TYPE_SYMBOL {
		report.Add(table.Filepath, userDefinedType.Loc(), string(userDefinedType.TypeName)+" is not a type").SetLevel(report.NORMAL_ERROR)
		return nil
	}

	return sym.SymbolType
}

func checkStructLiteral(structLiteral *ast.StructLiteralExpr, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	sym, ok := table.Resolve(structLiteral.StructName.Name)

	scope := table.EnterScope(symboltable.STRUCT_SCOPE)
	defer table.ExitScope()

	fieldLocations := make(map[string]*source.Location)

	for _, field := range structLiteral.Fields {
		fieldValue := ASTNodeToAnalyzerNode(field.FieldValue, scope)
		scope.Define(&symboltable.Symbol{
			Name:       field.FieldIdentifier.Name,
			SymbolKind: symboltable.VARIABLE_SYMBOL,
			IsMutable:  true,
			FilePath:   table.Filepath,
			Location:   field.FieldIdentifier.Loc(),
			SymbolType: fieldValue,
		})

		fieldLocations[field.FieldIdentifier.Name] = field.FieldIdentifier.Loc()
	}

	if structLiteral.IsAnonymous {
		return &symboltable.StructType{
			TypeName: types.STRUCT,
			Scope:    scope,
		}
	}

	unwrappedSym := symboltable.UnwrapUserDefType(&sym.SymbolType)

	if !structLiteral.IsAnonymous {

		if !ok {
			report.Add(table.Filepath, structLiteral.StructName.Loc(), fmt.Sprintf("`%s` is not defined as a struct anywhere", structLiteral.StructName.Name)).AddHint("Did you forget to declare this struct?").SetLevel(report.CRITICAL_ERROR)
			return nil
		}

		//check if the symbol is a type
		if sym.SymbolKind != symboltable.TYPE_SYMBOL {
			report.Add(table.Filepath, structLiteral.StructName.Loc(), fmt.Sprintf("`%s` is not a type", structLiteral.StructName.Name)).SetLevel(report.CRITICAL_ERROR)
			return nil
		}

		fmt.Printf("Unwrapped sym: %T\n", unwrappedSym)

		//check if the symbol is a struct
		if _, ok := unwrappedSym.(*symboltable.StructType); !ok {
			report.Add(table.Filepath, structLiteral.StructName.Loc(), fmt.Sprintf("struct %s is not a struct", structLiteral.StructName.Name)).SetLevel(report.CRITICAL_ERROR)
			return nil
		}
	}

	structType := &symboltable.StructType{
		TypeName: types.STRUCT,
		Scope:    scope,
	}

	fmt.Printf("Struct name: %s\n", structLiteral.StructName.Name)

	checkPropsType(structLiteral.StructName.Name, unwrappedSym.(*symboltable.StructType), structType, fieldLocations, table)

	return structType
}

// check if the props are defined in the declared struct and the types match
func checkPropsType(structName string, declared, provided *symboltable.StructType, locs map[string]*source.Location, table *symboltable.SymbolTable) {

	for _, field := range provided.Scope.Symbols {
		//check if defined in declared struct
		fieldType, found := declared.Scope.ResolveLocal(field.Name)
		if !found {
			report.Add(table.Filepath, locs[field.Name], fmt.Sprintf("error in struct literal `@%s`: field %s is not defined in the struct", structName, field.Name)).SetLevel(report.NORMAL_ERROR)
		}

		//check if types match
		if ok, err := isCompatible(fieldType.SymbolType, field.SymbolType); !ok {
			report.Add(table.Filepath, locs[field.Name], fmt.Sprintf("error in struct literal `@%s`: field %s: %s", structName, field.Name, err.Error())).SetLevel(report.NORMAL_ERROR)
		}
	}
}

func checkStructType(structType *ast.StructType, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	scope := table.EnterScope(symboltable.STRUCT_SCOPE)

	for _, field := range structType.Fields {
		fieldValue := ASTNodeToAnalyzerNode(field.FieldType, scope)
		scope.Define(&symboltable.Symbol{
			Name:       field.FieldIdentifier.Name,
			SymbolKind: symboltable.VARIABLE_SYMBOL,
			IsMutable:  true,
			FilePath:   table.Filepath,
			Location:   field.FieldIdentifier.Loc(),
			SymbolType: fieldValue,
		})
	}

	return &symboltable.StructType{
		TypeName: types.STRUCT,
		Scope:    scope,
	}
}
