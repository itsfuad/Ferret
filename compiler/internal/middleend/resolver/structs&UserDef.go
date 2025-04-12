package resolver

import (
	"ferret/compiler/colors"
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/source"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/report"
	"ferret/compiler/types"
	"fmt"
)

func checkUserDefinedType(userDefinedType *ast.UserDefinedType, table *symboltable.SymbolTable) symboltable.AnalyzerNode {

	sym, ok := table.Resolve(string(userDefinedType.TypeName))

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

	checkPropsType(structLiteral.StructName.Name, unwrappedSym.(*symboltable.StructType), structType, fieldLocations, table)

	return sym.SymbolType
}

// check if the props are defined in the declared struct and the types match
func checkPropsType(structName string, declared, provided *symboltable.StructType, locs map[string]*source.Location, table *symboltable.SymbolTable) {

	for _, field := range provided.Scope.Symbols {
		//check if defined in declared struct
		fieldType, found := declared.Scope.ResolveLocal(field.Name)
		if !found {
			report.Add(table.Filepath, locs[field.Name], fmt.Sprintf("error in struct literal of type `%s`: field %s is not defined in the struct", structName, field.Name)).SetLevel(report.NORMAL_ERROR)
		}

		//check if types match
		if ok, err := isCompatible(fieldType.SymbolType, field.SymbolType); !ok {
			report.Add(table.Filepath, locs[field.Name], fmt.Sprintf("error in struct literal of type `%s`: field %s: %s", structName, field.Name, err.Error())).SetLevel(report.NORMAL_ERROR)
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

func checkStructCompatibility(first, second *symboltable.AnalyzerNode) (bool, error) {
	unwrappedFirst := symboltable.UnwrapUserDefType(first)
	unwrappedSecond := symboltable.UnwrapUserDefType(second)

	firstStruct := unwrappedFirst.(*symboltable.StructType)
	secondStruct := unwrappedSecond.(*symboltable.StructType)

	missingFields, extraFields, allFirstFields, allSecondFields := compareStructFields(firstStruct, secondStruct)

	if errorString := buildFieldErrorString(missingFields, extraFields); errorString != "" {
		firstString := (*first).ToString()
		secondString := (*second).ToString()
		return false, fmt.Errorf("cannot use %s as %s\nReason:\n%s", firstString, secondString, errorString)
	}

	return checkFieldTypeCompatibility(allFirstFields, allSecondFields)
}

func compareStructFields(first, second *symboltable.StructType) (map[string]bool, map[string]bool, map[string]symboltable.AnalyzerNode, map[string]symboltable.AnalyzerNode) {
	missingFields := make(map[string]bool)
	extraFields := make(map[string]bool)
	allFirstFields := make(map[string]symboltable.AnalyzerNode)
	allSecondFields := make(map[string]symboltable.AnalyzerNode)

	for _, field := range first.Scope.GetSymbols() {
		if _, ok := second.Scope.ResolveLocal(field.Name); !ok {
			missingFields[field.Name] = true
		}
		allFirstFields[field.Name] = field.SymbolType
	}

	for _, field := range second.Scope.GetSymbols() {
		if _, ok := first.Scope.ResolveLocal(field.Name); !ok {
			extraFields[field.Name] = true
		}
		allSecondFields[field.Name] = field.SymbolType
	}

	return missingFields, extraFields, allFirstFields, allSecondFields
}

func buildFieldErrorString(missingFields, extraFields map[string]bool) string {
	var errorString string
	for field := range missingFields {
		errorString += colors.BROWN.Sprintf(" - missing field: %s\n", field)
	}
	for field := range extraFields {
		errorString += colors.BROWN.Sprintf(" - extra field: %s\n", field)
	}
	return errorString
}

func checkFieldTypeCompatibility(firstFields, secondFields map[string]symboltable.AnalyzerNode) (bool, error) {
	for field, secondType := range secondFields {
		firstType := firstFields[field]
		if ok, err := isCompatible(firstType, secondType); !ok {
			return false, fmt.Errorf("struct field `%s` is %s", field, err)
		}
	}
	return true, nil
}
