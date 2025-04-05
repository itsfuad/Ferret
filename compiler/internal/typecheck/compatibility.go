package typecheck

import (
	"reflect"

	"ferret/compiler/colors"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/types"
	"fmt"
)

func isCompatible(first, second *symboltable.Symbol) (bool, error) {

	if reflect.TypeOf(first.SymbolType) == reflect.TypeOf(second.SymbolType) {
		switch first.SymbolType.(type) {
		case *symboltable.StructType:
			if _, ok := second.SymbolType.(*symboltable.StructType); ok {
				return checkStructCompatibility(first, second)
			}
		default:
			return true, nil
		}
	}

	return false, fmt.Errorf("expected `%s` but got `%s`", first.SymbolType.ToString(), second.SymbolType.ToString())
}

func checkStructCompatibility(first, second *symboltable.Symbol) (bool, error) {

	//check for missing and extra fields
	missingFields := make(map[string]bool)
	extraFields := make(map[string]bool)

	for _, field := range first.SymbolType.(*symboltable.StructType).Scope.GetSymbols() {
		if _, ok := second.SymbolType.(*symboltable.StructType).Scope.ResolveLocal(field.Name); !ok {
			missingFields[field.Name] = true
		}
	}

	for _, field := range second.SymbolType.(*symboltable.StructType).Scope.GetSymbols() {
		if _, ok := first.SymbolType.(*symboltable.StructType).Scope.ResolveLocal(field.Name); !ok {
			extraFields[field.Name] = true
		}
	}

	errorString := ""

	for field := range missingFields {
		errorString += colors.BROWN.Sprintf(" - missing field: %s\n", field)
	}

	for field := range extraFields {
		errorString += colors.BROWN.Sprintf(" - extra field: %s\n", field)
	}

	if errorString != "" {
		firstStr := ""
		if first.Name == string(types.STRUCT) {
			firstStr = fmt.Sprintf("`%s`", first.SymbolType.ToString())
		} else {
			firstStr = fmt.Sprintf("`%s` expands to `%s`", first.Name, first.SymbolType.ToString())
		}

		secondStr := ""
		if second.Name == string(types.STRUCT) {
			secondStr = fmt.Sprintf("`%s`", second.SymbolType.ToString())
		} else {
			secondStr = fmt.Sprintf("`%s` expands to `%s`", second.Name, second.SymbolType.ToString())
		}

		return false, fmt.Errorf("incompatible structs: %s and %s\n%s", firstStr, secondStr, errorString)
	}

	return true, nil
}
