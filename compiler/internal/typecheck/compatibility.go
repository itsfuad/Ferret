package typecheck

import (
	"reflect"

	"ferret/compiler/colors"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/types"
	"fmt"
)

func isCompatible(first, second symboltable.AnalyzerNode) (bool, error) {

	fmt.Printf("first: %s, second: %s\n", reflect.TypeOf(first), reflect.TypeOf(second))

	unwrappedFirst := symboltable.UnwrapUserDefType(&first)
	unwrappedSecond := symboltable.UnwrapUserDefType(&second)
	if reflect.TypeOf(unwrappedFirst) == reflect.TypeOf(unwrappedSecond) {
		switch unwrappedFirst.ANode() {
		case types.STRUCT:
			if unwrappedSecond.ANode() == types.STRUCT {
				return checkStructCompatibility(&first, &second)
			}
		default:
			return true, nil
		}
	}

	return false, fmt.Errorf("expected `%s` but got `%s`", first.ANode(), second.ANode())
}

func checkStructCompatibility(first, second *symboltable.AnalyzerNode) (bool, error) {

	unwrappedFirst := symboltable.UnwrapUserDefType(first)
	unwrappedSecond := symboltable.UnwrapUserDefType(second)

	//check for missing and extra fields
	allFirstFields := make(map[string]bool)
	allSecondFields := make(map[string]bool)
	missingFields := make(map[string]bool)
	extraFields := make(map[string]bool)

	for _, field := range unwrappedFirst.(*symboltable.StructType).Scope.GetSymbols() {
		if _, ok := unwrappedSecond.(*symboltable.StructType).Scope.ResolveLocal(field.Name); !ok {
			missingFields[field.Name] = true
		}
		allFirstFields[field.Name] = true
	}

	for _, field := range unwrappedSecond.(*symboltable.StructType).Scope.GetSymbols() {
		if _, ok := unwrappedFirst.(*symboltable.StructType).Scope.ResolveLocal(field.Name); !ok {
			extraFields[field.Name] = true
		}
		allSecondFields[field.Name] = true
	}

	errorString := ""

	for field := range missingFields {
		errorString += colors.BROWN.Sprintf(" - missing field: %s\n", field)
	}

	for field := range extraFields {
		errorString += colors.BROWN.Sprintf(" - extra field: %s\n", field)
	}

	fmt.Printf("First: %#v, Second: %#v\n", (*first).ToString(), (*second).ToString())

	if errorString != "" {
		firstStr := ""
		if (*first).ANode() == types.STRUCT {
			firstStr = fmt.Sprintf("`%s`", (*first).ToString())
		} else {
			firstStr = fmt.Sprintf("`%s` expands to `%s`", (*first).ToString(), unwrappedFirst.ToString())
		}

		secondStr := ""
		if (*second).ANode() == types.STRUCT {
			secondStr = fmt.Sprintf("`%s`", (*second).ToString())
		} else {
			secondStr = fmt.Sprintf("`%s` expands to `%s`", (*second).ToString(), unwrappedSecond.ToString())
		}

		return false, fmt.Errorf("incompatible structs: %s and %s\n%s", firstStr, secondStr, errorString)
	}

	//check for type compatibility on fields
	// for name, _ := range allFirstFields {
	// 	firstType := first.Scope.GetSymbols()[name].SymbolType
	// 	secondType := second.Scope.GetSymbols()[name].SymbolType

	// 	if ok, err := isCompatible(firstType, secondType); !ok {
	// 		return false, fmt.Errorf("incompatible types for field: %s\n%s", name, err)
	// 	}
	// }

	return true, nil
}
