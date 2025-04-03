package typecheck

import (
	"reflect"

	"ferret/compiler/colors"
	"ferret/compiler/internal/analyzer"
	"fmt"
)

func isCompatible(first, second analyzer.AnalyzerNode) (bool, error) {

	if reflect.TypeOf(first) == reflect.TypeOf(second) {
		switch t := first.(type) {
		case *analyzer.StructType:
			if secondStruct, ok := second.(*analyzer.StructType); ok {
				return checkStructCompatibility(t, secondStruct)
			}
		default:
			return true, nil
		}
	}

	return false, fmt.Errorf("expected `%s` but got `%s`", first.ToString(), second.ToString())
}

func checkStructCompatibility(first, second *analyzer.StructType) (bool, error) {

	//check for missing and extra fields
	missingFields := make(map[string]bool)
	extraFields := make(map[string]bool)

	for _, field := range first.Fields {
		missingFields[field.Name] = true
	}
	for _, field := range second.Fields {
		if _, ok := missingFields[field.Name]; ok {
			delete(missingFields, field.Name)
		} else {
			extraFields[field.Name] = true
		}
	}

	fieldErrors := ""

	for field := range missingFields {
		fieldErrors += colors.BROWN.Sprintf(" - missing field: %s\n", field)
	}
	for field := range extraFields {
		fieldErrors += colors.BROWN.Sprintf(" - extra field: %s\n", field)
	}

	if fieldErrors != "" {
		return false, fmt.Errorf("[ES01] incompatible structs: `%s` and `%s`\n%s", first.ToString(), second.ToString(), fieldErrors)
	}

	fieldErrors = ""

	for i := 0; i < len(first.Fields); i++ {
		fmt.Printf("First field: %s, Second field: %s\n", first.Fields[i], second.Fields[i])
		if first.Fields[i].Name != second.Fields[i].Name {
			return false, fmt.Errorf("[ES02] incompatible structs: `%s` and `%s`: field names do not match", first.ToString(), second.ToString())
		}
		if ok, err := isCompatible(first.Fields[i].Type, second.Fields[i].Type); !ok {
			fieldErrors += colors.BROWN.Sprintf(" - field %s: %s", first.Fields[i].Name, err)
		}
	}

	if fieldErrors != "" {
		return false, fmt.Errorf("[ES03] incompatible structs: `%s` and `%s`\n%s", first.ToString(), second.ToString(), fieldErrors)
	}

	return true, nil
}
