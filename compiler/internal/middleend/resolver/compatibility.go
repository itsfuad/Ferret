package resolver

import (
	"reflect"

	"ferret/compiler/internal/symboltable"
	"ferret/compiler/types"
	"fmt"
)

func isCompatible(first, second symboltable.AnalyzerNode) (bool, error) {

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

	return false, fmt.Errorf("expected to be `%s` but provided `%s`", first.ANode(), second.ANode())
}
