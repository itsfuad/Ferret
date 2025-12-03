package diagnostics

import (
	"compiler/internal/source"
	"compiler/internal/utils/strings"
	"fmt"
)

// Common diagnostic builders for type checker

// UndefinedSymbol creates a diagnostic for undefined symbol
func UndefinedSymbol(filepath string, loc *source.Location, name string) *Diagnostic {
	return NewError("undefined symbol: "+name).
		WithCode(ErrUndefinedSymbol).
		WithPrimaryLabel(loc, "not found in this scope").
		WithHelp("check if the symbol is declared and imported correctly")
}

// RedeclaredSymbol creates a diagnostic for redeclared symbol
func RedeclaredSymbol(filepath string, newLoc, prevLoc *source.Location, name string) *Diagnostic {
	return NewError(name+" is already declared").
		WithCode(ErrRedeclaredSymbol).
		WithPrimaryLabel(newLoc, "redeclared here").
		WithSecondaryLabel(prevLoc, "previously declared here").
		WithHelp("use a different name or remove one of the declarations")
}

// WrongArgumentCount creates a diagnostic for wrong number of arguments
func WrongArgumentCount(filepath string, loc *source.Location, expected, found int) *Diagnostic {
	return NewError("wrong number of arguments").
		WithCode(ErrWrongArgumentCount).
		WithPrimaryLabel(loc, fmt.Sprintf("expected %d %s, found %d",
			expected, strings.Pluralize("argument", "arguments", expected), found))
}

// WrongArgumentCountVariadic creates a diagnostic for wrong number of arguments to variadic function
func WrongArgumentCountVariadic(filepath string, loc *source.Location, minExpected, found int) *Diagnostic {
	return NewError("wrong number of arguments").
		WithCode(ErrWrongArgumentCount).
		WithPrimaryLabel(loc, fmt.Sprintf("expected at least %d %s, found %d",
			minExpected, strings.Pluralize("argument", "arguments", minExpected), found))
}

// ArgumentTypeMismatch creates a diagnostic for argument type mismatch
func ArgumentTypeMismatch(filepath string, loc *source.Location, param, expected, found string) *Diagnostic {
	
	msg := fmt.Sprintf("type mismatch in parameter '%s'", param)

	return NewError(msg).
		WithCode(ErrTypeMismatch).
		WithPrimaryLabel(loc, fmt.Sprintf("expected %s, found %s", expected, found))
}

// NotCallable creates a diagnostic for calling a non-function
func NotCallable(filepath string, loc *source.Location, typeName string) *Diagnostic {
	return NewError("cannot call non-function").
		WithCode(ErrNotCallable).
		WithPrimaryLabel(loc, fmt.Sprintf("type %s is not callable", typeName)).
		WithHelp("only functions can be called")
}

// FieldNotFound creates a diagnostic for field not found
func FieldNotFound(filepath string, loc *source.Location, fieldName, typeName string) *Diagnostic {
	return NewError("field "+fieldName+" not found").
		WithCode(ErrFieldNotFound).
		WithPrimaryLabel(loc, fmt.Sprintf("%s has no field %s", typeName, fieldName)).
		WithHelp("check the field name spelling")
}
