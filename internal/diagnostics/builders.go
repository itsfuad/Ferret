package diagnostics

import (
	"compiler/internal/source"
	"fmt"
)

// Common diagnostic builders for type checker

// UndefinedSymbol creates a diagnostic for undefined symbol
func UndefinedSymbol(filepath string, loc *source.Location, name string) *Diagnostic {
	return NewError("undefined symbol: "+name).
		WithCode(ErrUndefinedSymbol).
		WithPrimaryLabel(filepath, loc, "not found in this scope").
		WithHelp("check if the symbol is declared and imported correctly")
}

// RedeclaredSymbol creates a diagnostic for redeclared symbol
func RedeclaredSymbol(filepath string, newLoc, prevLoc *source.Location, name string) *Diagnostic {
	return NewError(name+" is already declared").
		WithCode(ErrRedeclaredSymbol).
		WithPrimaryLabel(filepath, newLoc, "redeclared here").
		WithSecondaryLabel(filepath, prevLoc, "previously declared here").
		WithHelp("use a different name or remove one of the declarations")
}

// WrongArgumentCount creates a diagnostic for wrong number of arguments
func WrongArgumentCount(filepath string, loc *source.Location, expected, found int) *Diagnostic {
	return NewError("wrong number of arguments").
		WithCode(ErrWrongArgumentCount).
		WithPrimaryLabel(filepath, loc, fmt.Sprintf("expected %d arguments, found %d", expected, found))
}

// WrongArgumentCountVariadic creates a diagnostic for wrong number of arguments to variadic function
func WrongArgumentCountVariadic(filepath string, loc *source.Location, minExpected, found int) *Diagnostic {
	return NewError("wrong number of arguments").
		WithCode(ErrWrongArgumentCount).
		WithPrimaryLabel(filepath, loc, fmt.Sprintf("expected at least %d arguments, found %d", minExpected, found))
}

// ArgumentTypeMismatch creates a diagnostic for argument type mismatch
func ArgumentTypeMismatch(filepath string, loc *source.Location, paramName, expected, found string) *Diagnostic {
	msg := "type mismatch in argument"
	if paramName != "" {
		msg = "type mismatch in argument '" + paramName + "'"
	}
	return NewError(msg).
		WithCode(ErrTypeMismatch).
		WithPrimaryLabel(filepath, loc, "expected "+expected+", found "+found)
}

// NotCallable creates a diagnostic for calling a non-function
func NotCallable(filepath string, loc *source.Location, typeName string) *Diagnostic {
	return NewError("cannot call non-function").
		WithCode(ErrNotCallable).
		WithPrimaryLabel(filepath, loc, "type "+typeName+" is not callable").
		WithHelp("only functions can be called")
}

// FieldNotFound creates a diagnostic for field not found
func FieldNotFound(filepath string, loc *source.Location, fieldName, typeName string) *Diagnostic {
	return NewError("field "+fieldName+" not found").
		WithCode(ErrFieldNotFound).
		WithPrimaryLabel(filepath, loc, typeName+" has no field "+fieldName).
		WithHelp("check the field name spelling")
}
