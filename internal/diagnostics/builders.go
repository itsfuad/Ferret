package diagnostics

import (
	"compiler/internal/source"
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
		WithPrimaryLabel(filepath, loc, "expected "+string(rune(expected))+" arguments, found "+string(rune(found)))
}

// FieldNotFound creates a diagnostic for field not found
func FieldNotFound(filepath string, loc *source.Location, fieldName, typeName string) *Diagnostic {
	return NewError("field "+fieldName+" not found").
		WithCode(ErrFieldNotFound).
		WithPrimaryLabel(filepath, loc, typeName+" has no field "+fieldName).
		WithHelp("check the field name spelling")
}