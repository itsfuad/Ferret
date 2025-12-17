package builtins

import (
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
)

// ModuleCreator is an interface to avoid circular dependency with context_v2
type ModuleCreator interface {
	GetUniverse() *table.SymbolTable
	AddModule(importPath string, module any)
}

// RegisterAllBuiltins registers all native builtin modules
func RegisterAllBuiltins(creator ModuleCreator) {
	RegisterIOBuiltins(creator)
}

// createNativeModule creates a synthetic module with native functions
func createNativeModule(creator ModuleCreator, importPath string, funcs []NativeFunction) any {
	modScope := table.NewSymbolTable(creator.GetUniverse())

	// Register each native function
	for _, fn := range funcs {
		sym := &symbols.Symbol{
			Name:       fn.Name,
			Kind:       symbols.SymbolFunction,
			Type:       fn.Signature,
			Exported:   true,
			IsNative:   true,
			NativeName: fn.NativeName,
		}
		modScope.Declare(fn.Name, sym)
	}

	// Use reflection or interface to create module
	// For now, we'll use a simple struct that matches Module interface
	mod := &NativeModule{
		ImportPath:   importPath,
		ModuleScope:  modScope,
		CurrentScope: modScope,
	}

	creator.AddModule(importPath, mod)
	return mod
}

// NativeModule represents a native builtin module
// Exported so context_v2 can convert it to Module
type NativeModule struct {
	ImportPath   string
	ModuleScope  *table.SymbolTable
	CurrentScope *table.SymbolTable
}
