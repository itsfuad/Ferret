package builtins

import (
	"compiler/internal/types"
)

// NativeFunction represents a native builtin function
type NativeFunction struct {
	Name       string              // Function name (e.g., "Println")
	Signature  *types.FunctionType // Function signature for type checking
	NativeName string              // C function name (e.g., "ferret_io_Println")
}

// IOBuiltins defines all native IO functions
var IOBuiltins = []NativeFunction{
	// Println accepts variadic arguments of any type (empty interface)
	{
		Name: "Println",
		Signature: types.NewFunction(
			[]types.ParamType{{Name: "values", Type: &types.InterfaceType{ID: "", Methods: nil}, IsVariadic: true}}, // ...interface{} - accepts any number of any type
			types.TypeVoid,
		),
		NativeName: "ferret_io_Println", // Codegen will format and concatenate all arguments
	},
	// Print accepts variadic arguments of any type (empty interface)
	{
		Name: "Print",
		Signature: types.NewFunction(
			[]types.ParamType{{Name: "values", Type: &types.InterfaceType{ID: "", Methods: nil}, IsVariadic: true}}, // ...interface{} - accepts any number of any type
			types.TypeVoid,
		),
		NativeName: "ferret_io_Print", // Codegen will format and concatenate all arguments
	},
	{
		Name: "ReadInt",
		Signature: types.NewFunction(
			[]types.ParamType{},
			types.NewResult(types.TypeI32, types.TypeString), // i32 ! str
		),
		NativeName: "ferret_io_ReadInt",
	},
	{
		Name: "ReadFloat",
		Signature: types.NewFunction(
			[]types.ParamType{},
			types.NewResult(types.TypeF64, types.TypeString), // f64 ! str
		),
		NativeName: "ferret_io_ReadFloat",
	},
}

// RegisterIOBuiltins registers the io module with native functions
func RegisterIOBuiltins(creator ModuleCreator) {
	createNativeModule(creator, "std/io", IOBuiltins)
}
