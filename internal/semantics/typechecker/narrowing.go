package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/controlflow"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/tokens"
	"compiler/internal/types"
)

// NarrowingContext tracks type narrowing information for variables within a scope.
// Used for optional type narrowing (T? → T) based on null checks.
type NarrowingContext struct {
	// narrowedTypes maps variable names to their narrowed types
	// Example: after "if a != none", narrowedTypes["a"] = T (unwrapped from T?)
	narrowedTypes map[string]types.SemType

	// parent allows nested scopes (for nested if statements, loops, etc.)
	parent *NarrowingContext
}

// NewNarrowingContext creates a new narrowing context
func NewNarrowingContext(parent *NarrowingContext) *NarrowingContext {
	return &NarrowingContext{
		narrowedTypes: make(map[string]types.SemType),
		parent:        parent,
	}
}

// Narrow records that a variable has been narrowed to a specific type
func (nc *NarrowingContext) Narrow(varName string, narrowedType types.SemType) {
	if nc == nil {
		return
	}
	nc.narrowedTypes[varName] = narrowedType
}

// GetNarrowedType returns the narrowed type for a variable, if any
func (nc *NarrowingContext) GetNarrowedType(varName string) (types.SemType, bool) {
	if nc == nil {
		return nil, false
	}

	// Check current scope
	if typ, ok := nc.narrowedTypes[varName]; ok {
		return typ, true
	}

	// Check parent scopes
	if nc.parent != nil {
		return nc.parent.GetNarrowedType(varName)
	}

	return nil, false
}

// analyzeConditionForNarrowing checks if a condition narrows optional types
// Returns two contexts: one for the "then" branch, one for the "else" branch
func analyzeConditionForNarrowing(_ *context_v2.CompilerContext, mod *context_v2.Module,
	condition ast.Expression, parentNarrowing *NarrowingContext) (*NarrowingContext, *NarrowingContext) {

	thenNarrowing := NewNarrowingContext(parentNarrowing)
	elseNarrowing := NewNarrowingContext(parentNarrowing)

	// Check for patterns like: a != none, a == none, none != a, none == a
	if binExpr, ok := condition.(*ast.BinaryExpr); ok {
		switch binExpr.Op.Kind {
		case tokens.NOT_EQUAL_TOKEN:
			// a != none → a is non-optional in then branch, none in else branch
			varName, isNoneCheck := isNoneComparison(binExpr)
			if isNoneCheck && varName != "" {
				// Check if variable has been narrowed in parent context
				parentNarrowedType, hasParentNarrowing := parentNarrowing.GetNarrowedType(varName)
				
				if hasParentNarrowing {
					// If already narrowed to none, we can't further narrow
					if parentNarrowedType.Equals(types.TypeNone) {
						// Variable is already none, so a != none is always false
						// then branch is unreachable, else branch stays none
						elseNarrowing.Narrow(varName, types.TypeNone) // Explicitly set to ensure it's applied
					} else {
						// Variable was narrowed to T (non-optional), so a != none is always true
						// then branch gets T (already from parent), else branch is unreachable
						thenNarrowing.Narrow(varName, parentNarrowedType) // Explicitly set to ensure it's applied
					}
				} else {
					// Not narrowed in parent, check if it's optional in symbol table
					if optType := getOptionalVarType(mod, varName); optType != nil {
						thenNarrowing.Narrow(varName, optType.Inner) // Unwrap to T
						elseNarrowing.Narrow(varName, types.TypeNone) // Narrow to none in else branch
					}
				}
			}

		case tokens.DOUBLE_EQUAL_TOKEN:
			// a == none → a is none in then branch, non-optional in else branch
			varName, isNoneCheck := isNoneComparison(binExpr)
			if isNoneCheck && varName != "" {
				// Check if variable has been narrowed in parent context
				parentNarrowedType, hasParentNarrowing := parentNarrowing.GetNarrowedType(varName)
				
				if hasParentNarrowing {
					// If already narrowed to none, we can't further narrow
					if parentNarrowedType.Equals(types.TypeNone) {
						// Variable is already none, so a == none is always true
						// then branch gets none (already from parent), else branch is unreachable
						thenNarrowing.Narrow(varName, types.TypeNone) // Explicitly set to ensure it's applied
					} else {
						// Variable was narrowed to T (non-optional), so a == none is always false
						// then branch is unreachable, else branch gets T (already from parent)
						elseNarrowing.Narrow(varName, parentNarrowedType) // Explicitly set to ensure it's applied
					}
				} else {
					// Not narrowed in parent, check if it's optional in symbol table
					if optType := getOptionalVarType(mod, varName); optType != nil {
						thenNarrowing.Narrow(varName, types.TypeNone) // Narrow to none in then branch
						elseNarrowing.Narrow(varName, optType.Inner)  // Unwrap to T in else
					}
				}
			}
		}
	}

	return thenNarrowing, elseNarrowing
}

// isNoneComparison checks if a binary expression is comparing a variable to "none"
// Returns the variable name and true if it's a none comparison
func isNoneComparison(binExpr *ast.BinaryExpr) (string, bool) {
	// Check left side: varName != none
	if ident, ok := binExpr.X.(*ast.IdentifierExpr); ok {
		if isNoneLiteral(binExpr.Y) {
			return ident.Name, true
		}
	}

	// Check right side: none != varName
	if ident, ok := binExpr.Y.(*ast.IdentifierExpr); ok {
		if isNoneLiteral(binExpr.X) {
			return ident.Name, true
		}
	}

	return "", false
}

// isNoneLiteral checks if an expression is the "none" literal
func isNoneLiteral(expr ast.Expression) bool {
	if ident, ok := expr.(*ast.IdentifierExpr); ok {
		return ident.Name == "none"
	}
	return false
}

// getOptionalVarType gets the optional type of a variable if it exists
func getOptionalVarType(mod *context_v2.Module, varName string) *types.OptionalType {
	sym, ok := mod.CurrentScope.Lookup(varName)
	if !ok {
		return nil
	}

	if optType, ok := sym.Type.(*types.OptionalType); ok {
		return optType
	}

	return nil
}

// Helper functions to eliminate code duplication

// addParamsToScope adds function/method parameters to the given scope with their types
func addParamsToScope(ctx *context_v2.CompilerContext, mod *context_v2.Module, scope *table.SymbolTable, params []ast.Field) {
	if params == nil {
		return
	}
	for _, param := range params {
		if param.Name != nil {
			paramType := typeFromTypeNodeWithContext(ctx, mod, param.Type)
			psym, ok := scope.GetSymbol(param.Name.Name)
			if !ok {
				continue // should not happen but safe side
			}
			psym.Type = paramType
		}
	}
}

// setupFunctionContext sets up function scope and return type tracking.
// Returns a cleanup function that should be deferred.
func setupFunctionContext(ctx *context_v2.CompilerContext, mod *context_v2.Module, scope *table.SymbolTable, funcType *ast.FuncType) func() {
	// Enter function scope and get restore function
	restoreScope := mod.EnterScope(scope)

	// Set expected return type for validation
	var expectedReturnType types.SemType = types.TypeVoid
	if funcType != nil && funcType.Result != nil {
		expectedReturnType = typeFromTypeNodeWithContext(ctx, mod, funcType.Result)
	}
	oldReturnType := mod.CurrentFunctionReturnType
	mod.CurrentFunctionReturnType = expectedReturnType

	// Return cleanup function that restores both scope and return type
	return func() {
		restoreScope()
		mod.CurrentFunctionReturnType = oldReturnType
	}
}

// analyzeFunctionReturns builds CFG and analyzes return paths for a function
func analyzeFunctionReturns(ctx *context_v2.CompilerContext, mod *context_v2.Module, funcDecl *ast.FuncDecl) {
	if funcDecl.Body == nil {
		return
	}
	cfgBuilder := controlflow.NewCFGBuilder(ctx, mod)
	cfg := cfgBuilder.BuildFunctionCFG(funcDecl)
	controlflow.AnalyzeReturns(ctx, mod, funcDecl, cfg)
}

// lookupTypeSymbol finds a type symbol by name, checking current module first, then imported modules.
// Returns the symbol and true if found, nil and false otherwise.
func lookupTypeSymbol(ctx *context_v2.CompilerContext, mod *context_v2.Module, typeName string) (*symbols.Symbol, bool) {
	// First check current module's scope
	if sym, found := mod.ModuleScope.Lookup(typeName); found {
		return sym, true
	}

	// Not in current module, search imported modules
	for _, importPath := range mod.ImportAliasMap {
		if importedMod, exists := ctx.GetModule(importPath); exists {
			if sym, ok := importedMod.ModuleScope.GetSymbol(typeName); ok && sym.Kind == symbols.SymbolType {
				return sym, true
			}
		}
	}

	return nil, false
}

// unwrapOptionalType unwraps optional types: T? -> T
// Returns the inner type if it's optional, otherwise returns the original type.
func unwrapOptionalType(typ types.SemType) types.SemType {
	if optType, ok := typ.(*types.OptionalType); ok {
		return optType.Inner
	}
	return typ
}

// dereferenceType unwraps reference types: &T -> T
// Returns the inner type if it's a reference, otherwise returns the original type.
func dereferenceType(typ types.SemType) types.SemType {
	if refType, ok := typ.(*types.ReferenceType); ok {
		return refType.Inner
	}
	return typ
}
