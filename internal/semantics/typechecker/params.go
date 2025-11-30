package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"fmt"
)

// ValidateFunctionParams validates function parameters including variadic rules
func ValidateFunctionParams(ctx *context_v2.CompilerContext, mod *context_v2.Module, funcDecl *ast.FuncDecl) {
	if funcDecl.Type == nil || funcDecl.Type.Params == nil {
		return
	}

	params := funcDecl.Type.Params
	variadicCount := 0
	variadicIndex := -1

	// Check for variadic parameters and their position
	for i, param := range params {
		if param.IsVariadic {
			variadicCount++
			variadicIndex = i
		}
	}

	// Rule: Maximum one variadic parameter
	if variadicCount > 1 {
		for _, param := range params {
			if param.IsVariadic {
				ctx.Diagnostics.Add(
					diagnostics.NewError("multiple variadic parameters").
						WithPrimaryLabel(mod.FilePath, param.Loc(), "variadic parameter here").
						WithHelp("a function can have at most one variadic parameter"),
				)
			}
		}
		return
	}

	// Rule: Variadic parameter must be last
	if variadicCount == 1 && variadicIndex != len(params)-1 {
		ctx.Diagnostics.Add(
			diagnostics.NewError("variadic parameter must be last").
				WithPrimaryLabel(mod.FilePath, params[variadicIndex].Loc(), "variadic parameter not at end").
				WithHelp(fmt.Sprintf("move this parameter to position %d (last)", len(params))),
		)
	}

	// Check for duplicate parameter names
	paramNames := make(map[string]*ast.Field)
	for _, param := range params {
		if param.Name != nil {
			name := param.Name.Name
			if existing, ok := paramNames[name]; ok {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("duplicate parameter name '%s'", name)).
						WithPrimaryLabel(mod.FilePath, param.Loc(), fmt.Sprintf("'%s' already declared", name)).
						WithSecondaryLabel(mod.FilePath, existing.Loc(), "previous declaration here"),
				)
			} else {
				paramNames[name] = &param
			}
		}
	}

	// Check for missing parameter types
	for _, param := range params {
		if param.Type == nil {
			paramName := "<unnamed>"
			if param.Name != nil {
				paramName = param.Name.Name
			}
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("parameter '%s' missing type annotation", paramName)).
					WithPrimaryLabel(mod.FilePath, param.Loc(), "type annotation required").
					WithHelp("add type annotation (e.g., 'name: i32')"),
			)
		}
	}
}

// CreateParameterScope creates a new scope with function parameters
func CreateParameterScope(
	ctx *context_v2.CompilerContext,
	mod *context_v2.Module,
	parentScope *context_v2.SymbolTable,
	params []ast.Field,
) *context_v2.SymbolTable {
	funcScope := context_v2.NewSymbolTable(parentScope)

	for _, param := range params {
		if param.Name != nil {
			// Parameter type will be resolved during type checking
			sym := &context_v2.Symbol{
				Name:     param.Name.Name,
				Kind:     context_v2.SymbolParameter,
				Type:     nil, // Will be filled by type checker
				Exported: false,
				Decl:     param.Name,
			}

			if err := funcScope.Declare(param.Name.Name, sym); err != nil {
				// Duplicate already reported by ValidateFunctionParams
				continue
			}
		}
	}

	return funcScope
}
