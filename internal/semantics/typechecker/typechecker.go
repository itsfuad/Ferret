package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/controlflow"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/types"
	"fmt"
	"sync"
)

// CheckModule performs type checking on a module.
// It fills Symbol.Type for declarations and creates ExprTypes map for expressions.
func CheckModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {

	// Reset CurrentScope to ModuleScope before type checking
	// This ensures we're starting from the module-level scope
	mod.CurrentScope = mod.ModuleScope

	// Check all declarations in the module
	if mod.AST != nil {
		for _, node := range mod.AST.Nodes {
			checkNode(ctx, mod, node)
		}
	}
}

// checkNode type checks a single AST node
func checkNode(ctx *context_v2.CompilerContext, mod *context_v2.Module, node ast.Node) {
	if node == nil {
		return
	}

	switch n := node.(type) {
	case *ast.VarDecl:
		checkVarDecl(ctx, mod, n, false)
	case *ast.ConstDecl:
		checkVarDecl(ctx, mod, n, true)
	case *ast.FuncDecl:
		checkFuncDecl(ctx, mod, n)
	case *ast.AssignStmt:
		checkAssignStmt(ctx, mod, n)
	case *ast.ReturnStmt:
		// Check return value type against function's declared return type
		if n.Result != nil {
			expectedReturnType := mod.CurrentFunctionReturnType
			if expectedReturnType == nil {
				expectedReturnType = types.TypeUnknown
			}
			returnedType := checkExpr(ctx, mod, n.Result, expectedReturnType)

			// Validate return type matches function signature
			if !expectedReturnType.Equals(types.TypeUnknown) && !returnedType.Equals(types.TypeUnknown) {
				compatibility := checkTypeCompatibility(returnedType, expectedReturnType)
				if compatibility == Incompatible {
					ctx.Diagnostics.Add(
						diagnostics.NewError("type mismatch in return statement").
							WithCode("T0015").
							WithPrimaryLabel(n.Result.Loc(),
								fmt.Sprintf("expected %s, found %s", expectedReturnType.String(), returnedType.String())),
					)
				}
			}
		}
	case *ast.IfStmt:
		// Enter if scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		checkExpr(ctx, mod, n.Cond, types.TypeBool)
		checkBlock(ctx, mod, n.Body)
		if n.Else != nil {
			checkNode(ctx, mod, n.Else)
		}
	case *ast.ForStmt:
		// Enter for loop scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		if n.Init != nil {
			checkNode(ctx, mod, n.Init)
		}
		if n.Cond != nil {
			checkExpr(ctx, mod, n.Cond, types.TypeBool)
		}
		if n.Post != nil {
			checkNode(ctx, mod, n.Post)
		}
		checkBlock(ctx, mod, n.Body)
	case *ast.WhileStmt:
		// Enter while loop scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		checkExpr(ctx, mod, n.Cond, types.TypeBool)
		checkBlock(ctx, mod, n.Body)
	case *ast.Block:
		checkBlock(ctx, mod, n)
	case *ast.DeclStmt:
		checkNode(ctx, mod, n.Decl)
	case *ast.ExprStmt:
		checkExpr(ctx, mod, n.X, types.TypeUnknown)
	}
}

// checkVarDecl type checks variable/constant declarations
func checkVarDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl interface{}, isConst bool) {
	var declItems []ast.DeclItem

	switch d := decl.(type) {
	case *ast.VarDecl:
		declItems = d.Decls
	case *ast.ConstDecl:
		declItems = d.Decls
	default:
		return
	}

	for _, item := range declItems {
		name := item.Name.Name

		// Get or create the symbol (module-level or local)
		sym, ok := mod.CurrentScope.GetSymbol(name)
		if !ok {
			continue
		}

		// Determine the type
		if item.Type != nil {
			// Explicit type annotation
			declType := typeFromTypeNode(item.Type)
			sym.Type = declType

			// Check initializer if present
			if item.Value != nil {
				// Pass item.Type for type location in error messages
				checkAssignLike(ctx, mod, declType, item.Type, item.Value)
			} else if isConst {
				// Constants must have an initializer even with explicit type
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("constant '%s' must be initialized", name)).
						WithPrimaryLabel(item.Name.Loc(), "constants require an initializer").
						WithHelp("provide a value: const x: i32 = 42"),
				)
			}
		} else if item.Value != nil {
			// Type inference from initializer
			rhsType := checkExpr(ctx, mod, item.Value, types.TypeUnknown)

			// If the RHS is UNTYPED, finalize it to a default type
			if types.IsUntyped(rhsType) {
				rhsType = finalizeUntyped(item.Value)
			}

			sym.Type = rhsType
		} else {
			// No type and no value - error
			if isConst {
				// Constants must have an initializer
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("constant '%s' must be initialized", name)).
						WithPrimaryLabel(item.Name.Loc(), "constants require an initializer").
						WithHelp("provide a value: const x := 42 or const x: i32 = 42"),
				)
			} else {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("cannot infer type for '%s'", name)).
						WithPrimaryLabel(item.Name.Loc(), "missing type or initializer"),
				)
			}
			sym.Type = types.TypeUnknown
		}
	}
}

// checkFuncDecl type checks a function declaration
func checkFuncDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.FuncDecl) {
	// Safety check: Scope should be set during collection phase
	if decl.Scope == nil {
		ctx.ReportError(fmt.Sprintf("internal error: function '%s' has nil scope", decl.Name.Name), decl.Loc())
		return
	}

	funcScope := decl.Scope.(*table.SymbolTable)

	// Update the function symbol's type with actual function signature
	if decl.Name != nil && decl.Type != nil {
		funcType := typeFromTypeNode(decl.Type).(*types.FunctionType)
		if sym, ok := mod.CurrentScope.Lookup(decl.Name.Name); ok {
			sym.Type = funcType
		}
	}

	// Enter function scope for the entire function processing
	defer mod.EnterScope(funcScope)()

	// Set expected return type for validation
	var expectedReturnType types.SemType = types.TypeVoid
	if decl.Type != nil && decl.Type.Result != nil {
		expectedReturnType = typeFromTypeNode(decl.Type.Result)
	}
	oldReturnType := mod.CurrentFunctionReturnType
	mod.CurrentFunctionReturnType = expectedReturnType
	defer func() {
		mod.CurrentFunctionReturnType = oldReturnType
	}()

	// Add parameters to the function scope with type information
	if decl.Type != nil && decl.Type.Params != nil {
		for _, param := range decl.Type.Params {
			if param.Name != nil {
				paramType := typeFromTypeNode(param.Type)
				// update the param type
				psym, ok := funcScope.GetSymbol(param.Name.Name)
				if !ok {
					continue // should not happen but safe side
				}

				psym.Type = paramType // Done
			}
		}
	}

	// Check the body with the function scope
	if decl.Body != nil {
		checkBlock(ctx, mod, decl.Body)

		// Build control flow graph for the function
		cfgBuilder := controlflow.NewCFGBuilder(ctx, mod)
		cfg := cfgBuilder.BuildFunctionCFG(decl)

		// Analyze return paths
		controlflow.AnalyzeReturns(ctx, mod, decl, cfg)
	}
}

// checkAssignStmt type checks an assignment statement
func checkAssignStmt(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.AssignStmt) {
	// Check if we're trying to reassign a constant
	if ident, ok := stmt.Lhs.(*ast.IdentifierExpr); ok {
		if sym, found := mod.CurrentScope.Lookup(ident.Name); found {
			if sym.Kind == symbols.SymbolConstant {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("cannot assign to constant '%s'", ident.Name)).
						WithPrimaryLabel(stmt.Lhs.Loc(), "cannot modify constant").
						WithSecondaryLabel(sym.Decl.Loc(), "declared as constant here").
						WithHelp("constants are immutable; use 'let' for mutable variables"),
				)
				return
			}
		}
	}

	// Get the type of the LHS
	lhsType := checkExpr(ctx, mod, stmt.Lhs, types.TypeUnknown)

	// Check the RHS with the LHS type as context
	// Pass stmt.Lhs for LHS location in error messages
	checkAssignLike(ctx, mod, lhsType, stmt.Lhs, stmt.Rhs)
}

// checkBlock type checks a block of statements
func checkBlock(ctx *context_v2.CompilerContext, mod *context_v2.Module, block *ast.Block) {
	if block == nil {
		return
	}

	// Enter block scope if it exists
	if block.Scope != nil {
		oldScope := mod.CurrentScope
		mod.CurrentScope = block.Scope.(*table.SymbolTable)
		defer func() {
			mod.CurrentScope = oldScope
		}()
	}

	for _, node := range block.Nodes {
		checkNode(ctx, mod, node)
	}
}

// checkExpr type checks an expression and returns its type.
// expected provides contextual type information (TypeUnknown if no context).
// This function now uses the separate inference and compatibility modules.
func checkExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr ast.Expression, expected types.SemType) types.SemType {
	if expr == nil {
		return types.TypeUnknown
	}

	// Special handling for call expressions - validate before inference
	if callExpr, ok := expr.(*ast.CallExpr); ok {
		checkCallExpr(ctx, mod, callExpr)
		// Continue with normal type inference for return type
	}

	// First, infer the type based on the expression structure
	inferredType := inferExprType(ctx, mod, expr)

	// Apply contextual typing
	resultType := inferredType

	// 1. Handle UNTYPED numeric literals
	if types.IsUntyped(inferredType) && !expected.Equals(types.TypeUnknown) {
		if lit, ok := expr.(*ast.BasicLit); ok {
			// If expected is optional, contextualize to inner type
			expectedForLit := expected
			if optType, ok := expected.(*types.OptionalType); ok {
				expectedForLit = optType.Inner
			}
			resultType = contextualizeUntyped(lit, expectedForLit)
		}
	}

	// 2. Handle composite literals (arrays, structs) without explicit type
	if inferredType.Equals(types.TypeUnknown) && !expected.Equals(types.TypeUnknown) {
		if lit, ok := expr.(*ast.CompositeLit); ok && lit.Type == nil {
			// Empty composite literal adopts expected type
			resultType = expected
		}
	}

	// 3. Handle optional type wrapping (T -> T?)
	if !expected.Equals(types.TypeUnknown) {
		if optType, ok := expected.(*types.OptionalType); ok {
			// If assigning non-optional to optional, check if inner types match
			if !resultType.Equals(types.TypeUnknown) && resultType.Equals(optType.Inner) {
				// Value matches inner type, allow implicit wrapping
				resultType = expected
			}
		}
	}

	// If still UNTYPED and we need a concrete type, finalize it
	if types.IsUntyped(resultType) && expected.Equals(types.TypeUnknown) {
		// Keep as UNTYPED for now - will be finalized when needed
	}

	return resultType
}

// checkAssignLike checks assignment-like operations with better error reporting
// typeNode: optional AST node for the target type (for location info)
// valueExpr: the value expression being assigned
// targetType: the expected/target type
func checkAssignLike(ctx *context_v2.CompilerContext, mod *context_v2.Module, targetType types.SemType, typeNode ast.Node, valueExpr ast.Expression) {
	rhsType := checkExpr(ctx, mod, valueExpr, targetType)

	// Special check for integer literals: ensure they fit in the target type
	if ok := checkFitness(ctx, rhsType, targetType, valueExpr, typeNode); !ok {
		return
	}

	// Check type compatibility
	compatibility := checkTypeCompatibility(rhsType, targetType)

	switch compatibility {
	case Identical, Assignable, LosslessConvertible:
		// OK - assignment is valid
		return

	case LossyConvertible:
		// Requires explicit cast
		diag := diagnostics.NewError(getConversionError(rhsType, targetType, compatibility))

		// Add dual labels if we have type node location
		if typeNode != nil {
			diag = diag.WithPrimaryLabel(valueExpr.Loc(), fmt.Sprintf("type '%s'", rhsType.String())).
				WithSecondaryLabel(typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
		} else {
			diag = diag.WithPrimaryLabel(valueExpr.Loc(), "implicit conversion may lose precision")
		}

		ctx.Diagnostics.Add(
			diag.WithHelp(fmt.Sprintf("use an explicit cast: as %s", targetType.String())),
		)

	case Incompatible:
		// Cannot convert - create user-friendly error message
		var errorMsg string
		if types.IsUntyped(rhsType) {
			// For untyped literals, use more intuitive message
			if types.IsUntypedInt(rhsType) {
				errorMsg = fmt.Sprintf("cannot use integer literal as type '%s'", targetType.String())
			} else if types.IsUntypedFloat(rhsType) {
				errorMsg = fmt.Sprintf("cannot use float literal as type '%s'", targetType.String())
			} else {
				errorMsg = getConversionError(rhsType, targetType, compatibility)
			}
		} else {
			errorMsg = getConversionError(rhsType, targetType, compatibility)
		}

		diag := diagnostics.NewError(errorMsg)

		// Add dual labels if we have type node location
		if typeNode != nil {
			// Format value description (special handling for untyped literals)
			valueDesc := formatValueDescription(rhsType)
			diag = diag.WithPrimaryLabel(valueExpr.Loc(), valueDesc).
				WithSecondaryLabel(typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
		} else {
			diag = diag.WithPrimaryLabel(valueExpr.Loc(), fmt.Sprintf("expected '%s', got '%s'", targetType.String(), rhsType.String()))
		}

		ctx.Diagnostics.Add(diag)
	}
}

// formatValueDescription formats a user-friendly description for a value in error messages.
// For untyped literals, it returns "integer literal" or "float literal" instead of "type 'untyped'".
func formatValueDescription(typ types.SemType) string {
	// Handle untyped literals specially
	if types.IsUntypedInt(typ) {
		return "integer literal"
	}
	if types.IsUntypedFloat(typ) {
		return "float literal"
	}
	return fmt.Sprintf("type '%s'", typ.String())
}

// formatTypeDescription formats a user-friendly type description (without "type" prefix).
// For untyped literals, it returns "integer" or "float" instead of "untyped".
func formatTypeDescription(typ types.SemType) string {
	// Handle untyped literals specially
	if types.IsUntypedInt(typ) {
		return "integer"
	}
	if types.IsUntypedFloat(typ) {
		return "float"
	}
	return typ.String()
}

func checkFitness(ctx *context_v2.CompilerContext, rhsType, targetType types.SemType, valueExpr ast.Expression, typeNode ast.Node) bool {
	if types.IsUntypedInt(rhsType) || rhsType.Equals(types.TypeI64) {
		if lit, ok := valueExpr.(*ast.BasicLit); ok && lit.Kind == ast.INT {
			if targetName, ok := types.GetPrimitiveName(targetType); ok && types.IsIntegerTypeName(targetName) {
				// Use big.Int for all range checking (simpler, consistent)
				if !fitsInType(lit.Value, targetType) {
					// Get minimum type that can hold this value
					minType := getMinimumTypeForValue(lit.Value)

					// Build error message
					diag := diagnostics.NewError(fmt.Sprintf("integer literal %s overflows %s", lit.Value, targetType.String()))

					if typeNode != nil {
						// Show value location (primary) and type location (secondary)
						if minType != "unknown" && minType != "too large for any supported type" {
							diag = diag.WithPrimaryLabel(valueExpr.Loc(), fmt.Sprintf("at least %s required to store this value", minType)).
								WithSecondaryLabel(typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
						} else if minType == "too large for any supported type" {
							diag = diag.WithPrimaryLabel(valueExpr.Loc(), "exceeds maximum supported integer size (256-bit)").
								WithSecondaryLabel(typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
						} else {
							diag = diag.WithPrimaryLabel(valueExpr.Loc(), "value too large for this type").
								WithSecondaryLabel(typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
						}
					} else {
						if minType != "unknown" && minType != "too large for any supported type" {
							diag = diag.WithPrimaryLabel(valueExpr.Loc(), fmt.Sprintf("at least %s required, got %s", minType, targetType.String()))
						} else {
							diag = diag.WithPrimaryLabel(valueExpr.Loc(), fmt.Sprintf("value doesn't fit in %s", targetType.String()))
						}
					}

					diag = diag.WithNote(fmt.Sprintf("%s can hold values in range: %s", targetType.String(), getTypeRange(targetType)))

					ctx.Diagnostics.Add(diag)
					return false
				}
			}
		}
	}
	return true
}

// typeFromTypeNode converts AST type nodes to semantic types
func typeFromTypeNode(typeNode ast.TypeNode) types.SemType {
	if typeNode == nil {
		return types.TypeUnknown
	}

	switch t := typeNode.(type) {
	case *ast.IdentifierExpr:
		// Simple type name (primitive or builtin)
		return types.FromTypeName(types.TYPE_NAME(t.Name))

	case *ast.UserDefinedType:
		// User-defined type (struct, enum, interface)
		// For now, treat as primitive - will be expanded with type registry
		return types.FromTypeName(types.TYPE_NAME(t.Name))

	case *ast.ArrayType:
		// Array type: [N]T or []T
		elementType := typeFromTypeNode(t.ElType)
		length := -1 // Dynamic array by default
		if t.Len != nil {
			// TODO: keep all array dynamic now
		}
		return types.NewArray(elementType, length)

	case *ast.OptionalType:
		// Optional type: T?
		innerType := typeFromTypeNode(t.Base)
		return types.NewOptional(innerType)

	case *ast.ResultType:
		// Result type: T ! E
		okType := typeFromTypeNode(t.Value)
		errType := typeFromTypeNode(t.Error)
		return types.NewResult(okType, errType)

	case *ast.StructType:
		// Struct type: struct { .x: i32, .y: i32 }
		fields := make([]types.StructField, len(t.Fields))

		// Process struct fields in parallel with bounded concurrency
		var wg sync.WaitGroup

		for i, f := range t.Fields {
			wg.Add(1)
			go func(idx int, field ast.Field) {
				defer wg.Done()

				fieldName := ""
				if field.Name != nil {
					fieldName = field.Name.Name
				}
				fields[idx] = types.StructField{
					Name: fieldName,
					Type: typeFromTypeNode(field.Type),
				}
			}(i, f)
		}

		wg.Wait()

		return types.NewStruct("", fields) // Anonymous struct

	case *ast.FuncType:
		// Function type: fn(T1, T2) -> R
		params := make([]types.ParamType, len(t.Params))

		// Process parameters in parallel with bounded concurrency
		var wg sync.WaitGroup

		for i, param := range t.Params {
			wg.Add(1)
			go func(idx int, par ast.Field) {
				defer wg.Done()
				param := &params[idx]
				param.Name = par.Name.Name
				param.Type = typeFromTypeNode(par.Type)
				param.IsVariadic = par.IsVariadic // Preserve variadic info
			}(i, param)
		}

		wg.Wait()

		returns := typeFromTypeNode(t.Result)

		return types.NewFunction(params, returns)

	default:
		return types.TypeUnknown
	}
}

// checkCallExpr validates function call expressions
// This includes:
// - Verifying the called expression is actually a function
// - Checking argument count (regular and variadic functions)
// - Validating argument types against parameter types
func checkCallExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CallExpr) {
	// 1. Infer the type of the expression being called
	funType := inferExprType(ctx, mod, expr.Fun)

	// 2. Check if it's unknown (error already reported in resolution phase)
	if funType.Equals(types.TypeUnknown) {
		// Still type check arguments to find additional errors
		for _, arg := range expr.Args {
			checkExpr(ctx, mod, arg, types.TypeUnknown)
		}
		return
	}

	// 3. Check if it's a function type
	funcType, ok := funType.(*types.FunctionType)
	if !ok {
		ctx.Diagnostics.Add(
			diagnostics.NotCallable(mod.FilePath, expr.Fun.Loc(), funType.String()),
		)
		// Still type check arguments
		for _, arg := range expr.Args {
			checkExpr(ctx, mod, arg, types.TypeUnknown)
		}
		return
	}

	// 4. Validate argument count
	validateCallArgumentCount(ctx, mod, expr, funcType)

	// 5. Validate argument types
	validateCallArgumentTypes(ctx, mod, expr, funcType)
}

// validateCallArgumentCount checks if the number of arguments matches the function signature
func validateCallArgumentCount(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CallExpr, funcType *types.FunctionType) {
	argCount := len(expr.Args)
	paramCount := len(funcType.Params)

	// Check if function is variadic
	isVariadic := paramCount > 0 && funcType.Params[paramCount-1].IsVariadic

	if isVariadic {
		// Variadic function: need at least (paramCount - 1) arguments
		minRequired := paramCount - 1
		if argCount < minRequired {
			ctx.Diagnostics.Add(
				diagnostics.WrongArgumentCountVariadic(
					mod.FilePath,
					&expr.Location,
					minRequired,
					argCount,
				),
			)
		}
	} else {
		// Regular function: need exactly paramCount arguments
		if argCount != paramCount {
			ctx.Diagnostics.Add(
				diagnostics.WrongArgumentCount(
					mod.FilePath,
					&expr.Location,
					paramCount,
					argCount,
				),
			)
		}
	}
}

// validateCallArgumentTypes checks if argument types are compatible with parameter types
func validateCallArgumentTypes(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CallExpr, funcType *types.FunctionType) {
	paramCount := len(funcType.Params)
	argCount := len(expr.Args)

	// Determine if function is variadic
	isVariadic := paramCount > 0 && funcType.Params[paramCount-1].IsVariadic

	// Check regular parameters
	regularParamCount := paramCount
	if isVariadic {
		regularParamCount = paramCount - 1
	}

	// Validate regular parameters
	for i := 0; i < regularParamCount && i < argCount; i++ {
		param := funcType.Params[i]
		arg := expr.Args[i]

		// Infer argument type with parameter type as context
		argType := checkExpr(ctx, mod, arg, param.Type)

		// Check compatibility
		compatibility := checkTypeCompatibility(argType, param.Type)

		if compatibility == Incompatible {
			// Format the argument type in a user-friendly way
			argTypeDesc := formatTypeDescription(argType)
			ctx.Diagnostics.Add(
				diagnostics.ArgumentTypeMismatch(
					mod.FilePath,
					arg.Loc(),
					param.Name,
					param.Type.String(),
					argTypeDesc,
				),
			)
		}
	}

	// Validate variadic arguments
	if isVariadic && argCount > regularParamCount {
		variadicParam := funcType.Params[paramCount-1]
		variadicElemType := variadicParam.Type // The element type (not array type)

		for i := regularParamCount; i < argCount; i++ {
			arg := expr.Args[i]

			// Infer argument type with variadic element type as context
			argType := checkExpr(ctx, mod, arg, variadicElemType)

			// Check compatibility with variadic element type
			compatibility := checkTypeCompatibility(argType, variadicElemType)

			if compatibility == Incompatible {
				// Format the argument type in a user-friendly way
				argTypeDesc := formatTypeDescription(argType)
				ctx.Diagnostics.Add(
					diagnostics.ArgumentTypeMismatch(
						mod.FilePath,
						arg.Loc(),
						variadicParam.Name,
						variadicElemType.String(),
						argTypeDesc,
					),
				)
			}
		}
	}

	// Type check any remaining arguments (in case of wrong count)
	// This helps find multiple errors in a single pass
	maxChecked := argCount
	if isVariadic {
		maxChecked = argCount // Already checked all
	} else {
		maxChecked = regularParamCount
	}

	for i := maxChecked; i < argCount; i++ {
		checkExpr(ctx, mod, expr.Args[i], types.TypeUnknown)
	}
}
