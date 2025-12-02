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
	"strconv"
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
		if n.Result != nil {
			checkExpr(ctx, mod, n.Result, types.TypeUnknown)
		}
	case *ast.IfStmt:
		// Enter if scope if it exists
		if n.Scope != nil {
			oldScope := mod.CurrentScope
			mod.CurrentScope = n.Scope.(*table.SymbolTable)
			defer func() {
				mod.CurrentScope = oldScope
			}()
		}

		checkExpr(ctx, mod, n.Cond, types.TypeBool)
		checkBlock(ctx, mod, n.Body)
		if n.Else != nil {
			checkNode(ctx, mod, n.Else)
		}
	case *ast.ForStmt:
		// Enter for loop scope if it exists
		if n.Scope != nil {
			oldScope := mod.CurrentScope
			mod.CurrentScope = n.Scope.(*table.SymbolTable)
			defer func() {
				mod.CurrentScope = oldScope
			}()
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
			oldScope := mod.CurrentScope
			mod.CurrentScope = n.Scope.(*table.SymbolTable)
			defer func() {
				mod.CurrentScope = oldScope
			}()
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
				// Pass item.Name for secondary label (variable location)
				checkAssignLike(ctx, mod, declType, item.Name, item.Value)
			} else if isConst {
				// Constants must have an initializer even with explicit type
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("constant '%s' must be initialized", name)).
						WithPrimaryLabel(mod.FilePath, item.Name.Loc(), "constants require an initializer").
						WithHelp("provide a value: const x: i32 = 42"),
				)
			}
		} else if item.Value != nil {
			// Type inference from initializer
			rhsType := checkExpr(ctx, mod, item.Value, types.TypeUnknown)

			// If the RHS is UNTYPED, finalize it to a default type
			if rhsType.Equals(types.TypeUntyped) {
				rhsType = finalizeUntyped(item.Value)
			}

			sym.Type = rhsType
		} else {
			// No type and no value - error
			if isConst {
				// Constants must have an initializer
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("constant '%s' must be initialized", name)).
						WithPrimaryLabel(mod.FilePath, item.Name.Loc(), "constants require an initializer").
						WithHelp("provide a value: const x := 42 or const x: i32 = 42"),
				)
			} else {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("cannot infer type for '%s'", name)).
						WithPrimaryLabel(mod.FilePath, item.Name.Loc(), "missing type or initializer"),
				)
			}
			sym.Type = types.TypeUnknown
		}
	}
}

// checkFuncDecl type checks a function declaration
func checkFuncDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.FuncDecl) {

	funcScope := decl.Scope.(*table.SymbolTable)

	// Save the current scope to restore later
	oldScope := mod.CurrentScope

	mod.CurrentScope = funcScope

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
		oldScope := mod.CurrentScope
		mod.CurrentScope = funcScope
		checkBlock(ctx, mod, decl.Body)
		mod.CurrentScope = oldScope

		// Build control flow graph for the function
		cfgBuilder := controlflow.NewCFGBuilder(ctx, mod)
		cfg := cfgBuilder.BuildFunctionCFG(decl)

		// Analyze return paths
		controlflow.AnalyzeReturns(ctx, mod, decl, cfg)
	}

	mod.CurrentScope = oldScope // restore
}

// checkAssignStmt type checks an assignment statement
func checkAssignStmt(ctx *context_v2.CompilerContext, mod *context_v2.Module, stmt *ast.AssignStmt) {
	// Check if we're trying to reassign a constant
	if ident, ok := stmt.Lhs.(*ast.IdentifierExpr); ok {
		if sym, found := mod.CurrentScope.Lookup(ident.Name); found {
			if sym.Kind == symbols.SymbolConstant {
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("cannot assign to constant '%s'", ident.Name)).
						WithPrimaryLabel(mod.FilePath, stmt.Lhs.Loc(), "cannot modify constant").
						WithSecondaryLabel(mod.FilePath, sym.Decl.Loc(), "declared as constant here").
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

	// First, infer the type based on the expression structure
	inferredType := inferExprType(ctx, mod, expr)

	// Apply contextual typing
	resultType := inferredType

	// 1. Handle UNTYPED numeric/string literals
	if inferredType.Equals(types.TypeUntyped) && !expected.Equals(types.TypeUnknown) {
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
	if resultType.Equals(types.TypeUntyped) && expected.Equals(types.TypeUnknown) {
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
	if ok := checkFitness(ctx, mod, rhsType, targetType, valueExpr, typeNode); !ok {
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
			diag = diag.WithPrimaryLabel(mod.FilePath, valueExpr.Loc(), fmt.Sprintf("type '%s'", rhsType.String())).
				WithSecondaryLabel(mod.FilePath, typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
		} else {
			diag = diag.WithPrimaryLabel(mod.FilePath, valueExpr.Loc(), "implicit conversion may lose precision")
		}

		ctx.Diagnostics.Add(
			diag.WithHelp(fmt.Sprintf("use an explicit cast: %s(value)", targetType.String())),
		)

	case Incompatible:
		// Cannot convert
		diag := diagnostics.NewError(getConversionError(rhsType, targetType, compatibility))

		// Add dual labels if we have type node location
		if typeNode != nil {
			diag = diag.WithPrimaryLabel(mod.FilePath, valueExpr.Loc(), fmt.Sprintf("type '%s'", rhsType.String())).
				WithSecondaryLabel(mod.FilePath, typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
		} else {
			diag = diag.WithPrimaryLabel(mod.FilePath, valueExpr.Loc(), fmt.Sprintf("expected '%s', got '%s'", targetType.String(), rhsType.String()))
		}

		ctx.Diagnostics.Add(diag)
	}
}

func checkFitness(ctx *context_v2.CompilerContext, mod *context_v2.Module, rhsType, targetType types.SemType, valueExpr ast.Expression, typeNode ast.Node) bool {
	if rhsType.Equals(types.TypeUntyped) || rhsType.Equals(types.TypeI64) {
		if lit, ok := valueExpr.(*ast.BasicLit); ok && lit.Kind == ast.INT {
			if targetName, ok := types.GetPrimitiveName(targetType); ok && types.IsIntegerTypeName(targetName) {
				value, err := strconv.ParseInt(lit.Value, 0, 64)
				if err == nil && !fitsInType(value, targetType) {
					// Literal doesn't fit in target type
					minType := getMinimumTypeForValue(value)
					diag := diagnostics.NewError(fmt.Sprintf("integer literal %s overflows %s", lit.Value, targetType.String()))

					if typeNode != nil {
						// Show value location (primary) and variable/LHS location (secondary)
						diag = diag.WithPrimaryLabel(mod.FilePath, valueExpr.Loc(), fmt.Sprintf("need at least %s", minType)).
							WithSecondaryLabel(mod.FilePath, typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
					} else {
						diag = diag.WithPrimaryLabel(mod.FilePath, valueExpr.Loc(), fmt.Sprintf("value %s doesn't fit in %s", lit.Value, targetType.String()))
					}

					ctx.Diagnostics.Add(
						diag.WithHelp(fmt.Sprintf("%s can hold values in range: %s", targetType.String(), getTypeRange(targetType))),
					)
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
			}(i, param)
		}

		wg.Wait()

		returns := typeFromTypeNode(t.Result)

		return types.NewFunction(params, returns)

	default:
		return types.TypeUnknown
	}
}

// fitsInType checks if a value fits in a given type
func fitsInType(value int64, t types.SemType) bool {
	// Extract primitive type name
	name, ok := types.GetPrimitiveName(t)
	if !ok {
		return false // Non-primitive types don't have numeric ranges
	}

	switch name {
	case types.TYPE_I8:
		return value >= -128 && value <= 127
	case types.TYPE_I16:
		return value >= -32768 && value <= 32767
	case types.TYPE_I32:
		return value >= -2147483648 && value <= 2147483647
	case types.TYPE_I64:
		return true // int64 always fits
	case types.TYPE_U8:
		return value >= 0 && value <= 255
	case types.TYPE_U16:
		return value >= 0 && value <= 65535
	case types.TYPE_U32:
		return value >= 0 && value <= 4294967295
	case types.TYPE_U64:
		return value >= 0
	default:
		return false
	}
}

// getMinimumTypeForValue returns the smallest type that can hold the given value
func getMinimumTypeForValue(value int64) string {
	// For negative values, check signed types
	if value < 0 {
		if value >= -128 {
			return "i8"
		}
		if value >= -32768 {
			return "i16"
		}
		if value >= -2147483648 {
			return "i32"
		}
		return "i64"
	}

	// For positive values, prefer unsigned types but also show signed alternative
	if value <= 127 {
		return "i8 or u8"
	}
	if value <= 255 {
		return "u8 or i16"
	}
	if value <= 32767 {
		return "i16 or u16"
	}
	if value <= 65535 {
		return "u16 or i32"
	}
	if value <= 2147483647 {
		return "i32 or u32"
	}
	if value <= 4294967295 {
		return "u32 or i64"
	}
	return "i64 or u64"
}

// getTypeRange returns a human-readable range for integer types
func getTypeRange(t types.SemType) string {
	// Extract primitive type name
	name, ok := types.GetPrimitiveName(t)
	if !ok {
		return "unknown range"
	}

	switch name {
	case types.TYPE_I8:
		return "-128 to 127"
	case types.TYPE_I16:
		return "-32,768 to 32,767"
	case types.TYPE_I32:
		return "-2,147,483,648 to 2,147,483,647"
	case types.TYPE_I64:
		return "-9,223,372,036,854,775,808 to 9,223,372,036,854,775,807"
	case types.TYPE_U8:
		return "0 to 255"
	case types.TYPE_U16:
		return "0 to 65,535"
	case types.TYPE_U32:
		return "0 to 4,294,967,295"
	case types.TYPE_U64:
		return "0 to 18,446,744,073,709,551,615"
	default:
		return "unknown range"
	}
}
