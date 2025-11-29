package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/types"
	"fmt"
	"runtime"
	"strconv"
	"sync"
)

// CheckModule performs type checking on a module.
// It fills Symbol.Type for declarations and creates ExprTypes map for expressions.
func CheckModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	// Initialize ExprTypes map
	if mod.ExprTypes == nil {
		mod.ExprTypes = make(map[ast.Expression]types.SemType)
	}

	// Create a type checker instance
	tc := &typeChecker{
		ctx:          ctx,
		mod:          mod,
		exprTypes:    mod.ExprTypes,
		currentScope: mod.Symbols, // Start with module scope
	}

	// Check all declarations in the module
	if mod.AST != nil {
		for _, node := range mod.AST.Nodes {
			tc.checkNode(node)
		}
	}
}

// TODO: TYPE_NAME (string alias) is insufficient for complex types.
// We need a proper Type interface hierarchy to represent:
// - Generic types: Array<T>, Map<K,V>
// - Function types: fn(i32, i32) -> i64
// - Optional types: i32?
// - Result types: i32 ! Error
// - Struct/enum types with fields
// This refactor should happen before implementing complex type features.

// typeChecker holds state for type checking a module
type typeChecker struct {
	ctx          *context_v2.CompilerContext
	mod          *context_v2.Module
	exprTypes    map[ast.Expression]types.SemType
	currentScope *context_v2.SymbolTable // Current scope for name lookups
}

// checkNode type checks a single AST node
func (tc *typeChecker) checkNode(node ast.Node) {
	if node == nil {
		return
	}

	switch n := node.(type) {
	case *ast.VarDecl:
		tc.checkVarDecl(n, false)
	case *ast.ConstDecl:
		tc.checkVarDecl(n, true)
	case *ast.FuncDecl:
		tc.checkFuncDecl(n)
	case *ast.AssignStmt:
		tc.checkAssignStmt(n)
	case *ast.ReturnStmt:
		if n.Result != nil {
			tc.checkExpr(n.Result, types.TypeUnknown)
		}
	case *ast.IfStmt:
		tc.checkExpr(n.Cond, types.TypeBool)
		tc.checkBlock(n.Body)
		if n.Else != nil {
			tc.checkNode(n.Else)
		}
	case *ast.ForStmt:
		if n.Init != nil {
			tc.checkNode(n.Init)
		}
		if n.Cond != nil {
			tc.checkExpr(n.Cond, types.TypeBool)
		}
		if n.Post != nil {
			tc.checkNode(n.Post)
		}
		tc.checkBlock(n.Body)
	case *ast.Block:
		tc.checkBlock(n)
	case *ast.DeclStmt:
		tc.checkNode(n.Decl)
	case *ast.ExprStmt:
		tc.checkExpr(n.X, types.TypeUnknown)
	}
}

// checkVarDecl type checks variable/constant declarations
func (tc *typeChecker) checkVarDecl(decl interface{}, isConst bool) {
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
		sym, ok := tc.currentScope.Lookup(name)
		if !ok {
			// This is a local variable declaration - create a new symbol
			sym = &context_v2.Symbol{
				Name:     name,
				Kind:     context_v2.SymbolVariable,
				Type:     types.TypeUnknown,
				Exported: false,
				Decl:     item.Name, // Use the identifier as the Decl node
			}
			if err := tc.currentScope.Declare(name, sym); err != nil {
				tc.ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("redeclaration of '%s'", name)).
						WithPrimaryLabel(tc.mod.FilePath, item.Name.Loc(), "already declared in this scope"),
				)
				continue
			}
		}

		// Determine the type
		if item.Type != nil {
			// Explicit type annotation
			declType := tc.typeFromTypeNode(item.Type)
			sym.Type = declType

			// Check initializer if present
			if item.Value != nil {
				// Pass item.Name for secondary label (variable location)
				tc.checkAssignLike(declType, item.Name, item.Value)
			}
		} else if item.Value != nil {
			// Type inference from initializer
			rhsType := tc.checkExpr(item.Value, types.TypeUnknown)

			// If the RHS is UNTYPED, finalize it to a default type
			if rhsType.Equals(types.TypeUntyped) {
				rhsType = tc.finalizeUntyped(item.Value)
			}

			sym.Type = rhsType
		} else {
			// No type and no value - error
			tc.ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("cannot infer type for '%s'", name)).
					WithPrimaryLabel(tc.mod.FilePath, item.Name.Loc(), "missing type or initializer"),
			)
			sym.Type = types.TypeUnknown
		}
	}
}

// checkFuncDecl type checks a function declaration
func (tc *typeChecker) checkFuncDecl(decl *ast.FuncDecl) {
	// Create a new scope for the function body
	funcScope := context_v2.NewSymbolTable(tc.currentScope)

	// Add parameters to the function scope
	if decl.Type != nil && decl.Type.Params != nil {
		for _, param := range decl.Type.Params {
			if param.Name != nil {
				paramType := tc.typeFromTypeNode(param.Type)
				sym := &context_v2.Symbol{
					Name:     param.Name.Name,
					Kind:     context_v2.SymbolParameter,
					Type:     paramType,
					Exported: false,
					Decl:     param.Name, // Use IdentifierExpr as Decl
				}
				funcScope.Declare(param.Name.Name, sym)
			}
		}
	}
	// Check the body with the function scope
	if decl.Body != nil {
		prevScope := tc.currentScope
		tc.currentScope = funcScope
		tc.checkBlock(decl.Body)
		tc.currentScope = prevScope
	}
}

// checkAssignStmt type checks an assignment statement
func (tc *typeChecker) checkAssignStmt(stmt *ast.AssignStmt) {
	// Get the type of the LHS
	lhsType := tc.checkExpr(stmt.Lhs, types.TypeUnknown)

	// Check the RHS with the LHS type as context
	// Pass stmt.Lhs for LHS location in error messages
	tc.checkAssignLike(lhsType, stmt.Lhs, stmt.Rhs)
}

// checkBlock type checks a block of statements
func (tc *typeChecker) checkBlock(block *ast.Block) {
	if block == nil {
		return
	}
	for _, node := range block.Nodes {
		tc.checkNode(node)
	}
}

// checkExpr type checks an expression and returns its type.
// expected provides contextual type information (TypeUnknown if no context).
// This function now uses the separate inference and compatibility modules.
func (tc *typeChecker) checkExpr(expr ast.Expression, expected types.SemType) types.SemType {
	if expr == nil {
		return types.TypeUnknown
	}

	// Check if we already computed this expression's type
	if t, ok := tc.exprTypes[expr]; ok {
		return t
	}

	// First, infer the type based on the expression structure
	inferredType := tc.inferExprType(expr)

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
			resultType = tc.contextualizeUntyped(lit, expectedForLit)
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

	// Store the result
	tc.exprTypes[expr] = resultType
	return resultType
}

// checkAssignLike checks assignment-like operations with better error reporting
// typeNode: optional AST node for the target type (for location info)
// valueExpr: the value expression being assigned
// targetType: the expected/target type
func (tc *typeChecker) checkAssignLike(targetType types.SemType, typeNode ast.Node, valueExpr ast.Expression) {
	rhsType := tc.checkExpr(valueExpr, targetType)

	// Special check for integer literals: ensure they fit in the target type
	if ok := checkFitness(tc, rhsType, targetType, valueExpr, typeNode); !ok {
		return
	}

	// Check type compatibility
	compatibility := checkTypeCompatibility(rhsType, targetType)

	switch compatibility {
	case Identical, Assignable, LosslessConvertible:
		// OK - assignment is valid
		// If RHS is UNTYPED, update its type to target
		if rhsType.Equals(types.TypeUntyped) {
			tc.exprTypes[valueExpr] = targetType
		}
		return

	case LossyConvertible:
		// Requires explicit cast
		diag := diagnostics.NewError(getConversionError(rhsType, targetType, compatibility))

		// Add dual labels if we have type node location
		if typeNode != nil {
			diag = diag.WithPrimaryLabel(tc.mod.FilePath, valueExpr.Loc(), fmt.Sprintf("type '%s'", rhsType.String())).
				WithSecondaryLabel(tc.mod.FilePath, typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
		} else {
			diag = diag.WithPrimaryLabel(tc.mod.FilePath, valueExpr.Loc(), "implicit conversion may lose precision")
		}

		tc.ctx.Diagnostics.Add(
			diag.WithHelp(fmt.Sprintf("use an explicit cast: %s(value)", targetType.String())),
		)

	case Incompatible:
		// Cannot convert
		diag := diagnostics.NewError(getConversionError(rhsType, targetType, compatibility))

		// Add dual labels if we have type node location
		if typeNode != nil {
			diag = diag.WithPrimaryLabel(tc.mod.FilePath, valueExpr.Loc(), fmt.Sprintf("type '%s'", rhsType.String())).
				WithSecondaryLabel(tc.mod.FilePath, typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
		} else {
			diag = diag.WithPrimaryLabel(tc.mod.FilePath, valueExpr.Loc(), fmt.Sprintf("expected '%s', got '%s'", targetType.String(), rhsType.String()))
		}

		tc.ctx.Diagnostics.Add(diag)
	}
}

func checkFitness(tc *typeChecker, rhsType, targetType types.SemType, valueExpr ast.Expression, typeNode ast.Node) bool {
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
						diag = diag.WithPrimaryLabel(tc.mod.FilePath, valueExpr.Loc(), fmt.Sprintf("need at least %s", minType)).
							WithSecondaryLabel(tc.mod.FilePath, typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String()))
					} else {
						diag = diag.WithPrimaryLabel(tc.mod.FilePath, valueExpr.Loc(), fmt.Sprintf("value %s doesn't fit in %s", lit.Value, targetType.String()))
					}

					tc.ctx.Diagnostics.Add(
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
func (tc *typeChecker) typeFromTypeNode(typeNode ast.TypeNode) types.SemType {
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
		elementType := tc.typeFromTypeNode(t.ElType)
		length := -1 // Dynamic array by default
		if t.Len != nil {
			// TODO: keep all array dynamic now
		}
		return types.NewArray(elementType, length)

	case *ast.OptionalType:
		// Optional type: T?
		innerType := tc.typeFromTypeNode(t.Base)
		return types.NewOptional(innerType)

	case *ast.ResultType:
		// Result type: T ! E
		okType := tc.typeFromTypeNode(t.Value)
		errType := tc.typeFromTypeNode(t.Error)
		return types.NewResult(okType, errType)

	case *ast.StructType:
		// Struct type: struct { .x: i32, .y: i32 }
		fields := make([]types.StructField, len(t.Fields))

		// Process struct fields in parallel with bounded concurrency
		var wg sync.WaitGroup
		semaphore := make(chan struct{}, runtime.NumCPU())

		for i, f := range t.Fields {
			wg.Add(1)
			go func(idx int, field ast.Field) {
				defer wg.Done()
				semaphore <- struct{}{}        // Acquire semaphore
				defer func() { <-semaphore }() // Release semaphore

				fieldName := ""
				if field.Name != nil {
					fieldName = field.Name.Name
				}
				fields[idx] = types.StructField{
					Name: fieldName,
					Type: tc.typeFromTypeNode(field.Type),
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
		semaphore := make(chan struct{}, runtime.NumCPU())

		for i, param := range t.Params {
			wg.Add(1)
			go func(idx int, par ast.Field) {
				defer wg.Done()
				semaphore <- struct{}{}        // Acquire semaphore
				defer func() { <-semaphore }() // Release semaphore
				param := &params[idx]
				param.Name = par.Name.Name
				param.Type = tc.typeFromTypeNode(par.Type)
			}(i, param)
		}

		wg.Wait()

		returns := tc.typeFromTypeNode(t.Result)

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
