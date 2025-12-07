package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/consteval"
	"compiler/internal/semantics/controlflow"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/tokens"
	"compiler/internal/types"
	str "compiler/internal/utils/strings"

	"fmt"
	"strconv"
	"strings"
	"sync"
)

// TypeCheckMethodSignatures only processes method signatures to attach methods to types
// This must run before CheckModule so all methods are available
func TypeCheckMethodSignatures(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	if mod.AST == nil {
		return
	}

	if ctx.Debug {
		fmt.Printf("    [TypeCheckMethodSignatures for %s]\n", mod.ImportPath)
	}

	// Reset CurrentScope to ModuleScope
	mod.CurrentScope = mod.ModuleScope

	// CRITICAL: Check type declarations FIRST so receiver types are resolved
	for _, node := range mod.AST.Nodes {
		if typeDecl, ok := node.(*ast.TypeDecl); ok {
			checkTypeDecl(ctx, mod, typeDecl)
		}
	}

	// Now process method declarations - attach them to types
	methodCount := 0
	for _, node := range mod.AST.Nodes {
		if methodDecl, ok := node.(*ast.MethodDecl); ok {
			if ctx.Debug {
				fmt.Printf("      [Found method %s]\n", methodDecl.Name.Name)
			}
			checkMethodSignatureOnly(ctx, mod, methodDecl)
			methodCount++
		}
	}

	if ctx.Debug {
		fmt.Printf("    [Processed %d methods]\n", methodCount)
	}
}

// CheckModule performs type checking on a module.
// Type declarations and method signatures are already checked in phase 4a.
// This phase checks function bodies, variables, and method bodies.
func CheckModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {

	// Reset CurrentScope to ModuleScope before type checking
	// This ensures we're starting from the module-level scope
	mod.CurrentScope = mod.ModuleScope

	// Check all declarations in the module
	// SKIP type declarations and method signatures - already done in phase 4a
	if mod.AST != nil {
		// Check everything EXCEPT type declarations (already done in phase 4a)
		for _, node := range mod.AST.Nodes {
			if _, ok := node.(*ast.TypeDecl); !ok {
				checkNode(ctx, mod, node)
			}
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
	case *ast.MethodDecl:
		checkMethodDecl(ctx, mod, n)
	case *ast.TypeDecl:
		checkTypeDecl(ctx, mod, n)
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
							WithCode(diagnostics.ErrTypeMismatch).
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

		// Check condition
		checkExpr(ctx, mod, n.Cond, types.TypeBool)

		// Check for constant condition (dead code detection)
		if constVal := consteval.EvaluateExpr(ctx, mod, n.Cond); constVal != nil {
			if boolVal, ok := constVal.AsBool(); ok {
				if boolVal {
					// Condition is always true - else branch is dead code
					if n.Else != nil {
						ctx.Diagnostics.Add(
							diagnostics.NewWarning("condition is always true").
								WithCode(diagnostics.WarnConstantConditionTrue).
								WithPrimaryLabel(n.Cond.Loc(), "this condition is always true").
								WithSecondaryLabel(n.Else.Loc(), "this branch will never execute").
								WithHelp("remove the if statement or the unreachable else branch"),
						)
					}
				} else {
					// Condition is always false - then branch is dead code
					ctx.Diagnostics.Add(
						diagnostics.NewWarning("condition is always false").
							WithCode(diagnostics.WarnConstantConditionFalse).
							WithPrimaryLabel(n.Cond.Loc(), "this condition is always false").
							WithSecondaryLabel(n.Body.Loc(), "this branch will never execute").
							WithHelp("remove the if statement or fix the condition"),
					)
				}
			}
		}

		// Analyze condition for type narrowing
		thenNarrowing, elseNarrowing := analyzeConditionForNarrowing(ctx, mod, n.Cond, nil)

		// Check then branch with narrowing
		if n.Body != nil {
			applyNarrowingToBlock(ctx, mod, n.Body, thenNarrowing)
		}

		// Check else/else-if chain with narrowing
		if n.Else != nil {
			applyNarrowingToElse(ctx, mod, n.Else, elseNarrowing)
		}
	case *ast.ForStmt:
		// Enter for loop scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		// Check range expression first to infer element type
		var rangeElemType types.SemType = types.TypeUnknown
		if n.Range != nil {
			rangeType := checkExpr(ctx, mod, n.Range, types.TypeUnknown)
			// Extract element type from array - for integer ranges this will be i32
			if arrayType, ok := rangeType.(*types.ArrayType); ok {
				rangeElemType = arrayType.Element
			}
		}

		// Check iterator
		if n.Iterator != nil {
			if varDecl, ok := n.Iterator.(*ast.VarDecl); ok {
				// For VarDecl iterator: apply types and validate structure
				// Both single and dual variables get the same type (range element type)
				for _, item := range varDecl.Decls {
					// Validate iterator declaration structure
					if item.Value != nil {
						ctx.Diagnostics.Add(diagnostics.NewError("for loop iterator cannot have initializer").
							WithPrimaryLabel(item.Value.Loc(), "remove this initializer"))
					}

					// Skip placeholder '_' - it doesn't get a symbol or type
					if item.Name.Name == "_" {
						continue
					}

					if item.Type != nil {
						// Explicit type annotation - use it
						declType := typeFromTypeNodeWithContext(ctx, mod, item.Type)
						if sym, ok := mod.CurrentScope.GetSymbol(item.Name.Name); ok {
							sym.Type = declType
						}
					} else {
						// Infer type: all variables get range element type
						// For numeric ranges like 0..10, both index and value are i32
						if sym, ok := mod.CurrentScope.GetSymbol(item.Name.Name); ok {
							sym.Type = rangeElemType
						}
					}
				}
			} else {
				// For IdentifierExpr iterator: validate it exists
				checkNode(ctx, mod, n.Iterator)
			}
		}
		checkBlock(ctx, mod, n.Body)
	case *ast.WhileStmt:
		// Enter while loop scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		checkExpr(ctx, mod, n.Cond, types.TypeBool)

		// Check for constant condition
		if constVal := consteval.EvaluateExpr(ctx, mod, n.Cond); constVal != nil {
			if boolVal, ok := constVal.AsBool(); ok {
				// Only warn if condition is always false - loop never executes
				// Always-true conditions are often intentional (infinite loops with break)
				if !boolVal {
					ctx.Diagnostics.Add(
						diagnostics.NewWarning("condition is always false").
							WithCode(diagnostics.WarnConstantConditionFalse).
							WithPrimaryLabel(n.Cond.Loc(), "this condition is always false").
							WithSecondaryLabel(n.Body.Loc(), "this loop will never execute").
							WithHelp("remove the while loop or fix the condition"),
					)
				}
			}
		}

		// Analyze condition for narrowing in loop body
		loopNarrowing, _ := analyzeConditionForNarrowing(ctx, mod, n.Cond, nil)
		applyNarrowingToBlock(ctx, mod, n.Body, loopNarrowing)
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
			declType := typeFromTypeNodeWithContext(ctx, mod, item.Type)

			// Disallow 'none' as a variable/const type
			if declType.Equals(types.TypeNone) {
				declKind := "variable"
				if isConst {
					declKind = "constant"
				}
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("%s '%s' cannot have type 'none'", declKind, name)).
						WithPrimaryLabel(item.Name.Loc(), "'none' is not a valid type for variables or constants").
						WithHelp("'none' is only used as a value for optional types like i32?"),
				)
				sym.Type = types.TypeUnknown
				continue
			}

			sym.Type = declType

			// Check initializer if present
			if item.Value != nil {
				// Pass item.Type for type location in error messages
				checkAssignLike(ctx, mod, declType, item.Type, item.Value)

				// Try to evaluate the initializer as a compile-time constant
				if constVal := consteval.EvaluateExpr(ctx, mod, item.Value); constVal != nil && constVal.IsConstant() {
					sym.ConstValue = constVal
				}
			} else if isConst {
				// Constants must have an initializer even with explicit type
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("constant '%s' must be initialized", name)).
						WithPrimaryLabel(item.Name.Loc(), "constants require an initializer"),
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

			// Try to evaluate the initializer as a compile-time constant
			// This enables constant propagation for code like: let i := 10; arr[i]
			if constVal := consteval.EvaluateExpr(ctx, mod, item.Value); constVal != nil && constVal.IsConstant() {
				sym.ConstValue = constVal
			}
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
		expectedReturnType = typeFromTypeNodeWithContext(ctx, mod, decl.Type.Result)
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
				paramType := typeFromTypeNodeWithContext(ctx, mod, param.Type)
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

// checkSelectorExpr validates that a field or method exists on a struct
// checkBinaryExpr validates that operands of a binary expression have compatible types
func checkBinaryExpr(ctx *context_v2.CompilerContext, _ *context_v2.Module, expr *ast.BinaryExpr, lhsType, rhsType types.SemType) {
	// Skip if either type is unknown (error already reported)
	if lhsType.Equals(types.TypeUnknown) || rhsType.Equals(types.TypeUnknown) {
		return
	}

	// Allow untyped operands - they will be contextualized
	if types.IsUntyped(lhsType) || types.IsUntyped(rhsType) {
		return
	}

	// For arithmetic operators, both operands must be numeric
	switch expr.Op.Kind {
	case tokens.PLUS_TOKEN, tokens.MINUS_TOKEN, tokens.MUL_TOKEN, tokens.DIV_TOKEN, tokens.MOD_TOKEN:
		lhsNumeric := types.IsNumericType(lhsType)
		rhsNumeric := types.IsNumericType(rhsType)

		if !lhsNumeric || !rhsNumeric {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("invalid operation: %s (mismatched types %s and %s)", expr.Op.Value, lhsType.String(), rhsType.String())).
					WithCode(diagnostics.ErrTypeMismatch).
					WithPrimaryLabel(expr.Loc(), fmt.Sprintf("cannot use '%s' operator with %s and %s", expr.Op.Value, lhsType.String(), rhsType.String())),
			)
			return
		}

	case tokens.DOUBLE_EQUAL_TOKEN, tokens.NOT_EQUAL_TOKEN:
		// Special case: allow comparing optional with none
		lhsOptional := false
		rhsOptional := false
		lhsNone := lhsType == types.TypeNone
		rhsNone := rhsType == types.TypeNone

		if _, ok := lhsType.(*types.OptionalType); ok {
			lhsOptional = true
		}
		if _, ok := rhsType.(*types.OptionalType); ok {
			rhsOptional = true
		}

		// Allow T? == none or none == T?
		if (lhsOptional && rhsNone) || (rhsOptional && lhsNone) {
			return // Valid comparison
		}

		// Comparison operators require compatible types
		compatibility := checkTypeCompatibility(lhsType, rhsType)
		if compatibility == Incompatible {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("invalid operation: cannot compare %s and %s", lhsType.String(), rhsType.String())).
					WithCode(diagnostics.ErrTypeMismatch).
					WithPrimaryLabel(expr.Loc(), "incompatible types in comparison"),
			)
		}

	case tokens.LESS_TOKEN, tokens.LESS_EQUAL_TOKEN, tokens.GREATER_TOKEN, tokens.GREATER_EQUAL_TOKEN:
		// Ordering operators require numeric types
		lhsNumeric := types.IsNumericType(lhsType)
		rhsNumeric := types.IsNumericType(rhsType)

		if !lhsNumeric || !rhsNumeric {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("invalid operation: cannot compare %s and %s", lhsType.String(), rhsType.String())).
					WithCode(diagnostics.ErrTypeMismatch).
					WithPrimaryLabel(expr.Loc(), "ordering comparison requires numeric types"),
			)
		}

	case tokens.AND_TOKEN, tokens.OR_TOKEN:
		// Logical operators require bool types
		if !lhsType.Equals(types.TypeBool) || !rhsType.Equals(types.TypeBool) {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("invalid operation: logical operator requires bool operands, got %s and %s", lhsType.String(), rhsType.String())).
					WithCode(diagnostics.ErrTypeMismatch).
					WithPrimaryLabel(expr.Loc(), "logical operators require bool types"),
			)
		}
	}
}

// checkCastExpr validates that a cast expression is valid
func checkCastExpr(ctx *context_v2.CompilerContext, _ *context_v2.Module, expr *ast.CastExpr, sourceType, targetType types.SemType) {
	// Skip if target type is unknown
	if targetType.Equals(types.TypeUnknown) {
		return
	}

	// Skip further validation if source type is unknown (error already reported)
	if sourceType.Equals(types.TypeUnknown) {
		return
	}

	// Allow untyped literals to be cast to any compatible type
	if types.IsUntyped(sourceType) {
		return
	}

	// Check if cast is valid
	compatibility := checkTypeCompatibility(sourceType, targetType)

	// For struct types, check structural compatibility (unwrap NamedType on both sides)
	srcUnwrapped := types.UnwrapType(sourceType)
	dstUnwrapped := types.UnwrapType(targetType)

	// Handle map type casts
	if srcMap, ok := srcUnwrapped.(*types.MapType); ok {
		if dstMap, ok := dstUnwrapped.(*types.MapType); ok {
			// Allow cast if key and value types are compatible
			keyCompat := checkTypeCompatibility(srcMap.Key, dstMap.Key)
			valueCompat := checkTypeCompatibility(srcMap.Value, dstMap.Value)

			if keyCompat != Incompatible && valueCompat != Incompatible {
				return // Compatible map cast
			}

			// Build error for incompatible map cast
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("cannot cast %s to %s", sourceType.String(), targetType.String())).
					WithCode(diagnostics.ErrInvalidCast).
					WithPrimaryLabel(expr.Loc(), "incompatible map types").
					WithHelp(fmt.Sprintf("key types: %s vs %s, value types: %s vs %s",
						srcMap.Key.String(), dstMap.Key.String(),
						srcMap.Value.String(), dstMap.Value.String())),
			)
			return
		}
	}

	if srcStruct, ok := srcUnwrapped.(*types.StructType); ok {
		if dstStruct, ok := dstUnwrapped.(*types.StructType); ok {
			// Check if struct types are compatible by structure
			if missingFields, mismatchedFields := analyzeStructCompatibility(srcStruct, dstStruct); len(missingFields) > 0 || len(mismatchedFields) > 0 {
				// Build detailed error message
				diag := diagnostics.NewError(fmt.Sprintf("cannot cast %s to %s", sourceType.String(), targetType.String())).
					WithCode(diagnostics.ErrInvalidCast).
					WithPrimaryLabel(expr.Loc(), "invalid cast")

				// Add help message with field details
				var helpParts []string
				if len(missingFields) > 0 {
					helpParts = append(helpParts, fmt.Sprintf("missing %s: %s", str.Pluralize("field", "fields", len(missingFields)), strings.Join(missingFields, ", ")))
				}
				if len(mismatchedFields) > 0 {
					helpParts = append(helpParts, fmt.Sprintf("type mismatch in %s: %s", str.Pluralize("field", "fields", len(mismatchedFields)), strings.Join(mismatchedFields, ", ")))
				}
				if len(helpParts) > 0 {
					diag = diag.WithHelp(strings.Join(helpParts, "; "))
				}

				ctx.Diagnostics.Add(diag)
			}
			return
		}
	}

	// Allow explicit casts between numeric types
	if types.IsNumericType(sourceType) && types.IsNumericType(targetType) {
		return
	}

	// Otherwise, check standard compatibility
	if compatibility == Incompatible {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("cannot cast %s to %s", sourceType.String(), targetType.String())).
				WithCode(diagnostics.ErrInvalidCast).
				WithPrimaryLabel(expr.Loc(), "invalid cast"),
		)
	}
}

// checkCompositeLit validates that a composite literal matches its target type
func checkCompositeLit(ctx *context_v2.CompilerContext, mod *context_v2.Module, lit *ast.CompositeLit, targetType types.SemType) {
	// Unwrap NamedType to get the underlying type
	underlyingType := types.UnwrapType(targetType)

	// Handle struct literals
	if structType, ok := underlyingType.(*types.StructType); ok {
		checkStructLiteral(ctx, lit, structType, targetType)
		return
	}

	// Handle map literals
	if mapType, ok := underlyingType.(*types.MapType); ok {
		checkMapLiteral(ctx, mod, lit, mapType)
		return
	}
}

// checkStructLiteral validates struct literal elements
func checkStructLiteral(ctx *context_v2.CompilerContext, lit *ast.CompositeLit, structType *types.StructType, targetType types.SemType) {
	// Build a map of provided fields
	providedFields := make(map[string]bool)
	for _, elem := range lit.Elts {
		if kv, ok := elem.(*ast.KeyValueExpr); ok {
			if key, ok := kv.Key.(*ast.IdentifierExpr); ok {
				fieldName := key.Name
				providedFields[fieldName] = true
			}
		}
	}

	// Check for missing required fields
	var missingFields []string
	for _, field := range structType.Fields {
		if !providedFields[field.Name] {
			missingFields = append(missingFields, field.Name)
		}
	}

	// Only report error if there are missing fields (extra fields are allowed and will be stripped)
	if len(missingFields) > 0 {
		// Use the original type's name if available (for better error messages)
		typeName := targetType.String()
		errorMsg := fmt.Sprintf("cannot convert composite literal to type '%s'", typeName)
		diag := diagnostics.NewError(errorMsg).
			WithCode(diagnostics.ErrTypeMismatch).
			WithPrimaryLabel(lit.Loc(), "incompatible struct literal")

		// Build note with missing fields list
		noteMsg := fmt.Sprintf("missing %s (%s)", str.Pluralize("field", "fields", len(missingFields)), strings.Join(missingFields, ", "))
		diag = diag.WithNote(noteMsg)

		ctx.Diagnostics.Add(diag)
	}
}

// checkMapLiteral validates map literal key/value types
func checkMapLiteral(ctx *context_v2.CompilerContext, mod *context_v2.Module, lit *ast.CompositeLit, mapType *types.MapType) {
	// Validate that all keys have compatible types with the map's key type
	// Validate that all values have compatible types with the map's value type
	for _, elem := range lit.Elts {
		kv, ok := elem.(*ast.KeyValueExpr)
		if !ok {
			// Non key-value elements in map literal (shouldn't happen with correct parsing)
			continue
		}

		// Check key type compatibility
		keyType := inferExprType(ctx, mod, kv.Key)
		keyCompat := checkTypeCompatibility(keyType, mapType.Key)
		if keyCompat == Incompatible {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("map key type mismatch: expected %s, found %s", mapType.Key.String(), keyType.String())).
					WithPrimaryLabel(kv.Key.Loc(), "incompatible key type").
					WithHelp(fmt.Sprintf("expected type %s", mapType.Key.String())),
			)
		}

		// Check value type compatibility
		valueType := inferExprType(ctx, mod, kv.Value)
		valueCompat := checkTypeCompatibility(valueType, mapType.Value)
		if valueCompat == Incompatible {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("map value type mismatch: expected %s, found %s", mapType.Value.String(), valueType.String())).
					WithPrimaryLabel(kv.Value.Loc(), "incompatible value type").
					WithHelp(fmt.Sprintf("expected type %s", mapType.Value.String())),
			)
		}
	}
}

// areStructsCompatible checks if two struct types are structurally compatible
func areStructsCompatible(src, dst *types.StructType) bool {
	missing, mismatched := analyzeStructCompatibility(src, dst)
	return len(missing) == 0 && len(mismatched) == 0
}

// analyzeStructCompatibility checks struct compatibility and returns detailed mismatch info
// Returns:
//   - missingFields: list of field names that are required in dst but missing in src
//   - mismatchedFields: list of "fieldName (expected Type, found Type)" for type mismatches
func analyzeStructCompatibility(src, dst *types.StructType) (missingFields []string, mismatchedFields []string) {
	// Check if destination has all required fields with matching or compatible types
	for _, dstField := range dst.Fields {
		found := false
		for _, srcField := range src.Fields {
			if srcField.Name == dstField.Name {
				found = true
				// Check if types match or are compatible
				if srcField.Type.Equals(dstField.Type) {
					break // Exact match
				}
				// Allow untyped literals to match concrete types
				compatibility := checkTypeCompatibility(srcField.Type, dstField.Type)
				if compatibility == Incompatible {
					// Type mismatch
					mismatchedFields = append(mismatchedFields,
						fmt.Sprintf("%s (expected %s, found %s)", dstField.Name, dstField.Type.String(), srcField.Type.String()))
				}
				break
			}
		}
		if !found {
			// Missing required field
			missingFields = append(missingFields, dstField.Name)
		}
	}
	return missingFields, mismatchedFields
}

func checkSelectorExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.SelectorExpr) {
	// Infer the base type
	baseType := inferExprType(ctx, mod, expr.X)

	// Skip validation if base type is unknown (error already reported)
	if baseType.Equals(types.TypeUnknown) {
		return
	}

	fieldName := expr.Sel.Name

	// Handle NamedType and anonymous StructType
	var typeSym *symbols.Symbol
	var structType *types.StructType
	var typeName string

	if namedType, ok := baseType.(*types.NamedType); ok {
		// It's a named type - look up the type symbol for method resolution
		typeName = namedType.Name

		// First check current module's scope
		if sym, found := mod.ModuleScope.Lookup(typeName); found {
			typeSym = sym
		} else {
			// Not in current module, search imported modules
			for _, importPath := range mod.ImportAliasMap {
				if importedMod, exists := ctx.GetModule(importPath); exists {
					if sym, ok := importedMod.ModuleScope.GetSymbol(typeName); ok && sym.Kind == symbols.SymbolType {
						typeSym = sym
						break
					}
				}
			}
		}

		// Get the underlying struct for field access
		underlying := types.UnwrapType(namedType)
		structType, _ = underlying.(*types.StructType)
	} else if st, ok := baseType.(*types.StructType); ok {
		// Anonymous struct - no methods, only fields
		structType = st
	} else {
		// Not a struct or named type with struct underlying - could be module access or other
		return
	}

	// Check if it's a field
	if structType != nil {
		for _, field := range structType.Fields {
			if field.Name == fieldName {
				return // Field exists
			}
		}
	}

	// Check if it's a method on the named type
	if typeSym != nil && typeSym.Methods != nil {
		if _, ok := typeSym.Methods[fieldName]; ok {
			return // Method exists
		}
	}

	// Field/method not found
	if typeName != "" {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("field or method '%s' not found on type '%s'", fieldName, typeName)).
				WithCode(diagnostics.ErrFieldNotFound).
				WithPrimaryLabel(expr.Sel.Loc(), fmt.Sprintf("'%s' does not exist", fieldName)),
		)
	} else {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("field '%s' not found on anonymous struct", fieldName)).
				WithCode(diagnostics.ErrFieldNotFound).
				WithPrimaryLabel(expr.Sel.Loc(), fmt.Sprintf("'%s' does not exist", fieldName)),
		)
	}
}

// checkMethodSignatureOnly processes only the method signature (receiver + parameters + return type)
// and attaches the method to its type. Does NOT check the body.
func checkMethodSignatureOnly(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.MethodDecl) {
	if ctx.Debug {
		fmt.Printf("        [checkMethodSignatureOnly: %s]\n", decl.Name.Name)
	}

	// Get receiver type
	if decl.Receiver == nil || decl.Receiver.Type == nil {
		if ctx.Debug {
			fmt.Printf("          [No receiver]\n")
		}
		return
	}

	receiverType := typeFromTypeNodeWithContext(ctx, mod, decl.Receiver.Type)

	// Only process valid named types
	if receiverType.Equals(types.TypeUnknown) {
		return
	}

	// Unwrap reference types: &T -> T
	if refType, ok := receiverType.(*types.ReferenceType); ok {
		receiverType = refType.Inner
	}

	// Extract type name - only NamedType can have methods
	var typeName string
	if namedType, ok := receiverType.(*types.NamedType); ok {
		typeName = namedType.Name
	} else {
		// Anonymous types cannot have methods
		return
	}

	if typeName == "" {
		return
	}

	// Find the type symbol - could be in current module or imported module
	typeSym, found := mod.ModuleScope.Lookup(typeName)
	if !found {
		// Not in current module, search imported modules
		for _, importPath := range mod.ImportAliasMap {
			if importedMod, exists := ctx.GetModule(importPath); exists {
				if sym, ok := importedMod.ModuleScope.GetSymbol(typeName); ok && sym.Kind == symbols.SymbolType {
					typeSym = sym
					found = true
					break
				}
			}
		}
	}

	// Type check the method signature and attach to type symbol
	if found && typeSym.Kind == symbols.SymbolType && decl.Type != nil {
		funcType := typeFromTypeNodeWithContext(ctx, mod, decl.Type).(*types.FunctionType)

		// Attach method to the type symbol's Methods map
		if typeSym.Methods == nil {
			typeSym.Methods = make(map[string]*symbols.MethodInfo)
		}

		// Create method info and attach to type symbol
		typeSym.Methods[decl.Name.Name] = &symbols.MethodInfo{
			Name:     decl.Name.Name,
			FuncType: funcType,
		}
	}
}

// checkMethodDecl type checks method body (signature already checked in phase 4a)
func checkMethodDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.MethodDecl) {
	// Safety check: Scope should be set during collection phase
	if decl.Scope == nil {
		ctx.ReportError(fmt.Sprintf("internal error: method '%s' has nil scope", decl.Name.Name), decl.Loc())
		return
	}

	methodScope := decl.Scope.(*table.SymbolTable)

	// Enter method scope
	defer mod.EnterScope(methodScope)()

	// Set expected return type for validation
	var expectedReturnType types.SemType = types.TypeVoid
	if decl.Type != nil && decl.Type.Result != nil {
		expectedReturnType = typeFromTypeNodeWithContext(ctx, mod, decl.Type.Result)
	}
	oldReturnType := mod.CurrentFunctionReturnType
	mod.CurrentFunctionReturnType = expectedReturnType
	defer func() {
		mod.CurrentFunctionReturnType = oldReturnType
	}()

	// Add receiver to method scope
	if decl.Receiver != nil && decl.Receiver.Name != nil {
		receiverSym, ok := methodScope.GetSymbol(decl.Receiver.Name.Name)
		if ok && decl.Receiver.Type != nil {
			receiverType := typeFromTypeNodeWithContext(ctx, mod, decl.Receiver.Type)
			receiverSym.Type = receiverType
		}
	}

	// Add parameters to the method scope with type information
	if decl.Type != nil && decl.Type.Params != nil {
		for _, param := range decl.Type.Params {
			if param.Name != nil {
				paramType := typeFromTypeNodeWithContext(ctx, mod, param.Type)
				psym, ok := methodScope.GetSymbol(param.Name.Name)
				if !ok {
					continue
				}
				psym.Type = paramType
			}
		}
	}

	// Check the body with the method scope
	if decl.Body != nil {
		checkBlock(ctx, mod, decl.Body)

		// Build control flow graph for the method
		// We'll wrap the method in a FuncDecl for CFG analysis
		// Create a temporary FuncDecl for CFG construction
		tempFuncDecl := &ast.FuncDecl{
			Name:     decl.Name,
			Type:     decl.Type,
			Body:     decl.Body,
			Scope:    decl.Scope,
			Location: decl.Location,
		}

		cfgBuilder := controlflow.NewCFGBuilder(ctx, mod)
		cfg := cfgBuilder.BuildFunctionCFG(tempFuncDecl)

		// Analyze return paths
		controlflow.AnalyzeReturns(ctx, mod, tempFuncDecl, cfg)
	}
}

// checkTypeDecl fills in the type for user-defined type declarations
func checkTypeDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.TypeDecl) {
	if decl.Name == nil || decl.Type == nil {
		return
	}

	typeName := decl.Name.Name

	// Look up the type symbol (created during collection)
	sym, ok := mod.CurrentScope.Lookup(typeName)
	if !ok {
		// Symbol should exist from collection phase
		ctx.ReportError(fmt.Sprintf("internal error: type symbol '%s' not found", typeName), decl.Loc())
		return
	}

	// Convert the AST type node to a semantic type
	semType := typeFromTypeNodeWithContext(ctx, mod, decl.Type)

	// Wrap the type with a NamedType to preserve the name
	// This enables nominal typing and method attachment
	namedType := types.NewNamed(typeName, semType)

	// Update the symbol's type
	sym.Type = namedType

	// Special handling for enums: compute variant values and update variant symbols
	if enumNode, ok := decl.Type.(*ast.EnumType); ok {
		// Current value tracker for sequential numbering
		currentValue := int64(0)

		for _, variant := range enumNode.Variants {
			variantName := variant.Name.Name
			qualifiedName := typeName + "::" + variantName

			// Compute variant value
			if variant.Value != nil {
				// Explicit value provided
				switch val := variant.Value.(type) {
				case *ast.BasicLit:
					if val.Kind == ast.INT {
						// Parse the integer literal
						parsedValue, err := strconv.ParseInt(val.Value, 0, 64)
						if err != nil {
							ctx.Diagnostics.Add(
								diagnostics.NewError(fmt.Sprintf("invalid integer value for enum variant '%s'", variantName)).
									WithPrimaryLabel(val.Loc(), "cannot parse as integer").
									WithHelp("use a valid integer literal"),
							)
							continue
						}
						currentValue = parsedValue
					} else {
						ctx.Diagnostics.Add(
							diagnostics.NewError(fmt.Sprintf("enum variant '%s' must have integer value", variantName)).
								WithPrimaryLabel(val.Loc(), "expected integer literal").
								WithHelp("use an integer literal (e.g., 42)"),
						)
						continue
					}
				case *ast.UnaryExpr:
					// Handle negative literals like -1
					if val.Op.Kind == tokens.MINUS_TOKEN {
						if lit, ok := val.X.(*ast.BasicLit); ok && lit.Kind == ast.INT {
							parsedValue, err := strconv.ParseInt(lit.Value, 0, 64)
							if err != nil {
								ctx.Diagnostics.Add(
									diagnostics.NewError(fmt.Sprintf("invalid integer value for enum variant '%s'", variantName)).
										WithPrimaryLabel(lit.Loc(), "cannot parse as integer").
										WithHelp("use a valid integer literal"),
								)
								continue
							}
							currentValue = -parsedValue
						} else {
							ctx.Diagnostics.Add(
								diagnostics.NewError(fmt.Sprintf("enum variant '%s' must have integer value", variantName)).
									WithPrimaryLabel(val.Loc(), "expected integer literal").
									WithHelp("use an integer literal (e.g., -42)"),
							)
							continue
						}
					} else {
						ctx.Diagnostics.Add(
							diagnostics.NewError(fmt.Sprintf("enum variant '%s' must have integer value", variantName)).
								WithPrimaryLabel(val.Loc(), "unexpected expression").
								WithHelp("use an integer literal"),
						)
						continue
					}
				default:
					ctx.Diagnostics.Add(
						diagnostics.NewError(fmt.Sprintf("enum variant '%s' must have constant integer value", variantName)).
							WithPrimaryLabel(variant.Value.Loc(), "expected integer literal").
							WithHelp("use an integer literal (e.g., 42)"),
					)
					continue
				}
			}
			// else: use currentValue as is (sequential numbering)

			// Look up the variant symbol (created during collection phase)
			variantSym, ok := mod.ModuleScope.GetSymbol(qualifiedName)
			if !ok {
				ctx.ReportError(fmt.Sprintf("internal error: enum variant symbol '%s' not found", qualifiedName), variant.Name.Loc())
				continue
			}

			// Update the variant symbol's type to the named enum type
			variantSym.Type = namedType

			// Increment for next variant (if no explicit value provided)
			currentValue++
		}
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

	// Track constant value propagation through assignments
	// If LHS is a simple identifier and RHS has a constant value, update the symbol
	if ident, ok := stmt.Lhs.(*ast.IdentifierExpr); ok {
		if sym, found := mod.CurrentScope.Lookup(ident.Name); found {
			// Try to evaluate the RHS as a constant
			if constVal := consteval.EvaluateExpr(ctx, mod, stmt.Rhs); constVal != nil {
				sym.ConstValue = constVal
			} else {
				// RHS is not constant - clear the constant value
				sym.ConstValue = nil
			}
		}
	}

	// TODO: Invalidate constant values for aliasing
	// When reference expressions (&x) are implemented, we should:
	// 1. Clear constant value when address is taken (addressOf expression)
	// 2. Clear all potentially aliased variables on reference assignment
	// 3. Consider inter-procedural effects (function calls with &params)
	// For now, method calls with reference receivers handle invalidation separately
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

	// Recursively validate subexpressions before type inference
	switch e := expr.(type) {
	case *ast.CallExpr:
		checkCallExpr(ctx, mod, e)

	case *ast.SelectorExpr:
		// Validate base expression first
		checkExpr(ctx, mod, e.X, types.TypeUnknown)
		checkSelectorExpr(ctx, mod, e)

	case *ast.BinaryExpr:
		// Recursively check operands
		lhsType := checkExpr(ctx, mod, e.X, types.TypeUnknown)
		rhsType := checkExpr(ctx, mod, e.Y, types.TypeUnknown)
		// Validate operand type compatibility
		checkBinaryExpr(ctx, mod, e, lhsType, rhsType)

	case *ast.UnaryExpr:
		// Recursively check operand
		checkExpr(ctx, mod, e.X, types.TypeUnknown)

	case *ast.IndexExpr:
		// Check both array and index expressions
		checkExpr(ctx, mod, e.X, types.TypeUnknown)
		checkExpr(ctx, mod, e.Index, types.TypeUnknown)

		// Perform bounds checking for fixed-size arrays with constant indices
		checkArrayBounds(ctx, mod, e)

	case *ast.ParenExpr:
		// Check inner expression
		return checkExpr(ctx, mod, e.X, expected)

	case *ast.CastExpr:
		// Check expression being cast and validate cast compatibility
		targetType := typeFromTypeNodeWithContext(ctx, mod, e.Type)
		// For composite literals, provide target type as context to allow untyped literal contextualization
		sourceType := checkExpr(ctx, mod, e.X, targetType)
		checkCastExpr(ctx, mod, e, sourceType, targetType)

	case *ast.CompositeLit:
		// Determine expected struct type for field contextualization
		var expectedStructType *types.StructType
		var expectedArrayType *types.ArrayType
		if !expected.Equals(types.TypeUnknown) {
			// Unwrap NamedType to get underlying struct or array
			unwrapped := types.UnwrapType(expected)
			expectedStructType, _ = unwrapped.(*types.StructType)
			if expectedStructType == nil {
				expectedArrayType, _ = unwrapped.(*types.ArrayType)
			}
		}

		// Check all element expressions in the composite literal
		for _, elem := range e.Elts {
			if kv, ok := elem.(*ast.KeyValueExpr); ok {
				// Struct field: check the value with field type as context
				if kv.Value != nil {
					fieldExpected := types.TypeUnknown
					// If we know the expected struct type, provide field type as context
					if expectedStructType != nil {
						if key, ok := kv.Key.(*ast.IdentifierExpr); ok {
							fieldName := key.Name
							for _, field := range expectedStructType.Fields {
								if field.Name == fieldName {
									fieldExpected = field.Type
									break
								}
							}
						}
					}
					checkExpr(ctx, mod, kv.Value, fieldExpected)
				}
			} else {
				// Array element: check with element type if known
				elemExpected := types.TypeUnknown
				if expectedArrayType != nil {
					elemExpected = expectedArrayType.Element
				}
				checkExpr(ctx, mod, elem, elemExpected)
			}
		}
		// Validate composite literal matches target type if specified
		if e.Type != nil {
			targetType := typeFromTypeNodeWithContext(ctx, mod, e.Type)
			checkCompositeLit(ctx, mod, e, targetType)
		}
	}

	// First, infer the type based on the expression structure
	inferredType := inferExprType(ctx, mod, expr)

	// Apply contextual typing for UNTYPED numeric literals only
	// Note: Struct literals, string literals, bool literals have concrete types immediately
	resultType := inferredType

	if types.IsUntyped(inferredType) && !expected.Equals(types.TypeUnknown) {
		// Only numeric literals (int/float) are untyped and need contextualization
		if lit, ok := expr.(*ast.BasicLit); ok {
			// If expected is optional, contextualize to inner type
			expectedForLit := expected
			if optType, ok := expected.(*types.OptionalType); ok {
				expectedForLit = optType.Inner
			}
			resultType = contextualizeUntyped(lit, expectedForLit)
		}
	}

	// Special handling for CompositeLit: if expected type is known and matches structure,
	// adopt the expected type (for better compatibility with named types)
	if compLit, ok := expr.(*ast.CompositeLit); ok && compLit.Type == nil {
		if !expected.Equals(types.TypeUnknown) {
			// Unwrap expected to get underlying struct or array
			expectedUnwrapped := types.UnwrapType(expected)
			if expectedStruct, ok := expectedUnwrapped.(*types.StructType); ok {
				// Check if inferred struct is compatible with expected
				if inferredStruct, ok := inferredType.(*types.StructType); ok {
					if areStructsCompatible(inferredStruct, expectedStruct) {
						// Use the expected type (preserves named type)
						resultType = expected
					}
				}
			} else if _, ok := expectedUnwrapped.(*types.ArrayType); ok {
				// For array literals without explicit type, adopt the expected array type
				// This allows [1, 2, 3] to adopt type [3]i32 when that's expected
				resultType = expected
				// Validate the composite literal against the expected array type
				checkCompositeLit(ctx, mod, compLit, expected)
			}
		}
	}

	// Handle optional type wrapping (T -> T?)
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
func checkAssignLike(ctx *context_v2.CompilerContext, mod *context_v2.Module, leftType types.SemType, leftNode ast.Node, rightNode ast.Expression) {
	rhsType := checkExpr(ctx, mod, rightNode, leftType)

	// Special check for integer literals: ensure they fit in the target type
	if ok := checkFitness(ctx, rhsType, leftType, rightNode, leftNode); !ok {
		return
	}

	// Check type compatibility
	compatibility := checkTypeCompatibility(rhsType, leftType)

	switch compatibility {
	case Identical, Assignable, LosslessConvertible:
		// OK - assignment is valid
		return

	case LossyConvertible:
		// Requires explicit cast
		diag := diagnostics.NewError(getConversionError(rhsType, leftType, compatibility))

		// Add dual labels if we have type node location
		if leftNode != nil {
			diag = diag.WithPrimaryLabel(rightNode.Loc(), fmt.Sprintf("type '%s'", rhsType.String())).
				WithSecondaryLabel(leftNode.Loc(), fmt.Sprintf("type '%s'", leftType.String()))
		} else {
			diag = diag.WithPrimaryLabel(rightNode.Loc(), "implicit conversion may lose precision")
		}

		ctx.Diagnostics.Add(
			diag.WithHelp(fmt.Sprintf("use an explicit cast: %s as %s", rightNode.Loc().GetText(), leftType.String())),
		)

	case Incompatible:
		// Cannot convert - create user-friendly error message
		var errorMsg string
		if types.IsUntyped(rhsType) {
			// For untyped literals, use more intuitive message
			if types.IsUntypedInt(rhsType) {
				errorMsg = fmt.Sprintf("cannot use integer literal as type '%s'", leftType.String())
			} else if types.IsUntypedFloat(rhsType) {
				errorMsg = fmt.Sprintf("cannot use float literal as type '%s'", leftType.String())
			} else {
				errorMsg = getConversionError(rhsType, leftType, compatibility)
			}
		} else {
			errorMsg = getConversionError(rhsType, leftType, compatibility)
		}

		diag := diagnostics.NewError(errorMsg)

		// Add dual labels if we have type node location
		if leftNode != nil {
			// Format value description (special handling for untyped literals)
			valueDesc := formatValueDescription(rhsType)
			diag = diag.WithPrimaryLabel(rightNode.Loc(), valueDesc).
				WithSecondaryLabel(leftNode.Loc(), fmt.Sprintf("type '%s'", leftType.String()))
		} else {
			diag = diag.WithPrimaryLabel(rightNode.Loc(), fmt.Sprintf("expected '%s', got '%s'", leftType.String(), rhsType.String()))
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
	// Check integer literal overflow
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

	// Check float literal precision
	if types.IsUntypedFloat(rhsType) {
		if lit, ok := valueExpr.(*ast.BasicLit); ok && lit.Kind == ast.FLOAT {
			if targetName, ok := types.GetPrimitiveName(targetType); ok && types.IsFloatTypeName(targetName) {
				if !fitsInType(lit.Value, targetType) {
					// Get minimum type that can hold this precision
					digits := countSignificantDigits(lit.Value)
					minType := getMinimumFloatTypeForDigits(digits)

					// Build error message
					diag := diagnostics.NewError(fmt.Sprintf("float literal has too many significant digits for %s", targetType.String()))

					if typeNode != nil {
						if minType != "exceeds f256 precision" {
							diag = diag.WithPrimaryLabel(valueExpr.Loc(), fmt.Sprintf("%d significant digits (needs %s)", digits, minType)).
								WithSecondaryLabel(typeNode.Loc(), fmt.Sprintf("type '%s' supports ~%d digits", targetType.String(), getFloatPrecision(targetName)))
						} else {
							diag = diag.WithPrimaryLabel(valueExpr.Loc(), fmt.Sprintf("%d significant digits", digits)).
								WithSecondaryLabel(typeNode.Loc(), fmt.Sprintf("type '%s'", targetType.String())).
								WithNote("This literal exceeds max f256 precision limit (~71 digits)")
						}
					} else {
						diag = diag.WithPrimaryLabel(valueExpr.Loc(), fmt.Sprintf("%d significant digits, need %s but got %s", digits, minType, targetType.String()))
					}

					ctx.Diagnostics.Add(diag)
					return false
				}
			}
		}
	}

	return true
}

// typeFromTypeNodeWithContext resolves type nodes including user-defined types by looking them up in the symbol table
func typeFromTypeNodeWithContext(ctx *context_v2.CompilerContext, mod *context_v2.Module, typeNode ast.TypeNode) types.SemType {
	if typeNode == nil {
		return types.TypeUnknown
	}

	switch t := typeNode.(type) {
	case *ast.ScopeResolutionExpr:
		// Handle module::Type references
		if moduleIdent, ok := t.X.(*ast.IdentifierExpr); ok {
			moduleAlias := moduleIdent.Name
			typeName := t.Selector.Name

			// Look up the import
			importPath, ok := mod.ImportAliasMap[moduleAlias]
			if !ok {
				// Module not imported - error should have been reported in resolver
				return types.TypeUnknown
			}

			// Get the imported module
			importedMod, exists := ctx.GetModule(importPath)
			if !exists {
				// Module not loaded - error should have been reported
				return types.TypeUnknown
			}

			// Look up the type in the imported module's scope
			sym, ok := importedMod.ModuleScope.GetSymbol(typeName)
			if !ok || sym.Kind != symbols.SymbolType {
				// Type not found - error should have been reported in resolver
				return types.TypeUnknown
			}

			// Return the resolved type
			return sym.Type
		}
		return types.TypeUnknown

	case *ast.IdentifierExpr:
		// Try to look up user-defined type in symbol table first
		sym, ok := mod.CurrentScope.Lookup(t.Name)
		if ok && sym.Kind == symbols.SymbolType {
			// Return the resolved user-defined type
			return sym.Type
		}

		// Not a user-defined type, check if it's a primitive type
		primitiveType := types.FromTypeName(types.TYPE_NAME(t.Name))
		if !primitiveType.Equals(types.TypeUnknown) {
			return primitiveType
		}

		// Type not found
		return types.TypeUnknown

	case *ast.ArrayType:
		// Array type: [N]T or []T
		elementType := typeFromTypeNodeWithContext(ctx, mod, t.ElType)
		length := -1
		if t.Len != nil {
			// Extract constant length from array size expression
			if constLength, ok := extractConstantIndex(t.Len); ok && constLength >= 0 {
				length = constLength
			}
		}
		return types.NewArray(elementType, length)

	case *ast.OptionalType:
		// Optional type: T?
		innerType := typeFromTypeNodeWithContext(ctx, mod, t.Base)
		return types.NewOptional(innerType)

	case *ast.ReferenceType:
		// Reference type: &T
		innerType := typeFromTypeNodeWithContext(ctx, mod, t.Base)
		return types.NewReference(innerType)

	case *ast.ResultType:
		// Result type: T ! E
		okType := typeFromTypeNodeWithContext(ctx, mod, t.Value)
		errType := typeFromTypeNodeWithContext(ctx, mod, t.Error)
		return types.NewResult(okType, errType)

	case *ast.StructType:
		// Anonymous struct
		fields := make([]types.StructField, len(t.Fields))
		for i, f := range t.Fields {
			fieldName := ""
			if f.Name != nil {
				fieldName = f.Name.Name
			}
			fields[i] = types.StructField{
				Name: fieldName,
				Type: typeFromTypeNodeWithContext(ctx, mod, f.Type),
			}
		}
		structType := types.NewStruct("", fields)
		// Propagate the ID from AST to semantic type for identity tracking
		structType.ID = t.ID
		return structType

	case *ast.FuncType:
		// Function type: fn(T1, T2) -> R
		params := make([]types.ParamType, len(t.Params))
		for i, param := range t.Params {
			params[i].Name = param.Name.Name
			params[i].Type = typeFromTypeNodeWithContext(ctx, mod, param.Type)
			params[i].IsVariadic = param.IsVariadic
		}
		returnType := typeFromTypeNodeWithContext(ctx, mod, t.Result)
		return types.NewFunction(params, returnType)

	case *ast.MapType:
		// Map type: map[K]V
		keyType := typeFromTypeNodeWithContext(ctx, mod, t.Key)
		valueType := typeFromTypeNodeWithContext(ctx, mod, t.Value)
		return types.NewMap(keyType, valueType)

	default:
		// Fallback to simple version
		return typeFromTypeNode(typeNode)
	}
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

	case *ast.MapType:
		// Map type: map[K]V
		keyType := typeFromTypeNode(t.Key)
		valueType := typeFromTypeNode(t.Value)
		return types.NewMap(keyType, valueType)

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
	// 0. If the function being called is a selector (method call), validate it first
	if selector, ok := expr.Fun.(*ast.SelectorExpr); ok {
		checkExpr(ctx, mod, selector.X, types.TypeUnknown)
		checkSelectorExpr(ctx, mod, selector)
	}

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

// applyNarrowingToBlock temporarily overrides symbol types based on narrowing context
// and checks the block. Uses defer to automatically restore original types.
func applyNarrowingToBlock(ctx *context_v2.CompilerContext, mod *context_v2.Module, block *ast.Block, narrowing *NarrowingContext) {
	if block == nil {
		return
	}

	if narrowing == nil {
		checkBlock(ctx, mod, block)
		return
	}

	// Collect all narrowed variables from this context and parent chain
	narrowedVars := make(map[string]types.SemType)
	collectNarrowedTypes(narrowing, narrowedVars)

	if len(narrowedVars) == 0 {
		checkBlock(ctx, mod, block)
		return
	}

	// Apply narrowing using defer for automatic restoration
	defer restoreSymbolTypes(mod, narrowedVars)()

	// Apply narrowed types
	for varName, narrowedType := range narrowedVars {
		if sym, ok := mod.CurrentScope.Lookup(varName); ok {
			sym.Type = narrowedType
		}
	}

	// Check the block with narrowed types
	checkBlock(ctx, mod, block)
}

// collectNarrowedTypes walks the narrowing context chain and collects all narrowed types
// Child contexts override parent contexts for the same variable
func collectNarrowedTypes(nc *NarrowingContext, result map[string]types.SemType) {
	if nc == nil {
		return
	}

	// First collect from parent (so child can override)
	if nc.parent != nil {
		collectNarrowedTypes(nc.parent, result)
	}

	// Then add/override with current level
	for varName, narrowedType := range nc.narrowedTypes {
		result[varName] = narrowedType
	}
}

// restoreSymbolTypes returns a function that restores original types
// This allows using defer for automatic cleanup
func restoreSymbolTypes(mod *context_v2.Module, narrowedVars map[string]types.SemType) func() {
	// Capture original types at the time of narrowing
	originalTypes := make(map[string]types.SemType)

	for varName := range narrowedVars {
		if sym, ok := mod.CurrentScope.Lookup(varName); ok {
			originalTypes[varName] = sym.Type
		}
	}

	// Return restoration function
	return func() {
		for varName, origType := range originalTypes {
			if sym, ok := mod.CurrentScope.Lookup(varName); ok {
				sym.Type = origType
			}
		}
	}
}

// applyNarrowingToElse handles else and else-if branches with narrowing
func applyNarrowingToElse(ctx *context_v2.CompilerContext, mod *context_v2.Module, elseNode ast.Node, elseNarrowing *NarrowingContext) {
	if elseNode == nil {
		return
	}

	// Check if it's an else-if (IfStmt) or plain else (Block)
	switch e := elseNode.(type) {
	case *ast.IfStmt:
		// For else-if, re-analyze the condition with parent narrowing from else branch
		elseIfThenNarrowing, elseIfElseNarrowing := analyzeConditionForNarrowing(ctx, mod, e.Cond, elseNarrowing)

		// Enter scope if exists
		if e.Scope != nil {
			defer mod.EnterScope(e.Scope.(*table.SymbolTable))()
		}

		// Check condition
		checkExpr(ctx, mod, e.Cond, types.TypeBool)

		// Check then branch with combined narrowing
		if e.Body != nil {
			applyNarrowingToBlock(ctx, mod, e.Body, elseIfThenNarrowing)
		}

		// Recursively handle nested else-if/else
		if e.Else != nil {
			applyNarrowingToElse(ctx, mod, e.Else, elseIfElseNarrowing)
		}

	case *ast.Block:
		// Plain else block - apply narrowing from condition
		applyNarrowingToBlock(ctx, mod, e, elseNarrowing)

	default:
		// Fallback for unexpected node types
		checkNode(ctx, mod, elseNode)
	}
}

// checkArrayBounds validates array indexing for compile-time bounds checking
// For fixed-size arrays with constant indices, reports errors for out-of-bounds access
// For dynamic arrays or runtime indices, skips validation (runtime check)
func checkArrayBounds(ctx *context_v2.CompilerContext, mod *context_v2.Module, indexExpr *ast.IndexExpr) {
	// Get the type of the array being indexed
	arrayType := inferExprType(ctx, mod, indexExpr.X)
	arrayType = types.UnwrapType(arrayType)

	// Only check fixed-size arrays
	arrType, ok := arrayType.(*types.ArrayType)
	if !ok || arrType.Length < 0 {
		// Not an array, or dynamic array - skip bounds checking
		// Dynamic arrays grow automatically, negative indices access from end
		return
	}

	// Try to evaluate the index expression as a compile-time constant
	// This handles not just literals like `arr[10]`, but also variables with known values like `let i := 10; arr[i]`
	indexValue, isConstant := consteval.EvaluateAsInt(ctx, mod, indexExpr.Index)
	if !isConstant {
		// Runtime value - cannot check at compile time
		return
	}

	// Store original index for better error message
	originalIndex := indexValue

	// Handle negative indices (access from end)
	if indexValue < 0 {
		indexValue = int64(arrType.Length) + indexValue
	}

	// Check bounds
	if indexValue < 0 || indexValue >= int64(arrType.Length) {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("array index out of bounds: index %d, array length %d", originalIndex, arrType.Length)).
				WithCode(diagnostics.ErrArrayOutOfBounds).
				WithPrimaryLabel(indexExpr.Index.Loc(), "index out of range").
				WithNote(fmt.Sprintf("valid indices are 0 to %d, or -%d to -1 for reverse access", arrType.Length-1, arrType.Length)).
				WithHelp("check the array length before accessing"),
		)
	}
}

// extractConstantIndex attempts to extract a compile-time constant integer from an expression
// Returns the integer value and whether it's a constant
func extractConstantIndex(expr ast.Expression) (int, bool) {
	switch e := expr.(type) {
	case *ast.BasicLit:
		// Direct integer literal
		if e.Kind == ast.INT {
			// Parse the integer value
			var val int64
			fmt.Sscanf(e.Value, "%d", &val)
			return int(val), true
		}

	case *ast.UnaryExpr:
		// Handle negative literals: -5
		if e.Op.Kind == tokens.MINUS_TOKEN {
			if val, ok := extractConstantIndex(e.X); ok {
				return -val, true
			}
		}

		// Could extend to handle constant folding of expressions like 2+3
		// For now, only handle direct literals
	}

	return 0, false
}
