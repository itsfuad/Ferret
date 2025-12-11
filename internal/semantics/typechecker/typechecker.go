package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/consteval"

	//"compiler/internal/semantics/controlflow"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/tokens"
	"compiler/internal/types"
	"compiler/internal/utils"
	str "compiler/internal/utils/strings"

	"fmt"
	"strconv"
	"strings"
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
	case *ast.BreakStmt:
		// Break statements don't need type checking
		// Validation (checking if inside loop) is done in control flow analysis phase
	case *ast.ContinueStmt:
		// Continue statements don't need type checking
		// Validation (checking if inside loop) is done in control flow analysis phase
	case *ast.ReturnStmt:
		// Check return value type against function's declared return type
		if n.Result != nil {
			expectedReturnType := mod.CurrentFunctionReturnType
			if expectedReturnType == nil {
				expectedReturnType = types.TypeUnknown
			}

			// Handle Result type returns: return value! (error) or return value (success)
			if resultType, isResult := expectedReturnType.(*types.ResultType); isResult {
				if n.IsError {
					// Returning error: return "error message"!
					returnedType := checkExpr(ctx, mod, n.Result, resultType.Err)

					// Resolve untyped literals
					if types.IsUntyped(returnedType) {
						if lit, ok := n.Result.(*ast.BasicLit); ok {
							returnedType = inferLiteralType(lit, resultType.Err)
						} else {
							returnedType = resolveType(returnedType, resultType.Err)
						}
					}

					if !returnedType.Equals(types.TypeUnknown) && !resultType.Err.Equals(types.TypeUnknown) {
						compatibility := checkTypeCompatibility(returnedType, resultType.Err)
						if compatibility == Incompatible {
							returnedDesc := resolveType(returnedType, resultType.Err)
							ctx.Diagnostics.Add(
								diagnostics.NewError("error return type mismatch").
									WithCode(diagnostics.ErrTypeMismatch).
									WithPrimaryLabel(n.Result.Loc(),
										fmt.Sprintf("expected error type %s, found %s", resultType.Err.String(), returnedDesc)),
							)
						}
					}
				} else {
					// Returning success: return value
					returnedType := checkExpr(ctx, mod, n.Result, resultType.Ok)

					// Resolve untyped literals
					if types.IsUntyped(returnedType) {
						if lit, ok := n.Result.(*ast.BasicLit); ok {
							returnedType = inferLiteralType(lit, resultType.Ok)
						} else {
							returnedType = resolveType(returnedType, resultType.Ok)
						}
					}

					if !returnedType.Equals(types.TypeUnknown) && !resultType.Ok.Equals(types.TypeUnknown) {
						compatibility := checkTypeCompatibility(returnedType, resultType.Ok)
						if compatibility == Incompatible {
							ctx.Diagnostics.Add(
								diagnostics.NewError("return type mismatch").
									WithCode(diagnostics.ErrTypeMismatch).
									WithPrimaryLabel(n.Result.Loc(),
										fmt.Sprintf("expected %s, found %s", resultType.Ok.String(), returnedType.String())),
							)
						}
					}
				}
			} else {
				// Non-Result type function
				if n.IsError {
					// Cannot use error return syntax in non-Result function
					ctx.Diagnostics.Add(
						diagnostics.InvalidErrorReturn(mod.FilePath, n.Loc(), expectedReturnType.String()),
					)
				} else {
					// Normal return type checking
					returnedType := checkExpr(ctx, mod, n.Result, expectedReturnType)
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
			}
		}
	case *ast.IfStmt:
		// Enter if scope if it exists
		if n.Scope != nil {
			defer mod.EnterScope(n.Scope.(*table.SymbolTable))()
		}

		// Check condition
		checkExpr(ctx, mod, n.Cond, types.TypeBool)

		// Dead code detection (constant conditions) is done in control flow analysis phase

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
						declType := TypeFromTypeNodeWithContext(ctx, mod, item.Type)
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

		// Dead code detection (constant conditions) is done in control flow analysis phase

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
			declType := TypeFromTypeNodeWithContext(ctx, mod, item.Type)

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
				if lit, ok := item.Value.(*ast.BasicLit); ok {
					rhsType = inferLiteralType(lit, types.TypeUnknown)
				}
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
		funcType := TypeFromTypeNodeWithContext(ctx, mod, decl.Type).(*types.FunctionType)
		if sym, ok := mod.CurrentScope.Lookup(decl.Name.Name); ok {
			sym.Type = funcType
		}
	}

	// Set up function context (scope and return type)
	defer setupFunctionContext(ctx, mod, funcScope, decl.Type)()

	// Add parameters to the function scope with type information
	if decl.Type != nil {
		addParamsToScope(ctx, mod, funcScope, decl.Type.Params)
	}

	// Check the body with the function scope
	if decl.Body != nil {
		checkBlock(ctx, mod, decl.Body)
		// Return path analysis is done in control flow analysis phase, not here
	}
}

// checkSelectorExpr validates that a field or method exists on a struct
// checkBinaryExpr validates that operands of a binary expression have compatible types
func checkBinaryExpr(ctx *context_v2.CompilerContext, _ *context_v2.Module, expr *ast.BinaryExpr, lhsType, rhsType types.SemType) {
	// Skip if either type is unknown (error already reported)
	if lhsType.Equals(types.TypeUnknown) || rhsType.Equals(types.TypeUnknown) {
		return
	}

	switch expr.Op.Kind {
	case tokens.PLUS_TOKEN:
		lhsString := lhsType.Equals(types.TypeString)
		rhsString := rhsType.Equals(types.TypeString)
		lhsNumericOrUntyped := types.IsNumericType(lhsType) || types.IsUntyped(lhsType)
		rhsNumericOrUntyped := types.IsNumericType(rhsType) || types.IsUntyped(rhsType)

		// If one side is string and the other is not, that's an error
		if (lhsString && !rhsString) || (!lhsString && rhsString) {
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("invalid operation: %s (mismatched types %s and %s)", expr.Op.Value, lhsType.String(), rhsType.String())).
					WithCode(diagnostics.ErrTypeMismatch).
					WithPrimaryLabel(expr.Loc(), fmt.Sprintf("cannot use '+' with %s and %s", lhsType.String(), rhsType.String())).
					WithHelp("'+' operator requires both operands to be numeric or both to be strings"),
			)
			return
		}

		if lhsString && rhsString {
			return
		}

		if lhsNumericOrUntyped && rhsNumericOrUntyped {
			return
		}

		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("invalid operation: %s (mismatched types %s and %s)", expr.Op.Value, lhsType.String(), rhsType.String())).
				WithCode(diagnostics.ErrTypeMismatch).
				WithPrimaryLabel(expr.Loc(), fmt.Sprintf("cannot use '+' with %s and %s", lhsType.String(), rhsType.String())).
				WithHelp("'+' operator requires both operands to be numeric or both to be strings"),
		)
		return

	case tokens.MINUS_TOKEN, tokens.MUL_TOKEN, tokens.DIV_TOKEN, tokens.MOD_TOKEN:
		// Allow untyped operands - they will be contextualized
		if types.IsUntyped(lhsType) || types.IsUntyped(rhsType) {
			return
		}

		// These operators only work with numeric types
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
		// Allow untyped operands for comparisons - they will be contextualized
		if types.IsUntyped(lhsType) || types.IsUntyped(rhsType) {
			return
		}

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
		// Allow untyped operands for ordering comparisons - they will be contextualized
		if types.IsUntyped(lhsType) || types.IsUntyped(rhsType) {
			return
		}

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
func checkCastExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CastExpr, sourceType, targetType types.SemType) {
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

	// Check interface casts: allow casting to interface if type implements it
	// Handle both direct InterfaceType and NamedType wrapping an InterfaceType
	var targetIface *types.InterfaceType
	if iface, ok := targetType.(*types.InterfaceType); ok {
		targetIface = iface
	} else if named, ok := targetType.(*types.NamedType); ok {
		if iface, ok := named.Underlying.(*types.InterfaceType); ok {
			targetIface = iface
		}
	}
	if targetIface != nil {
		if implementsInterface(ctx, mod, sourceType, targetIface) {
			return // Valid interface cast
		}
		// Build error message with missing methods
		var missingMethods []string
		var typeName string
		if named, ok := sourceType.(*types.NamedType); ok {
			typeName = named.Name
		}
		typeSym, _ := lookupTypeSymbol(ctx, mod, typeName)
		for _, requiredMethod := range targetIface.Methods {
			if typeSym == nil || typeSym.Methods == nil {
				missingMethods = append(missingMethods, requiredMethod.Name)
				continue
			}
			methodInfo, hasMethod := typeSym.Methods[requiredMethod.Name]
			if !hasMethod {
				missingMethods = append(missingMethods, requiredMethod.Name)
			} else if !methodSignaturesMatch(methodInfo.FuncType, requiredMethod.FuncType) {
				if ctx.Debug {
					fmt.Printf("      [Cast check: method %s signature mismatch]\n", requiredMethod.Name)
					fmt.Printf("        Method: %s\n", methodInfo.FuncType.String())
					fmt.Printf("        Interface: %s\n", requiredMethod.FuncType.String())
				}
				missingMethods = append(missingMethods, fmt.Sprintf("%s (signature mismatch)", requiredMethod.Name))
			}
		}
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("cannot cast %s to %s", sourceType.String(), targetType.String())).
				WithCode(diagnostics.ErrInvalidCast).
				WithPrimaryLabel(expr.Loc(), "type does not implement interface").
				WithHelp(fmt.Sprintf("missing methods: %s", strings.Join(missingMethods, ", "))),
		)
		return
	}

	// Check if cast is valid
	compatibility := checkTypeCompatibilityWithContext(ctx, mod, sourceType, targetType)

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
// It handles both explicit types (from lit.Type) and inferred types (when targetType is provided)
// This function checks elements with context AND validates consistency
// Returns missing fields for struct literals, nil otherwise
func checkCompositeLit(ctx *context_v2.CompilerContext, mod *context_v2.Module, lit *ast.CompositeLit, targetType types.SemType) []string {
	// Unwrap NamedType to get the underlying type
	underlyingType := types.UnwrapType(targetType)

	// Handle arrays
	if arrayType, ok := underlyingType.(*types.ArrayType); ok {
		validateArrayLiteral(ctx, mod, lit, arrayType)
		return nil
	}

	// Handle struct literals
	if structType, ok := underlyingType.(*types.StructType); ok {
		return checkStructLiteral(ctx, mod, lit, structType)
	}

	// Handle map literals
	if mapType, ok := underlyingType.(*types.MapType); ok {
		// Only validate as map if not all keys are identifiers (structs have all IdentifierExpr keys)
		allKeysAreIdentifiers := true
		hasKeyValueExpr := false
		for _, elem := range lit.Elts {
			if kv, ok := elem.(*ast.KeyValueExpr); ok {
				hasKeyValueExpr = true
				if _, isIdent := kv.Key.(*ast.IdentifierExpr); !isIdent {
					allKeysAreIdentifiers = false
					break
				}
			}
		}
		if hasKeyValueExpr && !allKeysAreIdentifiers {
			checkMapLiteral(ctx, mod, lit, mapType)
		}
		return nil
	}

	return nil
}

// checkStructLiteral validates struct literal elements
// It checks elements with field type context AND validates consistency
// Returns missing fields if any, nil otherwise
func checkStructLiteral(ctx *context_v2.CompilerContext, mod *context_v2.Module, lit *ast.CompositeLit, structType *types.StructType) []string {
	// Build a map of provided fields and check elements with context
	providedFields := make(map[string]bool)
	for _, elem := range lit.Elts {
		if kv, ok := elem.(*ast.KeyValueExpr); ok {
			if key, ok := kv.Key.(*ast.IdentifierExpr); ok {
				fieldName := key.Name
				providedFields[fieldName] = true

				// Check value with field type as context
				if kv.Value != nil {
					fieldExpected := types.TypeUnknown
					for _, field := range structType.Fields {
						if field.Name == fieldName {
							fieldExpected = field.Type
							break
						}
					}
					checkExpr(ctx, mod, kv.Value, fieldExpected)
				}
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

	// Return missing fields - error will be reported in checkAssignLike with better context
	return missingFields
}

// validateArrayLiteral validates that all array elements match the element type
func validateArrayLiteral(ctx *context_v2.CompilerContext, mod *context_v2.Module, lit *ast.CompositeLit, arrayType *types.ArrayType) {
	for _, elem := range lit.Elts {
		if _, isKV := elem.(*ast.KeyValueExpr); !isKV {
			elemType := checkExpr(ctx, mod, elem, arrayType.Element)
			if compat := checkTypeCompatibility(elemType, arrayType.Element); compat == Incompatible {
				elemTypeStr := resolveType(elemType, types.TypeUnknown)
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("array elements must all be same type, expected %s but found %s", arrayType.Element.String(), elemTypeStr)).
						WithCode(diagnostics.ErrTypeMismatch).
						WithPrimaryLabel(elem.Loc(), fmt.Sprintf("type %s", elemTypeStr)).
						WithHelp(fmt.Sprintf("all array elements must be %s", arrayType.Element.String())),
				)
			}
		}
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

		// Check key type compatibility - with contextualization
		keyType := checkExpr(ctx, mod, kv.Key, mapType.Key)
		if compat := checkTypeCompatibility(keyType, mapType.Key); compat == Incompatible {
			keyTypeStr := resolveType(keyType, types.TypeUnknown)
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("map keys must all be same type, expected %s but found %s", mapType.Key.String(), keyTypeStr)).
					WithCode(diagnostics.ErrTypeMismatch).
					WithPrimaryLabel(kv.Key.Loc(), fmt.Sprintf("type %s", keyTypeStr)).
					WithHelp(fmt.Sprintf("all map keys must be %s", mapType.Key.String())),
			)
		}

		// Check value type compatibility - with contextualization
		valueType := checkExpr(ctx, mod, kv.Value, mapType.Value)
		if compat := checkTypeCompatibility(valueType, mapType.Value); compat == Incompatible {
			valueTypeStr := resolveType(valueType, types.TypeUnknown)
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("map values must all be same type, expected %s but found %s", mapType.Value.String(), valueTypeStr)).
					WithCode(diagnostics.ErrTypeMismatch).
					WithPrimaryLabel(kv.Value.Loc(), fmt.Sprintf("type %s", valueTypeStr)).
					WithHelp(fmt.Sprintf("all map values must be %s", mapType.Value.String())),
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

	fieldName := expr.Field.Name

	// Check if baseType is an interface type (or NamedType wrapping an interface)
	var interfaceType *types.InterfaceType
	if iface, ok := baseType.(*types.InterfaceType); ok {
		interfaceType = iface
	} else if named, ok := baseType.(*types.NamedType); ok {
		if iface, ok := named.Underlying.(*types.InterfaceType); ok {
			interfaceType = iface
		}
	}

	// If it's an interface, check if the method exists in the interface
	if interfaceType != nil {
		for _, method := range interfaceType.Methods {
			if method.Name == fieldName {
				return // Method exists in interface
			}
		}
		// Method not found in interface
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("method '%s' not found in interface '%s'", fieldName, baseType.String())).
				WithCode(diagnostics.ErrFieldNotFound).
				WithPrimaryLabel(expr.Field.Loc(), fmt.Sprintf("'%s' does not exist in interface", fieldName)),
		)
		return
	}

	// Handle NamedType and anonymous StructType
	var typeSym *symbols.Symbol
	var structType *types.StructType
	var typeName string

	if namedType, ok := baseType.(*types.NamedType); ok {
		// It's a named type - look up the type symbol for method resolution
		typeName = namedType.Name
		if sym, found := lookupTypeSymbol(ctx, mod, typeName); found {
			typeSym = sym
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
				// Check field visibility - private fields (lowercase) have restrictions
				if !utils.IsExported(fieldName) {
					// Private field - only accessible through receiver parameter
					// Simple check: if expr.X is an identifier that refers to a receiver symbol
					isReceiver := false
					if ident, ok := expr.X.(*ast.IdentifierExpr); ok {
						// Look up the identifier - search scope chain to find receiver
						// Walk up the parent chain starting from current scope
						currentScope := mod.CurrentScope
						for currentScope != nil {
							if sym, found := currentScope.GetSymbol(ident.Name); found {
								// Found in this scope - check if it's a receiver
								if sym.Kind == symbols.SymbolReceiver {
									isReceiver = true
									break
								}
								// Found but not a receiver - this shadows any receiver in parent scopes
								// So we stop searching
								break
							}
							// Not found in this scope, check parent
							currentScope = currentScope.Parent()
						}
					}

					if !isReceiver {
						ctx.Diagnostics.Add(
							diagnostics.NewError(fmt.Sprintf("field '%s' is private", fieldName)).
								WithPrimaryLabel(expr.Field.Loc(), fmt.Sprintf("cannot access private field '%s'", fieldName)).
								WithNote("private fields (lowercase) can only be accessed through the receiver in methods"),
						)
						return
					}
				}
				return // Field exists and is accessible
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
				WithPrimaryLabel(expr.Field.Loc(), fmt.Sprintf("'%s' does not exist", fieldName)),
		)
	} else {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("field '%s' not found on anonymous struct", fieldName)).
				WithCode(diagnostics.ErrFieldNotFound).
				WithPrimaryLabel(expr.Field.Loc(), fmt.Sprintf("'%s' does not exist", fieldName)),
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

	receiverType := TypeFromTypeNodeWithContext(ctx, mod, decl.Receiver.Type)

	// Only process valid named types
	if receiverType.Equals(types.TypeUnknown) {
		return
	}

	// Unwrap reference types: &T -> T
	receiverType = dereferenceType(receiverType)

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
	typeSym, found := lookupTypeSymbol(ctx, mod, typeName)

	// Type check the method signature and attach to type symbol
	if found && typeSym.Kind == symbols.SymbolType && decl.Type != nil {
		funcType := TypeFromTypeNodeWithContext(ctx, mod, decl.Type).(*types.FunctionType)

		// Attach method to the type symbol's Methods map
		if typeSym.Methods == nil {
			typeSym.Methods = make(map[string]*symbols.MethodInfo)
		}

		// Create method info and attach to type symbol
		typeSym.Methods[decl.Name.Name] = &symbols.MethodInfo{
			Name:     decl.Name.Name,
			FuncType: funcType,
			Exported: utils.IsExported(decl.Name.Name),
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

	// Set up method context (scope and return type)
	defer setupFunctionContext(ctx, mod, methodScope, decl.Type)()

	// Add receiver to method scope
	if decl.Receiver != nil && decl.Receiver.Name != nil {
		receiverSym, ok := methodScope.GetSymbol(decl.Receiver.Name.Name)
		if ok && decl.Receiver.Type != nil {
			receiverType := TypeFromTypeNodeWithContext(ctx, mod, decl.Receiver.Type)
			receiverSym.Type = receiverType
		}
	}

	// Add parameters to the method scope with type information
	if decl.Type != nil {
		addParamsToScope(ctx, mod, methodScope, decl.Type.Params)
	}

	// Check the body with the method scope
	if decl.Body != nil {
		checkBlock(ctx, mod, decl.Body)
		// Return path analysis is done in control flow analysis phase, not here
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
	semType := TypeFromTypeNodeWithContext(ctx, mod, decl.Type)

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
	// Some blocks (like function body) use their parent scope
	if block.Scope != nil {
		scope := block.Scope.(*table.SymbolTable)
		defer mod.EnterScope(scope)()
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
		// Validate catch clause if present
		if e.Catch != nil {
			checkCatchClause(ctx, mod, e)
		}

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
		targetType := TypeFromTypeNodeWithContext(ctx, mod, e.Type)
		// For composite literals, provide target type as context to allow untyped literal contextualization
		sourceType := checkExpr(ctx, mod, e.X, targetType)
		checkCastExpr(ctx, mod, e, sourceType, targetType)

	case *ast.CompositeLit:
		// Determine target type for validation
		var targetType types.SemType
		if e.Type != nil {
			// Explicit type: use it as target
			targetType = TypeFromTypeNodeWithContext(ctx, mod, e.Type)
		} else if !expected.Equals(types.TypeUnknown) {
			// Expected type provided: use it as target
			targetType = expected
		} else {
			// No explicit type and no expected type: infer type
			targetType = inferExprType(ctx, mod, e)
		}

		// checkCompositeLit handles everything: element checking with context AND validation
		// Missing fields info will be handled in checkAssignLike for better error messages
		if !targetType.Equals(types.TypeUnknown) {
			checkCompositeLit(ctx, mod, e, targetType)
		}
	}

	// First, infer the type based on the expression structure
	inferredType := inferExprType(ctx, mod, expr)

	// Check if literal is too large for any supported type
	if lit, ok := expr.(*ast.BasicLit); ok {
		if inferredType.Equals(types.TypeUnknown) {
			// Literal doesn't fit in maximum type - report error
			switch lit.Kind {
			case ast.INT:
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("integer literal %s exceeds maximum supported integer size (256-bit)", lit.Value)).
						WithPrimaryLabel(lit.Loc(), "too large value").
						WithNote("maximum supported integer type is i256 (256-bit signed integer)").
						WithHelp("consider using a string representation or splitting the value"),
				)
			case ast.FLOAT:
				digits := countSignificantDigits(lit.Value)
				ctx.Diagnostics.Add(
					diagnostics.NewError(fmt.Sprintf("float literal has %d significant digits, exceeds maximum supported precision (f256, ~71 digits)", digits)).
						WithPrimaryLabel(lit.Loc(), "too large value").
						WithNote("maximum supported float type is f256 with ~71 significant digits").
						WithHelp("consider reducing precision or using a different representation"),
				)
			}
			return types.TypeUnknown
		}
	}

	// Apply contextual typing for literals when expected type is provided
	// Note: Struct literals, string literals, bool literals have concrete types immediately
	resultType := inferredType

	// If we have an expected type and the expression is a literal, contextualize it directly
	// This ensures literals are contextualized to the expected type when provided
	if !expected.Equals(types.TypeUnknown) {
		if lit, ok := expr.(*ast.BasicLit); ok {
			// If expected is optional, contextualize to inner type
			expectedForLit := unwrapOptionalType(expected)
			// For numeric literals, always try to contextualize to expected type
			// inferLiteralType will return expected type if compatible, or default if not
			if lit.Kind == ast.INT || lit.Kind == ast.FLOAT {
				resultType = inferLiteralType(lit, expectedForLit)
			}
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

	// Keep untyped literals untyped for better error messages
	// They will be resolved in specific contexts that need concrete types

	return resultType
}

// checkAssignLike checks assignment-like operations with better error reporting
// typeNode: optional AST node for the target type (for location info)
// valueExpr: the value expression being assigned
// targetType: the expected/target type
func checkAssignLike(ctx *context_v2.CompilerContext, mod *context_v2.Module, leftType types.SemType, leftNode ast.Node, rightNode ast.Expression) {
	rhsType := checkExpr(ctx, mod, rightNode, leftType)

	// Special check for integer literals: ensure they fit in the target type
	if ok := checkFitness(ctx, leftType, rightNode, leftNode); !ok {
		return
	}

	// Check type compatibility (use context-aware version for interface checking)
	compatibility := checkTypeCompatibilityWithContext(ctx, mod, rhsType, leftType)

	switch compatibility {
	case Identical, Assignable:
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

		exprText := rightNode.Loc().GetText(ctx.Diagnostics.GetSourceCache())
		if exprText == "" {
			exprText = "expression"
		}
		ctx.Diagnostics.Add(
			diag.WithHelp(fmt.Sprintf("use an explicit cast: %s as %s", exprText, leftType.String())),
		)

	case Incompatible:
		// Cannot convert - create user-friendly error message
		// Check if this is a struct compatibility issue that we can enhance
		rhsUnwrapped := types.UnwrapType(rhsType)
		leftUnwrapped := types.UnwrapType(leftType)

		var missingFields []string
		var mismatchedFields []string
		isStructCompatibility := false

		if rhsStruct, ok := rhsUnwrapped.(*types.StructType); ok {
			if leftStruct, ok := leftUnwrapped.(*types.StructType); ok {
				// Both are structs - get detailed compatibility info
				missingFields, mismatchedFields = analyzeStructCompatibility(rhsStruct, leftStruct)
				isStructCompatibility = true
			}
		}

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
			valueDesc := formatValueDescription(rhsType, rightNode)
			diag = diag.WithPrimaryLabel(rightNode.Loc(), valueDesc).
				WithSecondaryLabel(leftNode.Loc(), fmt.Sprintf("type '%s'", leftType.String()))
		} else {
			diag = diag.WithPrimaryLabel(rightNode.Loc(), fmt.Sprintf("expected '%s', got '%s'", leftType.String(), rhsType.String()))
		}

		// Add helpful note for struct compatibility issues
		if isStructCompatibility && (len(missingFields) > 0 || len(mismatchedFields) > 0) {
			var noteParts []string
			if len(missingFields) > 0 {
				noteParts = append(noteParts, fmt.Sprintf("missing %s (%s)", str.Pluralize("field", "fields", len(missingFields)), strings.Join(missingFields, ", ")))
			}
			if len(mismatchedFields) > 0 {
				noteParts = append(noteParts, fmt.Sprintf("type mismatch in %s: %s", str.Pluralize("field", "fields", len(mismatchedFields)), strings.Join(mismatchedFields, ", ")))
			}
			if len(noteParts) > 0 {
				diag = diag.WithNote(strings.Join(noteParts, "; "))
			}
		}

		ctx.Diagnostics.Add(diag)
	}
}

// formatValueDescription formats a user-friendly description for a value in error messages.
// For untyped literals, it shows the minimum type needed (e.g., "integer literal (needs i32)").
func formatValueDescription(typ types.SemType, expr ast.Expression) string {
	// Handle untyped literals specially - show what type they would resolve to
	if types.IsUntyped(typ) {
		if basicLit, ok := expr.(*ast.BasicLit); ok {
			// Resolve to see what type it needs
			resolvedType := inferLiteralType(basicLit, types.TypeUnknown)
			if types.IsUntypedInt(typ) {
				return fmt.Sprintf("integer literal (needs %s)", resolvedType.String())
			} else if types.IsUntypedFloat(typ) {
				return fmt.Sprintf("float literal (needs %s)", resolvedType.String())
			}
		}
		// Fallback for non-literal untyped expressions
		if types.IsUntypedInt(typ) {
			return "integer value"
		} else if types.IsUntypedFloat(typ) {
			return "float value"
		}
	}
	return fmt.Sprintf("type '%s'", typ.String())
}

func checkFitness(ctx *context_v2.CompilerContext, targetType types.SemType, valueExpr ast.Expression, typeNode ast.Node) bool {
	// Check integer literal overflow
	// Check literal directly (works for both untyped and already-resolved literals)
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

	// Check float literal precision
	// Check literal directly (works for both untyped and already-resolved literals)
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

	return true
}

// TypeFromTypeNodeWithContext resolves type nodes including user-defined types by looking them up in the symbol table
// TypeFromTypeNodeWithContext converts AST type nodes to semantic types with full context.
// This allows resolving user-defined types, module-qualified types, etc.
// Exported for use by codegen and other packages that need type resolution.
func TypeFromTypeNodeWithContext(ctx *context_v2.CompilerContext, mod *context_v2.Module, typeNode ast.TypeNode) types.SemType {
	if typeNode == nil {
		return types.TypeUnknown
	}

	switch t := typeNode.(type) {
	case *ast.ScopeResolutionExpr:
		// Handle module::Type references (requires context)
		if ctx == nil || mod == nil {
			return types.TypeUnknown
		}
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
		// If we have context, try to look up user-defined type in symbol table first
		if ctx != nil && mod != nil {
			sym, ok := mod.CurrentScope.Lookup(t.Name)
			if ok && sym.Kind == symbols.SymbolType {
				// Return the resolved user-defined type
				return sym.Type
			}
		}

		// Not a user-defined type (or no context), check if it's a primitive type
		primitiveType := types.FromTypeName(types.TYPE_NAME(t.Name))
		if !primitiveType.Equals(types.TypeUnknown) {
			return primitiveType
		}

		// Type not found
		return types.TypeUnknown

	case *ast.ArrayType:
		// Array type: [N]T or []T
		elementType := TypeFromTypeNodeWithContext(ctx, mod, t.ElType)
		length := -1 // Dynamic array by default
		if t.Len != nil {
			// Extract constant length from array size expression (only if we have context for const eval)
			if ctx != nil && mod != nil {
				if constLength, ok := extractConstantIndex(t.Len); ok && constLength >= 0 {
					length = constLength
				}
			}
		}
		return types.NewArray(elementType, length)

	case *ast.OptionalType:
		// Optional type: T?
		innerType := TypeFromTypeNodeWithContext(ctx, mod, t.Base)
		return types.NewOptional(innerType)

	case *ast.ReferenceType:
		// Reference type: &T
		innerType := TypeFromTypeNodeWithContext(ctx, mod, t.Base)
		return types.NewReference(innerType)

	case *ast.ResultType:
		// Result type: T ! E
		okType := TypeFromTypeNodeWithContext(ctx, mod, t.Value)
		errType := TypeFromTypeNodeWithContext(ctx, mod, t.Error)
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
				Type: TypeFromTypeNodeWithContext(ctx, mod, f.Type),
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
			params[i].Type = TypeFromTypeNodeWithContext(ctx, mod, param.Type)
			params[i].IsVariadic = param.IsVariadic
		}
		returnType := types.TypeVoid
		if t.Result != nil {
			returnType = TypeFromTypeNodeWithContext(ctx, mod, t.Result)
		}
		return types.NewFunction(params, returnType)

	case *ast.MapType:
		// Map type: map[K]V
		keyType := TypeFromTypeNodeWithContext(ctx, mod, t.Key)
		valueType := TypeFromTypeNodeWithContext(ctx, mod, t.Value)
		return types.NewMap(keyType, valueType)

	case *ast.InterfaceType:
		// Interface type: interface { method1(...), method2(...) }
		methods := make([]types.InterfaceMethod, 0, len(t.Methods))
		for _, m := range t.Methods {
			if m.Type == nil {
				continue
			}
			// m.Type should be a FuncType
			if funcType, ok := m.Type.(*ast.FuncType); ok {
				params := make([]types.ParamType, len(funcType.Params))
				for j, param := range funcType.Params {
					params[j].Name = param.Name.Name
					params[j].Type = TypeFromTypeNodeWithContext(ctx, mod, param.Type)
					params[j].IsVariadic = param.IsVariadic
				}
				returnType := types.TypeVoid
				if funcType.Result != nil {
					returnType = TypeFromTypeNodeWithContext(ctx, mod, funcType.Result)
				}
				methods = append(methods, types.InterfaceMethod{
					Name:     m.Name.Name,
					FuncType: types.NewFunction(params, returnType),
				})
			}
		}
		interfaceType := types.NewInterface(methods)
		// Propagate the ID from AST to semantic type for identity tracking
		interfaceType.ID = t.ID
		return interfaceType

	default:
		// For unknown types, try primitive lookup
		if ident, ok := typeNode.(*ast.IdentifierExpr); ok {
			return types.FromTypeName(types.TYPE_NAME(ident.Name))
		}
		return types.TypeUnknown
	}
}

// typeFromTypeNode converts AST type nodes to semantic types (without context)
// This is a convenience wrapper that calls TypeFromTypeNodeWithContext with nil context.
// For user-defined types, this will only work for primitives.
func typeFromTypeNode(typeNode ast.TypeNode) types.SemType {
	// Use context version with nil - it will handle primitives correctly
	// but won't resolve user-defined types (which is fine for this use case)
	return TypeFromTypeNodeWithContext(nil, nil, typeNode)
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
			argTypeDesc := resolveType(argType, param.Type)
			ctx.Diagnostics.Add(
				diagnostics.ArgumentTypeMismatch(
					mod.FilePath,
					arg.Loc(),
					param.Name,
					param.Type.String(),
					argTypeDesc.String(),
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
				argTypeDesc := resolveType(argType, variadicElemType)
				ctx.Diagnostics.Add(
					diagnostics.ArgumentTypeMismatch(
						mod.FilePath,
						arg.Loc(),
						variadicParam.Name,
						variadicElemType.String(),
						argTypeDesc.String(),
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

	// Validate Result type handling: check if function returns Result and needs catch
	validateResultTypeHandling(ctx, mod, expr, funcType)
}

// validateResultTypeHandling checks if Result types are properly handled with catch clauses
func validateResultTypeHandling(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CallExpr, funcType *types.FunctionType) {
	returnType := funcType.Return
	_, isResult := returnType.(*types.ResultType)

	if isResult {
		// Function returns a Result type
		if expr.Catch == nil {
			// No catch clause - error must be handled
			ctx.Diagnostics.Add(
				diagnostics.UncaughtError(ctx.Diagnostics.GetSourceCache(), mod.FilePath, expr.Loc(), returnType.String()),
			)
		}
	} else {
		// Function does not return a Result type
		if expr.Catch != nil {
			// Cannot use catch on non-Result function
			ctx.Diagnostics.Add(
				diagnostics.InvalidCatch(mod.FilePath, expr.Catch.Loc(), returnType.String()),
			)
		}
	}
}

// checkCatchClause validates catch clause semantics
func checkCatchClause(ctx *context_v2.CompilerContext, mod *context_v2.Module, callExpr *ast.CallExpr) {
	catch := callExpr.Catch
	if catch == nil {
		return
	}

	// Get the function's return type to determine error type
	funType := inferExprType(ctx, mod, callExpr.Fun)
	funcType, ok := funType.(*types.FunctionType)
	if !ok {
		return // Error already reported in checkCallExpr
	}

	resultType, isResult := funcType.Return.(*types.ResultType)
	if !isResult {
		return // Error already reported in validateResultTypeHandling
	}

	// get the scope of the catch block
	scope := catch.Handler.Scope.(*table.SymbolTable)
	defer mod.EnterScope(scope)()
	// check catch block
	// set the catch error identifier type to the error type
	if catch.ErrIdent != nil {
		if sym, ok := mod.CurrentScope.Lookup(catch.ErrIdent.Name); ok {
			sym.Type = resultType.Err
		}
	}
	// check the catch block
	checkBlock(ctx, mod, catch.Handler)
	// check the fallback expression - must match the OK type of the result
	if catch.Fallback != nil {
		// Use checkAssignLike to validate type compatibility and report errors
		checkAssignLike(ctx, mod, resultType.Ok, nil, catch.Fallback)
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
