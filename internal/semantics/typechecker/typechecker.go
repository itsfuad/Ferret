package typechecker

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/controlflow"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/tokens"
	"compiler/internal/types"
	str "compiler/internal/utils/strings"

	"fmt"
	"strings"
	"sync"
)

// CheckModule performs type checking on a module.
// It fills Symbol.Type for declarations and creates ExprTypes map for expressions.
func CheckModule(ctx *context_v2.CompilerContext, mod *context_v2.Module) {

	// Reset CurrentScope to ModuleScope before type checking
	// This ensures we're starting from the module-level scope
	mod.CurrentScope = mod.ModuleScope

	// Check all declarations in the module
	// IMPORTANT: Check type declarations FIRST so user-defined types are available
	// for use in variable declarations, function signatures, etc.
	if mod.AST != nil {
		// Pass 1: Check all type declarations first
		for _, node := range mod.AST.Nodes {
			if typeDecl, ok := node.(*ast.TypeDecl); ok {
				checkTypeDecl(ctx, mod, typeDecl)
			}
		}

		// Pass 2: Check everything else
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
			declType := typeFromTypeNodeWithContext(ctx, mod, item.Type)
			sym.Type = declType

			// Check initializer if present
			if item.Value != nil {
				// Pass item.Type for type location in error messages
				checkAssignLike(ctx, mod, declType, item.Type, item.Value)
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
func checkBinaryExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.BinaryExpr, lhsType, rhsType types.SemType) {
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
func checkCastExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.CastExpr, sourceType, targetType types.SemType) {
	// Skip if target type is unknown
	if targetType.Equals(types.TypeUnknown) {
		return
	}

	// Special case: casting composite literal to struct type
	// Validate that the composite literal matches the struct type
	if compLit, ok := expr.X.(*ast.CompositeLit); ok {
		if structType, ok := targetType.(*types.StructType); ok {
			checkCompositeLit(ctx, mod, compLit, structType)
			return // Composite literal validation is sufficient
		}
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

	// For struct types, check structural compatibility
	if srcStruct, ok := sourceType.(*types.StructType); ok {
		if dstStruct, ok := targetType.(*types.StructType); ok {
			// Check if struct types are compatible by structure
			if !areStructsCompatible(srcStruct, dstStruct) {
				ctx.Diagnostics.Add(
					diagnostics.NewError("cannot cast incompatible struct types").
						WithCode(diagnostics.ErrTypeMismatch).
						WithPrimaryLabel(expr.Loc(), fmt.Sprintf("cannot cast from %s to %s: field mismatch", sourceType.String(), targetType.String())).
						WithHelp("ensure the source has all required fields with matching types"),
				)
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
	// Only validate struct literals
	structType, ok := targetType.(*types.StructType)
	if !ok {
		return
	}

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
		errorMsg := fmt.Sprintf("cannot convert composite literal to type '%s'", structType.Name)
		diag := diagnostics.NewError(errorMsg).
			WithCode(diagnostics.ErrTypeMismatch).
			WithPrimaryLabel(lit.Loc(), "incompatible struct literal")

		// Build note with missing fields list
		noteMsg := fmt.Sprintf("missing %s (%s)", str.Pluralize("field", "fields", len(missingFields)), strings.Join(missingFields, ", "))
		diag = diag.WithNote(noteMsg)

		ctx.Diagnostics.Add(diag)
	}
}

// areStructsCompatible checks if two struct types are structurally compatible
func areStructsCompatible(src, dst *types.StructType) bool {
	// Check if destination has all required fields with matching types
	for _, dstField := range dst.Fields {
		found := false
		for _, srcField := range src.Fields {
			if srcField.Name == dstField.Name {
				found = true
				if !srcField.Type.Equals(dstField.Type) {
					return false // Type mismatch
				}
				break
			}
		}
		if !found {
			return false // Missing required field
		}
	}
	return true
}

func checkSelectorExpr(ctx *context_v2.CompilerContext, mod *context_v2.Module, expr *ast.SelectorExpr) {
	// Infer the base type
	baseType := inferExprType(ctx, mod, expr.X)

	// Skip validation if base type is unknown (error already reported)
	if baseType.Equals(types.TypeUnknown) {
		return
	}

	// Only validate struct field/method access
	structType, ok := baseType.(*types.StructType)
	if !ok {
		// Not a struct - could be module access or other, handled elsewhere
		return
	}

	fieldName := expr.Sel.Name

	// Check if it's a field
	for _, field := range structType.Fields {
		if field.Name == fieldName {
			return // Field exists
		}
	}

	// Check if it's a method (look in type's scope)
	if structType.Name != "" {
		// Look up the type symbol to get its scope
		if typeSym, ok := mod.ModuleScope.Lookup(structType.Name); ok && typeSym.Scope != nil {
			if _, ok := typeSym.Scope.GetSymbol(fieldName); ok {
				return // Method exists
			}
		}
	}

	// Field/method not found
	if structType.Name != "" {
		ctx.Diagnostics.Add(
			diagnostics.NewError(fmt.Sprintf("field or method '%s' not found on type '%s'", fieldName, structType.Name)).
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

// checkMethodDecl type checks a method declaration with receiver
func checkMethodDecl(ctx *context_v2.CompilerContext, mod *context_v2.Module, decl *ast.MethodDecl) {
	// Safety check: Scope should be set during collection phase
	if decl.Scope == nil {
		ctx.ReportError(fmt.Sprintf("internal error: method '%s' has nil scope", decl.Name.Name), decl.Loc())
		return
	}

	methodScope := decl.Scope.(*table.SymbolTable)

	// Get receiver type
	if decl.Receiver == nil || decl.Receiver.Type == nil {
		// Error already reported in collector
	} else {
		receiverType := typeFromTypeNodeWithContext(ctx, mod, decl.Receiver.Type)

		// Validate receiver type is a named struct (not anonymous or primitive)
		structType, ok := receiverType.(*types.StructType)
		if !ok {
			ctx.Diagnostics.Add(
				diagnostics.NewError("method receiver must be a struct type").
					WithCode(diagnostics.ErrInvalidMethodReceiver).
					WithPrimaryLabel(decl.Receiver.Loc(), fmt.Sprintf("type %s is not a struct", receiverType.String())),
			)
		} else if structType.Name != "" {
			// Look up the receiver type symbol to get its method scope
			if typeSym, ok := mod.ModuleScope.Lookup(structType.Name); ok && typeSym.Scope != nil {
				typeScope := typeSym.Scope.(*table.SymbolTable)
				// Update the method symbol with actual function signature
				if methodSym, ok := typeScope.GetSymbol(decl.Name.Name); ok && decl.Type != nil {
					funcType := typeFromTypeNodeWithContext(ctx, mod, decl.Type).(*types.FunctionType)
					methodSym.Type = funcType
				}
			}
		}
	}

	// Enter method scope for the entire method processing (even for invalid methods)
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

	// Add receiver to method scope as a parameter (even for invalid methods, to allow body analysis)
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

	// For struct types, set the name and ensure ID is set
	if structType, ok := semType.(*types.StructType); ok {
		structType.Name = typeName
		// If the struct doesn't have an ID yet (shouldn't happen, but be safe), use the type name
		if structType.ID == "" {
			structType.ID = typeName
		}
	}

	// Update the symbol's type
	sym.Type = semType
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

	case *ast.ParenExpr:
		// Check inner expression
		return checkExpr(ctx, mod, e.X, expected)

	case *ast.CastExpr:
		// Check expression being cast and validate cast compatibility
		sourceType := checkExpr(ctx, mod, e.X, types.TypeUnknown)
		targetType := typeFromTypeNodeWithContext(ctx, mod, e.Type)
		checkCastExpr(ctx, mod, e, sourceType, targetType)

	case *ast.CompositeLit:
		// Check all element expressions in the composite literal
		for _, elem := range e.Elts {
			if kv, ok := elem.(*ast.KeyValueExpr); ok {
				// Struct field: check the value
				if kv.Value != nil {
					checkExpr(ctx, mod, kv.Value, types.TypeUnknown)
				}
			} else {
				// Array element: check the expression directly
				checkExpr(ctx, mod, elem, types.TypeUnknown)
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
			// TODO: keep all array dynamic now
		}
		return types.NewArray(elementType, length)

	case *ast.OptionalType:
		// Optional type: T?
		innerType := typeFromTypeNodeWithContext(ctx, mod, t.Base)
		return types.NewOptional(innerType)

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
