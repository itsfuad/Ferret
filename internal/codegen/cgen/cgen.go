package cgen

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"compiler/colors"
	"compiler/internal/codegen"
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
	"compiler/internal/semantics/symbols"
	"compiler/internal/semantics/table"
	"compiler/internal/semantics/typechecker"
	"compiler/internal/tokens"
	"compiler/internal/types"
)

// interfaceAssignmentInfo holds information about an interface assignment
type interfaceAssignmentInfo struct {
	concreteType  types.SemType
	interfaceType types.SemType
	concreteName  string
	interfaceName string
	iface         *types.InterfaceType
	typeSym       *symbols.Symbol
}

// Generator generates C code from Ferret AST
type Generator struct {
	ctx          *context_v2.CompilerContext
	mod          *context_v2.Module
	buf          strings.Builder
	indent       int
	indentStr    string
	counter      int            // Counter for generating unique temporary variable names
	arrayLengths map[string]int // Map variable names to their array literal lengths
	optionalTyps map[string]types.SemType
}

// New creates a new C code generator
func New(ctx *context_v2.CompilerContext, mod *context_v2.Module) *Generator {
	optTypes := make(map[string]types.SemType)
	if stored := codegen.OptionalTypesFromModule(mod); stored != nil {
		for k, v := range stored {
			optTypes[k] = v
		}
	}
	return &Generator{
		ctx:          ctx,
		mod:          mod,
		indentStr:    "    ",
		arrayLengths: make(map[string]int),
		optionalTyps: optTypes,
	}
}

// Generate generates C code for the module
func (g *Generator) Generate() (string, error) {
	g.writeHeader()
	g.writeIncludes()
	g.writeRuntimeDeclarations()

	// Generate struct/type definitions first
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.TypeDecl:
				g.generateTypeDecl(n)
			}
		}
	}

	// Collect and generate all interface code upfront (wrappers and vtables)
	assignments := g.collectInterfaceAssignments()
	g.generateAllInterfaceCode(assignments)

	// Generate code for each top-level declaration
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.FuncDecl:
				g.generateFunction(n)
			case *ast.MethodDecl:
				g.generateMethod(n)
			case *ast.VarDecl:
				g.generateVarDecl(n)
			case *ast.ConstDecl:
				g.generateConstDecl(n)
			}
		}
	}

	return g.buf.String(), nil
}

func (g *Generator) writeHeader() {
	g.write("// Generated C code from Ferret\n")
	g.write("// Module: %s\n\n", g.mod.ImportPath)
}

func (g *Generator) writeIncludes() {
	codegen.WriteStandardIncludes(&g.buf)
}

func (g *Generator) writeRuntimeDeclarations() {
	// Include the runtime headers
	codegen.WriteRuntimeIncludes(&g.buf)
	g.write("\n")
}

func (g *Generator) generateFunction(decl *ast.FuncDecl) {
	if decl.Name == nil {
		return
	}

	funcName := decl.Name.Name

	// Get function signature from symbol
	sym, ok := g.mod.ModuleScope.GetSymbol(funcName)
	if !ok || sym == nil {
		g.ctx.ReportError(fmt.Sprintf("codegen: function '%s' not found in symbol table", funcName), nil)
		return
	}

	if sym.Type == nil {
		g.ctx.ReportError(fmt.Sprintf("codegen: function '%s' has no type information", funcName), nil)
		return
	}

	funcType, ok := sym.Type.(*types.FunctionType)
	if !ok {
		g.ctx.ReportError(fmt.Sprintf("codegen: function '%s' is not a function type (got %T)", funcName, sym.Type), nil)
		return
	}

	// Generate function signature
	// Special case: main() should return int
	returnType := g.typeToC(funcType.Return)
	if funcName == "main" && returnType == "void" {
		returnType = "int"
	}

	// For functions from non-entry modules, prefix with module name
	cFuncName := g.sanitizeName(funcName)
	if funcName != "main" && g.mod.ImportPath != g.ctx.EntryModule {
		// Prefix with module path to avoid conflicts
		modulePrefix := strings.ReplaceAll(strings.ReplaceAll(g.mod.ImportPath, "/", "_"), "-", "_")
		cFuncName = g.sanitizeName(modulePrefix) + "_" + cFuncName
	}

	g.write("%s %s(", returnType, cFuncName)

	// Generate parameters
	for i, param := range funcType.Params {
		if i > 0 {
			g.write(", ")
		}
		g.write("%s %s", g.typeToC(param.Type), g.sanitizeName(param.Name))
	}

	g.write(") {\n")
	g.indent++

	// Generate function body
	// Temporarily switch to function scope if available
	if decl.Body != nil {
		// If function has a scope (set during collection), use it
		if decl.Body.Scope != nil {
			if funcScope, ok := decl.Body.Scope.(*table.SymbolTable); ok {
				restore := g.mod.EnterScope(funcScope)
				defer restore()
			}
		}
		g.generateBlock(decl.Body)
	}

	// Add return 0 for main() if it returns int
	if funcName == "main" {
		funcType, ok := sym.Type.(*types.FunctionType)
		if ok && funcType.Return.Equals(types.TypeVoid) {
			g.writeIndent()
			g.write("return 0;\n")
		}
	}

	g.indent--
	g.write("}\n\n")
}

// generateTypeDecl generates C struct definition for a type declaration
func (g *Generator) generateTypeDecl(decl *ast.TypeDecl) {
	if decl.Name == nil {
		return
	}

	typeName := decl.Name.Name

	// Get type from symbol table
	sym, ok := g.mod.ModuleScope.GetSymbol(typeName)
	if !ok || sym == nil || sym.Type == nil {
		return
	}

	// Check if it's a struct type or interface type
	namedType, ok := sym.Type.(*types.NamedType)
	if !ok {
		return
	}

	// Handle struct types
	if structType, ok := namedType.Underlying.(*types.StructType); ok {

		// Generate struct definition
		cTypeName := g.sanitizeName(typeName)
		g.write("typedef struct {\n")
		g.indent++

		// Generate fields
		for _, field := range structType.Fields {
			g.writeIndent()
			fieldType := g.typeToC(field.Type)
			fieldName := g.sanitizeName(field.Name)
			g.write("%s %s;\n", fieldType, fieldName)
		}

		g.indent--
		g.write("} %s;\n\n", cTypeName)
		return
	}

	// Handle interface types - generate vtable type
	if ifaceType, ok := namedType.Underlying.(*types.InterfaceType); ok {
		g.generateInterfaceVTableType(typeName, ifaceType)
		return
	}

	// Other types (enums, etc.) handled elsewhere
}

// generateInterfaceVTableType generates the vtable struct type for an interface
func (g *Generator) generateInterfaceVTableType(interfaceName string, ifaceType *types.InterfaceType) {
	vtableTypeName := g.sanitizeName(interfaceName) + "_vtable"
	g.write("typedef struct {\n")
	g.indent++

	for _, method := range ifaceType.Methods {
		g.writeIndent()
		// Generate function pointer signature
		// Return type
		returnType := g.typeToC(method.FuncType.Return)

		// Generate parameter list: void* self, then method parameters
		g.write("%s (*%s)(", returnType, g.sanitizeName(method.Name))
		g.write("void*")
		for _, param := range method.FuncType.Params {
			g.write(", %s", g.typeToC(param.Type))
		}
		g.write(");\n")
	}

	g.indent--
	g.write("} %s;\n\n", vtableTypeName)
}

// isInterfaceType checks if a type is an interface (non-empty)
func (g *Generator) isInterfaceType(typ types.SemType) bool {
	if iface, ok := typ.(*types.InterfaceType); ok && len(iface.Methods) > 0 {
		return true
	}
	if named, ok := typ.(*types.NamedType); ok {
		if iface, ok := named.Underlying.(*types.InterfaceType); ok && len(iface.Methods) > 0 {
			return true
		}
	}
	return false
}

// getInterfaceType extracts the InterfaceType from a type (handles NamedType wrapping)
func (g *Generator) getInterfaceType(typ types.SemType) *types.InterfaceType {
	if iface, ok := typ.(*types.InterfaceType); ok {
		return iface
	}
	if named, ok := typ.(*types.NamedType); ok {
		if iface, ok := named.Underlying.(*types.InterfaceType); ok {
			return iface
		}
	}
	return nil
}

// collectInterfaceAssignments scans the AST to find all interface assignments
// Returns a map keyed by "concreteType:interfaceType" to avoid duplicates
func (g *Generator) collectInterfaceAssignments() map[string]*interfaceAssignmentInfo {
	assignments := make(map[string]*interfaceAssignmentInfo)

	var scanNode func(ast.Node)
	scanNode = func(node ast.Node) {
		if node == nil {
			return
		}

		switch n := node.(type) {
		case *ast.VarDecl:
			for _, item := range n.Decls {
				if item.Value != nil {
					// Get variable type from symbol or type annotation
					var varType types.SemType
					if item.Type != nil {
						varType = g.typeFromTypeNode(item.Type)
					} else if item.Value != nil {
						varType = g.inferExprType(item.Value)
					}

					if varType != nil && !varType.Equals(types.TypeUnknown) && g.isInterfaceType(varType) {
						concreteType := g.inferExprType(item.Value)
						if concreteType != nil && !concreteType.Equals(types.TypeUnknown) {
							// Get concrete type name
							var concreteTypeName string
							if named, ok := concreteType.(*types.NamedType); ok {
								concreteTypeName = named.Name
							} else {
								continue // Only named types can implement interfaces
							}

							// Create unique key
							interfaceName := g.getInterfaceName(varType)
							key := concreteTypeName + ":" + interfaceName

							if _, exists := assignments[key]; !exists {
								iface := g.getInterfaceType(varType)
								if iface != nil {
									typeSym, found := g.lookupTypeSymbol(concreteTypeName)
									assignments[key] = &interfaceAssignmentInfo{
										concreteType:  concreteType,
										interfaceType: varType,
										concreteName:  concreteTypeName,
										interfaceName: interfaceName,
										iface:         iface,
										typeSym:       typeSym,
									}
									if !found {
										assignments[key].typeSym = nil
									}
								}
							}
						}
					}
				}
			}
		case *ast.FuncDecl:
			if n.Body != nil {
				for _, stmt := range n.Body.Nodes {
					scanNode(stmt)
				}
			}
		case *ast.MethodDecl:
			if n.Body != nil {
				for _, stmt := range n.Body.Nodes {
					scanNode(stmt)
				}
			}
		case *ast.Block:
			for _, stmt := range n.Nodes {
				scanNode(stmt)
			}
		case *ast.IfStmt:
			if n.Body != nil {
				for _, stmt := range n.Body.Nodes {
					scanNode(stmt)
				}
			}
			if n.Else != nil {
				scanNode(n.Else)
			}
		case *ast.ForStmt:
			if n.Body != nil {
				for _, stmt := range n.Body.Nodes {
					scanNode(stmt)
				}
			}
		case *ast.WhileStmt:
			if n.Body != nil {
				for _, stmt := range n.Body.Nodes {
					scanNode(stmt)
				}
			}
		}
	}

	// Scan all top-level nodes
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			scanNode(node)
		}
	}

	return assignments
}

// generateAllInterfaceCode generates all wrapper functions and vtable instances upfront
func (g *Generator) generateAllInterfaceCode(assignments map[string]*interfaceAssignmentInfo) {
	if len(assignments) == 0 {
		return
	}

	g.write("\n// Interface wrapper functions and vtable instances\n")

	for _, info := range assignments {
		// Generate wrapper functions for each interface method
		if info.typeSym != nil && info.typeSym.Methods != nil {
			for _, method := range info.iface.Methods {
				methodInfo, hasMethod := info.typeSym.Methods[method.Name]
				if hasMethod {
					wrapperName := g.sanitizeName(info.concreteName) + "_" + g.sanitizeName(method.Name) + "_" + g.sanitizeName(info.interfaceName) + "_wrapper"
					methodCopy := method
					g.generateMethodWrapperForInterfaceAtFileScope(info.concreteName, method.Name, methodInfo.FuncType, wrapperName, &methodCopy)
				}
			}
		}

		// Generate vtable instance
		vtableTypeName := g.sanitizeName(info.interfaceName) + "_vtable"
		vtableVarName := g.sanitizeName(info.concreteName) + "_" + g.sanitizeName(info.interfaceName) + "_vtable_inst"

		// Use const for vtable - it's immutable and can be optimized better
		g.write("static const %s %s = {", vtableTypeName, vtableVarName)
		if info.typeSym != nil && info.typeSym.Methods != nil {
			first := true
			for _, method := range info.iface.Methods {
				if !first {
					g.write(", ")
				}
				first = false

				_, hasMethod := info.typeSym.Methods[method.Name]
				if hasMethod {
					wrapperName := g.sanitizeName(info.concreteName) + "_" + g.sanitizeName(method.Name) + "_" + g.sanitizeName(info.interfaceName) + "_wrapper"
					g.write(".%s = %s", g.sanitizeName(method.Name), wrapperName)
				}
			}
		}
		g.write("};\n")

		// Generate vtable pointer - non-const pointer to const vtable (for interface assignment)
		vtablePtrName := vtableVarName + "_ptr"
		g.write("static %s* %s = (void*)&%s;\n", vtableTypeName, vtablePtrName, vtableVarName)
	}

	g.write("\n")
}

// generateInterfaceAssignment generates code to assign a concrete type to an interface
// Creates interface value with data pointer and vtable
// Note: Wrappers and vtables are now pre-generated, so this just references them
func (g *Generator) generateInterfaceAssignment(varName string, ifaceType types.SemType, valueExpr ast.Expression) {
	// Get the concrete type from the value expression
	concreteType := g.inferExprType(valueExpr)
	if concreteType == nil || concreteType.Equals(types.TypeUnknown) {
		g.ctx.ReportError("codegen: cannot determine concrete type for interface assignment", nil)
		g.write("/* error: unknown type */")
		return
	}

	// Get interface type
	iface := g.getInterfaceType(ifaceType)
	if iface == nil {
		g.ctx.ReportError("codegen: expected interface type", nil)
		g.write("/* error: not an interface */")
		return
	}

	// Get concrete type name (for vtable generation)
	var concreteTypeName string
	if named, ok := concreteType.(*types.NamedType); ok {
		concreteTypeName = named.Name
	} else {
		g.ctx.ReportError("codegen: only named types can implement interfaces", nil)
		g.write("/* error: not a named type */")
		return
	}

	// Generate temporary variable for the concrete value
	g.counter++
	tempVarName := fmt.Sprintf("__temp_%d", g.counter)
	concreteCType := g.typeToC(concreteType)

	// Get vtable pointer name (pre-generated in generateAllInterfaceCode)
	interfaceName := g.getInterfaceName(ifaceType)
	vtableVarName := g.sanitizeName(concreteTypeName) + "_" + g.sanitizeName(interfaceName) + "_vtable_inst"
	vtablePtrName := vtableVarName + "_ptr"

	// Generate temporary variable at function scope (not in a block) to avoid scope issues
	g.writeIndent()
	g.write("%s %s = ", concreteCType, tempVarName)
	g.generateExpr(valueExpr)
	g.write(";\n")

	// Assign interface value - use address of temp variable (which is in function scope, not block scope)
	g.writeIndent()
	g.write("%s = (ferret_interface_t) { .data = &%s, .vtable = (void**)&%s };\n",
		g.sanitizeName(varName), tempVarName, vtablePtrName)
}

// generateInterfaceMethodCall generates code to call a method on an interface using vtable dispatch
func (g *Generator) generateInterfaceMethodCall(objExpr ast.Expression, methodName string, args []ast.Expression) {
	// Get interface type from objExpr
	objType := g.inferExprType(objExpr)
	iface := g.getInterfaceType(objType)
	if iface == nil {
		g.ctx.ReportError("codegen: expected interface type for method call", nil)
		g.write("/* error: not an interface */")
		return
	}

	// Find the method in the interface
	var ifaceMethod *types.InterfaceMethod
	for i := range iface.Methods {
		if iface.Methods[i].Name == methodName {
			ifaceMethod = &iface.Methods[i]
			break
		}
	}
	if ifaceMethod == nil {
		g.ctx.ReportError(fmt.Sprintf("codegen: method '%s' not found in interface", methodName), nil)
		g.write("/* error: method not in interface */")
		return
	}

	// Get interface name for vtable type
	interfaceName := "Interface"
	if named, ok := objType.(*types.NamedType); ok {
		interfaceName = named.Name
	}
	vtableTypeName := g.sanitizeName(interfaceName) + "_vtable"

	// Generate vtable cast and method call
	// vtable is void**, so we need to dereference it: ((Interface_vtable*)*obj.vtable)->method(obj.data, args...)
	g.write("((%s*)", vtableTypeName)
	g.write("*")
	g.generateExpr(objExpr)
	g.write(".vtable)->%s(", g.sanitizeName(methodName))
	g.generateExpr(objExpr)
	g.write(".data")

	for _, arg := range args {
		g.write(", ")
		g.generateExpr(arg)
	}
	g.write(")")
}

// generateMethodWrapperForInterface generates a wrapper function for interface method dispatch
// Converts void* self to concrete type and calls the actual method
// This version generates the wrapper at file scope (static, no indentation)
func (g *Generator) generateMethodWrapperForInterfaceAtFileScope(typeName, methodName string, funcType *types.FunctionType, wrapperName string, ifaceMethod *types.InterfaceMethod) {
	// Generate wrapper function signature matching interface method
	// Use inline hint for performance - these are small functions called frequently
	returnType := g.typeToC(ifaceMethod.FuncType.Return)
	g.write("static inline %s %s(void* self", returnType, wrapperName)

	// Use interface method parameters (they define the vtable signature)
	for _, param := range ifaceMethod.FuncType.Params {
		g.write(", %s %s", g.typeToC(param.Type), g.sanitizeName(param.Name))
	}
	g.write(") {\n")
	g.indent++

	// Cast self to concrete type and call actual method
	concreteCType := g.sanitizeName(typeName)
	actualMethodName := g.sanitizeName(typeName) + "_" + g.sanitizeName(methodName)

	g.writeIndent()
	if !ifaceMethod.FuncType.Return.Equals(types.TypeVoid) {
		g.write("return ")
	}

	// Call actual method: TypeName_method((TypeName*)self, params...)
	// self is already a void* (pointer), so we just cast it to the concrete type
	// If the method takes a pointer receiver, self is already a pointer, so no & needed
	g.write("%s((%s*)self", actualMethodName, concreteCType)

	// Pass through parameters
	for _, param := range ifaceMethod.FuncType.Params {
		g.write(", %s", g.sanitizeName(param.Name))
	}
	g.write(");\n")

	g.indent--
	g.write("}\n\n")
}

// getInterfaceName gets the interface name from a type
func (g *Generator) getInterfaceName(typ types.SemType) string {
	if named, ok := typ.(*types.NamedType); ok {
		return named.Name
	}
	// For anonymous interfaces, generate a name
	return "Interface"
}

// lookupTypeSymbol looks up a type symbol in the current module or imports
func (g *Generator) lookupTypeSymbol(typeName string) (*symbols.Symbol, bool) {
	// Check current module
	if sym, found := g.mod.ModuleScope.Lookup(typeName); found {
		return sym, true
	}

	// Check imported modules
	for _, importPath := range g.mod.ImportAliasMap {
		if importedMod, exists := g.ctx.GetModule(importPath); exists {
			if sym, ok := importedMod.ModuleScope.GetSymbol(typeName); ok && sym.Kind == symbols.SymbolType {
				return sym, true
			}
		}
	}

	return nil, false
}

// generateMethodWrapper generates a wrapper function that converts void* to concrete type
func (g *Generator) generateMethodWrapper(typeName, methodName string, funcType *types.FunctionType, wrapperName string) {
	// This is kept for backward compatibility but should use generateMethodWrapperForInterface
	// Generate wrapper function signature
	returnType := g.typeToC(funcType.Return)
	g.write("\nstatic %s %s(void* self", returnType, wrapperName)

	for _, param := range funcType.Params {
		g.write(", %s", g.typeToC(param.Type))
	}
	g.write(") {\n")
	g.indent++

	// Cast self to concrete type and call method
	concreteType := g.typeToC(types.NewNamed(typeName, nil)) // This will be the struct name
	actualMethodName := g.sanitizeName(typeName) + "_" + g.sanitizeName(methodName)

	g.writeIndent()
	if !funcType.Return.Equals(types.TypeVoid) {
		g.write("return ")
	}
	g.write("%s((%s*)self", actualMethodName, concreteType)
	for i := range funcType.Params {
		g.write(", param%d", i)
	}
	g.write(");\n")

	g.indent--
	g.write("}\n")
}

// generateMethod generates C code for a method declaration
// Methods are converted to functions with the receiver as the first parameter
func (g *Generator) generateMethod(decl *ast.MethodDecl) {
	if decl.Name == nil || decl.Receiver == nil {
		return
	}

	methodName := decl.Name.Name

	// Get receiver type
	receiverType := g.typeFromTypeNode(decl.Receiver.Type)
	if receiverType == nil || receiverType.Equals(types.TypeUnknown) {
		return
	}

	// Handle reference types: unwrap &T to get T
	var namedType *types.NamedType
	var isReference bool
	if refType, ok := receiverType.(*types.ReferenceType); ok {
		isReference = true
		if nt, ok := refType.Inner.(*types.NamedType); ok {
			namedType = nt
		}
	} else if nt, ok := receiverType.(*types.NamedType); ok {
		namedType = nt
	}

	// Get method signature from symbol (if available)
	// Methods are stored in the type's Methods map
	var funcType *types.FunctionType
	if namedType != nil {
		if typeSym, found := g.mod.ModuleScope.GetSymbol(namedType.Name); found && typeSym != nil {
			if typeSym.Methods != nil {
				if methodInfo, exists := typeSym.Methods[methodName]; exists && methodInfo != nil {
					funcType = methodInfo.FuncType
				}
			}
		}
	}

	// Fallback: try to construct from AST
	if funcType == nil && decl.Type != nil {
		// Build function type from AST
		// This is a simplified version - full implementation would need proper type resolution
		return // Skip for now if we can't get proper type info
	}

	if funcType == nil {
		return
	}

	// Generate function name: TypeName_methodName
	receiverTypeName := "unknown"
	if namedType != nil {
		receiverTypeName = namedType.Name
	}
	cFuncName := g.sanitizeName(receiverTypeName) + "_" + g.sanitizeName(methodName)

	// Generate return type
	returnType := g.typeToC(funcType.Return)

	g.write("%s %s(", returnType, cFuncName)

	// First parameter is the receiver
	// For reference types, the parameter type is T* (pointer), otherwise use receiverType as-is
	var receiverCType string
	if isReference && namedType != nil {
		// For &T, generate T* in C
		receiverCType = g.typeToC(namedType) + "*"
	} else {
		receiverCType = g.typeToC(receiverType)
	}
	receiverName := "self"
	if decl.Receiver.Name != nil {
		receiverName = g.sanitizeName(decl.Receiver.Name.Name)
	}
	g.write("%s %s", receiverCType, receiverName)

	// Generate other parameters
	for i, param := range funcType.Params {
		g.write(", ")
		g.write("%s %s", g.typeToC(param.Type), g.sanitizeName(param.Name))
		_ = i // Avoid unused variable warning
	}

	g.write(") {\n")
	g.indent++

	// Generate method body
	if decl.Body != nil {
		// Enter method scope
		if decl.Body.Scope != nil {
			if methodScope, ok := decl.Body.Scope.(*table.SymbolTable); ok {
				restore := g.mod.EnterScope(methodScope)
				defer restore()
			}
		}
		g.generateBlock(decl.Body)
	}

	g.indent--
	g.write("}\n\n")
}

// generateFunctionSignature generates just the function signature (forward declaration)
func (g *Generator) generateFunctionSignature(decl *ast.FuncDecl) {
	if decl.Name == nil {
		return
	}

	funcName := decl.Name.Name

	// Get function signature from symbol
	sym, ok := g.mod.ModuleScope.GetSymbol(funcName)
	if !ok || sym == nil {
		return // Skip if symbol not found
	}

	if sym.Type == nil {
		return
	}

	funcType, ok := sym.Type.(*types.FunctionType)
	if !ok {
		return
	}

	// Generate function signature
	returnType := g.typeToC(funcType.Return)
	if funcName == "main" && returnType == "void" {
		returnType = "int"
	}

	// For functions from non-entry modules, prefix with module name
	cFuncName := g.sanitizeName(funcName)
	if funcName != "main" && g.mod.ImportPath != g.ctx.EntryModule {
		// Prefix with module path to avoid conflicts
		modulePrefix := strings.ReplaceAll(strings.ReplaceAll(g.mod.ImportPath, "/", "_"), "-", "_")
		cFuncName = g.sanitizeName(modulePrefix) + "_" + cFuncName
	}

	g.write("%s %s(", returnType, cFuncName)

	// Generate parameters
	for i, param := range funcType.Params {
		if i > 0 {
			g.write(", ")
		}
		g.write("%s %s", g.typeToC(param.Type), g.sanitizeName(param.Name))
	}

	g.write(");\n")
}

// GenerateFunctionDeclarations generates forward declarations for all functions and constants in the module
func (g *Generator) GenerateFunctionDeclarations() string {
	var buf strings.Builder
	oldBuf := g.buf
	g.buf = buf

	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.FuncDecl:
				g.generateFunctionSignature(n)
			case *ast.ConstDecl:
				g.generateConstDeclaration(n)
			}
		}
	}

	result := g.buf.String()
	g.buf = oldBuf
	return result
}

// generateConstDeclaration generates an extern declaration for a constant
func (g *Generator) generateConstDeclaration(decl *ast.ConstDecl) {
	for _, item := range decl.Decls {
		if item.Name.Name == "_" {
			continue // Skip placeholder
		}

		// Use ModuleScope for module-level constants
		sym, ok := g.mod.ModuleScope.GetSymbol(item.Name.Name)
		if !ok || sym == nil || sym.Type == nil {
			continue
		}

		// For constants from non-entry modules, prefix with module name
		constName := g.sanitizeName(item.Name.Name)
		if g.mod.ImportPath != g.ctx.EntryModule {
			modulePrefix := strings.ReplaceAll(strings.ReplaceAll(g.mod.ImportPath, "/", "_"), "-", "_")
			constName = g.sanitizeName(modulePrefix) + "_" + constName
		}

		// Generate extern declaration
		g.write("extern const %s %s;\n", g.typeToC(sym.Type), constName)
	}
}

// GenerateHeader generates a C header file for the module
func (g *Generator) GenerateHeader() string {
	var buf strings.Builder
	oldBuf := g.buf
	g.buf = buf

	// Collect optional types up front so typedefs exist before use
	g.collectOptionalTypes()

	// Generate header guard
	guardName := strings.ToUpper(strings.ReplaceAll(strings.ReplaceAll(g.mod.ImportPath, "/", "_"), "-", "_")) + "_H"
	g.write("#ifndef %s\n", guardName)
	g.write("#define %s\n\n", guardName)
	codegen.WriteHeaderIncludes(&g.buf)

	// Include runtime headers (shared list)
	codegen.WriteRuntimeIncludes(&g.buf)
	g.write("\n")

	// Generate optional type definitions for all optional types used in the module
	g.generateOptionalTypeDefinitions()

	// Generate type definitions (structs, interfaces, etc.)
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.TypeDecl:
				g.generateTypeDecl(n)
			}
		}
	}

	// Generate function and constant declarations
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.FuncDecl:
				g.generateFunctionSignature(n)
			case *ast.MethodDecl:
				// Methods are converted to functions, so generate function signature
				if n.Name != nil && n.Receiver != nil {
					receiverType := g.typeFromTypeNode(n.Receiver.Type)
					if receiverType != nil && !receiverType.Equals(types.TypeUnknown) {
						var typeName string
						if named, ok := receiverType.(*types.NamedType); ok {
							typeName = named.Name
						} else if ref, ok := receiverType.(*types.ReferenceType); ok {
							if named, ok := ref.Inner.(*types.NamedType); ok {
								typeName = named.Name
							}
						}
						if typeName != "" {
							// Get method signature from symbol
							if typeSym, found := g.mod.ModuleScope.GetSymbol(typeName); found && typeSym != nil && typeSym.Methods != nil {
								if methodInfo, exists := typeSym.Methods[n.Name.Name]; exists && methodInfo != nil {
									funcName := g.sanitizeName(typeName) + "_" + g.sanitizeName(n.Name.Name)
									returnType := g.typeToC(methodInfo.FuncType.Return)
									g.write("%s %s(", returnType, funcName)
									// First parameter is receiver
									var receiverCType string
									if ref, ok := receiverType.(*types.ReferenceType); ok {
										if named, ok := ref.Inner.(*types.NamedType); ok {
											receiverCType = g.typeToC(named) + "*"
										}
									} else {
										receiverCType = g.typeToC(receiverType)
									}
									g.write("%s self", receiverCType)
									// Other parameters
									for _, param := range methodInfo.FuncType.Params {
										g.write(", %s %s", g.typeToC(param.Type), g.sanitizeName(param.Name))
									}
									g.write(");\n")
								}
							}
						}
					}
				}
			case *ast.ConstDecl:
				g.generateConstDeclaration(n)
			}
		}
	}

	g.write("\n#endif // %s\n", guardName)

	result := g.buf.String()
	g.buf = oldBuf
	return result
}

// GenerateImplementation generates C implementation code (without includes - they're in the .c file)
func (g *Generator) GenerateImplementation() string {
	var buf strings.Builder
	oldBuf := g.buf
	g.buf = buf

	// Don't write includes here - they're added by the pipeline when creating the .c file.
	// This keeps the implementation clean and allows the .c file to include its .h file.

	// Collect and generate all interface code upfront (wrappers and vtables)
	assignments := g.collectInterfaceAssignments()
	g.generateAllInterfaceCode(assignments)

	// Generate code for each top-level declaration
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.FuncDecl:
				g.generateFunction(n)
			case *ast.MethodDecl:
				g.generateMethod(n)
			case *ast.VarDecl:
				g.generateVarDecl(n)
			case *ast.ConstDecl:
				g.generateConstDecl(n)
			}
		}
	}

	result := g.buf.String()
	g.buf = oldBuf
	return result
}

func (g *Generator) generateVarDecl(decl *ast.VarDecl) {
	for _, item := range decl.Decls {
		if item.Name.Name == "_" {
			continue // Skip placeholder
		}

		// Get symbol for type (use Lookup to search parent scopes including function scopes)
		sym, ok := g.mod.CurrentScope.Lookup(item.Name.Name)
		if !ok || sym == nil {
			g.ctx.ReportError(fmt.Sprintf("codegen: variable '%s' not found in symbol table", item.Name.Name), nil)
			continue
		}

		var varType types.SemType
		// Prefer symbol's resolved type (already type-checked)
		if sym.Type != nil && !sym.Type.Equals(types.TypeUnknown) {
			varType = sym.Type
		} else if item.Type != nil {
			// Fallback to parsing type node (shouldn't happen if type checking worked)
			varType = g.typeFromTypeNode(item.Type)
			if varType == types.TypeUnknown {
				g.ctx.ReportError(fmt.Sprintf("codegen: could not resolve type for variable '%s'", item.Name.Name), nil)
				continue
			}
		} else if item.Value != nil {
			// Type inference from value expression
			varType = g.inferExprType(item.Value)
			if varType == nil || varType.Equals(types.TypeUnknown) {
				g.ctx.ReportError(fmt.Sprintf("codegen: could not infer type for variable '%s' from expression", item.Name.Name), nil)
				continue
			}
		} else {
			g.ctx.ReportError(fmt.Sprintf("codegen: variable '%s' has no type information", item.Name.Name), nil)
			continue
		}

		g.writeIndent()
		// For anonymous structs, generate the struct type inline instead of using void*
		var cType string
		if structType, ok := varType.(*types.StructType); ok {
			cType = g.structTypeToC(structType)
		} else {
			cType = g.typeToC(varType)
		}
		g.write("%s %s", cType, g.sanitizeName(item.Name.Name))

		if item.Value != nil {
			// Check if this is an interface assignment
			if g.isInterfaceType(varType) {
				// Interface assignment - declare variable first, then assign in a block
				g.write(";\n")
				// Generate assignment in a block scope (for temporary variables)
				g.generateInterfaceAssignment(item.Name.Name, varType, item.Value)
			} else {
				// Regular assignment
				g.write(" = ")
				// For interface{} (void*), we need to cast the value appropriately
				isEmptyInterface := false
				if iface, ok := varType.(*types.InterfaceType); ok && len(iface.Methods) == 0 {
					isEmptyInterface = true
				} else if named, ok := varType.(*types.NamedType); ok {
					if iface, ok := named.Underlying.(*types.InterfaceType); ok && len(iface.Methods) == 0 {
						isEmptyInterface = true
					}
				}

				if isEmptyInterface {
					// Empty interface - need to cast based on value type
					// For now, use a simple cast (works for pointers, but integers need uintptr_t)
					// In a full implementation, we'd need type information at runtime
					g.write("(void*)(uintptr_t)(")
					g.generateExpr(item.Value)
					g.write(")")
				} else {
					// Check if we need to wrap a value into an optional (T -> T?)
					if optType, ok := varType.(*types.OptionalType); ok {
						// Check if value is a none literal
						if lit, ok := item.Value.(*ast.BasicLit); ok && lit.Kind == ast.NONE {
							// Initializing with none - generate proper none struct
							g.write("((ferret_optional_%s){.value = 0, .is_some = 0})", g.getOptionalTypeName(optType.Inner))
						} else {
							// Check if value is a literal that needs wrapping
							if lit, ok := item.Value.(*ast.BasicLit); ok {
								// For integer/float literals with numeric inner types, always wrap
								if lit.Kind == ast.INT || lit.Kind == ast.FLOAT {
									innerUnwrapped := types.UnwrapType(optType.Inner)
									if prim, ok := innerUnwrapped.(*types.PrimitiveType); ok {
										// Check if inner type is numeric
										name := prim.GetName()
										isNumeric := name == types.TYPE_I8 || name == types.TYPE_I16 || name == types.TYPE_I32 || name == types.TYPE_I64 ||
											name == types.TYPE_U8 || name == types.TYPE_U16 || name == types.TYPE_U32 || name == types.TYPE_U64 ||
											name == types.TYPE_F32 || name == types.TYPE_F64
										if isNumeric {
											// Wrap: T -> T? by creating optional struct
											g.write("((ferret_optional_%s){.value = ", g.getOptionalTypeName(optType.Inner))
											g.generateExpr(item.Value)
											g.write(", .is_some = 1})")
										} else {
											g.generateExpr(item.Value)
										}
									} else {
										g.generateExpr(item.Value)
									}
								} else {
									// For non-numeric literals, check type compatibility
									valueType := g.inferExprType(item.Value)
									innerUnwrapped := types.UnwrapType(optType.Inner)
									if valueType != nil && !valueType.Equals(types.TypeUnknown) {
										valueUnwrapped := types.UnwrapType(valueType)
										// Check if types match
										if valueUnwrapped.Equals(innerUnwrapped) {
											// Wrap: T -> T? by creating optional struct
											g.write("((ferret_optional_%s){.value = ", g.getOptionalTypeName(optType.Inner))
											g.generateExpr(item.Value)
											g.write(", .is_some = 1})")
										} else {
											g.generateExpr(item.Value)
										}
									} else {
										g.generateExpr(item.Value)
									}
								}
							} else {
								valueType := g.inferExprType(item.Value)
								// Variable is optional - check if value needs wrapping
								if valueType != nil && !valueType.Equals(types.TypeUnknown) {
									// Unwrap types to compare inner types correctly
									unwrappedValueType := types.UnwrapType(valueType)
									unwrappedInnerType := types.UnwrapType(optType.Inner)

									// Check if value type matches inner type (needs wrapping)
									if unwrappedValueType.Equals(unwrappedInnerType) {
										// Wrap: T -> T? by creating optional struct
										g.write("((ferret_optional_%s){.value = ", g.getOptionalTypeName(optType.Inner))
										g.generateExpr(item.Value)
										g.write(", .is_some = 1})")
									} else {
										// Value is already optional or different type - generate as-is
										g.generateExpr(item.Value)
									}
								} else {
									// Type unknown - generate as-is
									g.generateExpr(item.Value)
								}
							}
						}
					} else if named, ok := varType.(*types.NamedType); ok {
						// Check if underlying is optional
						if optType, ok := named.Underlying.(*types.OptionalType); ok {
							// Check if value is a none literal
							if lit, ok := item.Value.(*ast.BasicLit); ok && lit.Kind == ast.NONE {
								// Initializing with none - generate proper none struct
								g.write("((ferret_optional_%s){.value = 0, .is_some = 0})", g.getOptionalTypeName(optType.Inner))
							} else {
								valueType := g.inferExprType(item.Value)
								if valueType != nil && !valueType.Equals(types.TypeUnknown) {
									unwrappedValueType := types.UnwrapType(valueType)
									unwrappedInnerType := types.UnwrapType(optType.Inner)
									if unwrappedValueType.Equals(unwrappedInnerType) {
										// Wrap: T -> T?
										g.write("((ferret_optional_%s){.value = ", g.getOptionalTypeName(optType.Inner))
										g.generateExpr(item.Value)
										g.write(", .is_some = 1})")
									} else {
										g.generateExpr(item.Value)
									}
								} else {
									g.generateExpr(item.Value)
								}
							}
						} else {
							// If the value is a CompositeLit and we have a type from the variable declaration,
							// use the variable's type directly for generation
							// This handles cases like: let fruits : []str = ["apple", "banana", "cherry"];
							if compLit, ok := item.Value.(*ast.CompositeLit); ok {
								if compLit.Type == nil && varType != nil && !varType.Equals(types.TypeUnknown) {
									// Generate composite literal with the variable's type
									// For variable initialization, don't use cast for anonymous structs
									g.generateCompositeLitWithType(compLit, varType, true)
									// Store array length for dynamic arrays initialized with literals
									if arrayType, ok := varType.(*types.ArrayType); ok && arrayType.Length < 0 {
										g.arrayLengths[item.Name.Name] = len(compLit.Elts)
									}
								} else {
									g.generateExpr(item.Value)
								}
							} else {
								g.generateExpr(item.Value)
							}
						}
					} else {
						// If the value is a CompositeLit and we have a type from the variable declaration,
						// use the variable's type directly for generation
						// This handles cases like: let fruits : []str = ["apple", "banana", "cherry"];
						if compLit, ok := item.Value.(*ast.CompositeLit); ok {
							if compLit.Type == nil && varType != nil && !varType.Equals(types.TypeUnknown) {
								// Generate composite literal with the variable's type
								// For variable initialization, don't use cast for anonymous structs
								g.generateCompositeLitWithType(compLit, varType, true)
								// Store array length for dynamic arrays initialized with literals
								if arrayType, ok := varType.(*types.ArrayType); ok && arrayType.Length < 0 {
									g.arrayLengths[item.Name.Name] = len(compLit.Elts)
								}
							} else {
								g.generateExpr(item.Value)
							}
						} else {
							g.generateExpr(item.Value)
						}
					}
				}
				g.write(";\n")
			}
		} else {
			g.write(";\n")
		}
	}
}

func (g *Generator) generateConstDecl(decl *ast.ConstDecl) {
	// Constants are similar to variables but with const keyword
	for _, item := range decl.Decls {
		// Use Lookup to search parent scopes
		sym, ok := g.mod.CurrentScope.Lookup(item.Name.Name)
		if !ok || sym == nil {
			g.ctx.ReportError(fmt.Sprintf("codegen: constant '%s' not found in symbol table", item.Name.Name), nil)
			continue
		}

		var varType types.SemType
		// Prefer symbol's resolved type (already type-checked)
		if sym.Type != nil {
			varType = sym.Type
		} else if item.Type != nil {
			// Fallback to parsing type node (shouldn't happen if type checking worked)
			varType = g.typeFromTypeNode(item.Type)
			if varType == types.TypeUnknown {
				g.ctx.ReportError(fmt.Sprintf("codegen: could not resolve type for constant '%s'", item.Name.Name), nil)
				continue
			}
		} else {
			g.ctx.ReportError(fmt.Sprintf("codegen: constant '%s' has no type information", item.Name.Name), nil)
			continue
		}

		g.writeIndent()
		// For constants from non-entry modules, prefix with module name
		constName := g.sanitizeName(item.Name.Name)
		if g.mod.ImportPath != g.ctx.EntryModule {
			// Prefix with module path to avoid conflicts
			modulePrefix := strings.ReplaceAll(strings.ReplaceAll(g.mod.ImportPath, "/", "_"), "-", "_")
			constName = g.sanitizeName(modulePrefix) + "_" + constName
		}
		g.write("const %s %s", g.typeToC(varType), constName)

		if item.Value != nil {
			g.write(" = ")
			g.generateExpr(item.Value)
		}

		g.write(";\n")
	}
}

func (g *Generator) generateBlock(block *ast.Block) {
	// Enter block scope if it exists (for nested scopes)
	// Note: Block scopes are children of their parent scope (e.g., for loop scope)
	// So Lookup will search parent scopes automatically
	if block.Scope != nil {
		if blockScope, ok := block.Scope.(*table.SymbolTable); ok {
			restore := g.mod.EnterScope(blockScope)
			defer restore()
		}
	}

	for _, node := range block.Nodes {
		g.generateStmt(node)
	}
}

// isPrintLikeCall returns true for Print/Println calls (native io helpers) so
// we can emit them as full statements without an extra semicolon.
func (g *Generator) isPrintLikeCall(call *ast.CallExpr) bool {
	switch fn := call.Fun.(type) {
	case *ast.ScopeResolutionExpr:
		if fn.Selector == nil {
			return false
		}
		name := fn.Selector.Name
		return name == "Print" || name == "Println"
	case *ast.IdentifierExpr:
		return fn.Name == "Print" || fn.Name == "Println"
	default:
		return false
	}
}

func (g *Generator) generateStmt(stmt ast.Node) {
	switch s := stmt.(type) {
	case *ast.VarDecl:
		g.generateVarDecl(s)
	case *ast.AssignStmt:
		g.generateAssignStmt(s)
	case *ast.ReturnStmt:
		g.generateReturnStmt(s)
	case *ast.IfStmt:
		g.generateIfStmt(s)
	case *ast.ForStmt:
		g.generateForStmt(s)
	case *ast.WhileStmt:
		g.generateWhileStmt(s)
	case *ast.BreakStmt:
		g.writeIndent()
		g.write("break;\n")
	case *ast.ContinueStmt:
		g.writeIndent()
		g.write("continue;\n")
	case *ast.ExprStmt:
		// Check if the expression is nil or empty (shouldn't happen, but be safe)
		if s.X == nil {
			return
		}
		if call, ok := s.X.(*ast.CallExpr); ok && g.isPrintLikeCall(call) {
			// Print/Println emit their own indentation/terminators
			g.generateCallExpr(call)
		} else {
			g.writeIndent()
			g.generateExpr(s.X)
			g.write(";\n")
		}
	case *ast.Block:
		g.writeIndent()
		g.write("{\n")
		g.indent++
		g.generateBlock(s)
		g.indent--
		g.writeIndent()
		g.write("}\n")
	default:
		// Unsupported statement type
		g.ctx.ReportError(fmt.Sprintf("codegen: unsupported statement type %T (not yet implemented)", s), nil)
		g.writeIndent()
		g.write("/* unsupported statement: %T */\n", s)
	}
}

func (g *Generator) generateAssignStmt(stmt *ast.AssignStmt) {
	g.writeIndent()

	// Handle increment/decrement operators (x++, x--)
	if stmt.Op != nil && (stmt.Op.Kind == tokens.PLUS_PLUS_TOKEN || stmt.Op.Kind == tokens.MINUS_MINUS_TOKEN) {
		g.generateExpr(stmt.Lhs)
		g.write(" %s;\n", stmt.Op.Value)
		return
	}

	// Handle compound assignment operators (x += y, x -= y, etc.)
	if stmt.Op != nil && stmt.Op.Kind != tokens.EQUALS_TOKEN {
		g.generateExpr(stmt.Lhs)
		g.write(" %s ", stmt.Op.Value)
		g.generateExpr(stmt.Rhs)
		g.write(";\n")
		return
	}

	// Map element assignment: map[key] = value
	if idx, ok := stmt.Lhs.(*ast.IndexExpr); ok {
		if mapType, ok := types.UnwrapType(g.inferExprType(idx.X)).(*types.MapType); ok {
			g.counter++
			mapTemp := fmt.Sprintf("__map_set_%d", g.counter)
			g.counter++
			keyTemp := fmt.Sprintf("__map_key_%d", g.counter)
			g.counter++
			valTemp := fmt.Sprintf("__map_val_%d", g.counter)

			g.write("{\n")
			g.indent++
			g.writeIndent()
			g.write("ferret_map_t* %s = ", mapTemp)
			g.generateExpr(idx.X)
			g.write(";\n")

			g.writeIndent()
			g.write("%s %s = ", g.typeToC(mapType.Key), keyTemp)
			g.generateExpr(idx.Index)
			g.write(";\n")

			g.writeIndent()
			g.write("%s %s = ", g.typeToC(mapType.Value), valTemp)
			g.generateExpr(stmt.Rhs)
			g.write(";\n")

			g.writeIndent()
			g.write("ferret_map_set(%s, &%s, &%s);\n", mapTemp, keyTemp, valTemp)
			g.indent--
			g.writeIndent()
			g.write("}\n")
			return
		}
	}

	// Regular assignment (x = y)
	// Check if we need to wrap the value for optional types
	lhsType := g.inferExprType(stmt.Lhs)
	rhsType := g.inferExprType(stmt.Rhs)

	g.generateExpr(stmt.Lhs)
	g.write(" = ")

	// Check if LHS is optional and RHS needs wrapping
	if lhsType != nil && !lhsType.Equals(types.TypeUnknown) {
		var lhsOptType *types.OptionalType
		if opt, ok := lhsType.(*types.OptionalType); ok {
			lhsOptType = opt
		} else if named, ok := lhsType.(*types.NamedType); ok {
			if opt, ok := named.Underlying.(*types.OptionalType); ok {
				lhsOptType = opt
			}
		}

		if lhsOptType != nil {
			// Check if RHS is a none literal (check both AST node and type)
			isNone := false
			if lit, ok := stmt.Rhs.(*ast.BasicLit); ok && lit.Kind == ast.NONE {
				isNone = true
			} else if rhsType != nil && rhsType.Equals(types.TypeNone) {
				// Also check if type is TypeNone
				isNone = true
			}

			if isNone {
				// Assigning none to optional - generate proper none struct
				g.write("((ferret_optional_%s){.value = 0, .is_some = 0})", g.getOptionalTypeName(lhsOptType.Inner))
				g.write(";\n")
				return
			}

			if rhsType != nil && !rhsType.Equals(types.TypeUnknown) {
				// LHS is optional - check if RHS needs wrapping
				unwrappedRhsType := types.UnwrapType(rhsType)
				unwrappedInnerType := types.UnwrapType(lhsOptType.Inner)

				if unwrappedRhsType.Equals(unwrappedInnerType) {
					// RHS is the inner type, needs wrapping: T -> T?
					g.write("((ferret_optional_%s){.value = ", g.getOptionalTypeName(lhsOptType.Inner))
					g.generateExpr(stmt.Rhs)
					g.write(", .is_some = 1})")
					g.write(";\n")
					return
				}
			}

			// Fallback: if RHS type is unknown or check failed, but LHS is optional and RHS is a literal,
			// check if it's a none literal that wasn't caught above
			if lit, ok := stmt.Rhs.(*ast.BasicLit); ok && lit.Kind == ast.NONE {
				// Assigning none to optional - generate proper none struct
				g.write("((ferret_optional_%s){.value = 0, .is_some = 0})", g.getOptionalTypeName(lhsOptType.Inner))
				g.write(";\n")
				return
			}
		}
	}

	// No wrapping needed - generate as-is
	g.generateExpr(stmt.Rhs)
	g.write(";\n")
}

func (g *Generator) generateReturnStmt(stmt *ast.ReturnStmt) {
	g.writeIndent()
	g.write("return")
	if stmt.Result != nil {
		g.write(" ")
		g.generateExpr(stmt.Result)
	}
	g.write(";\n")
}

func (g *Generator) generateIfStmt(stmt *ast.IfStmt) {
	g.writeIndent()
	g.write("if (")
	g.generateExpr(stmt.Cond)
	g.write(") {\n")
	g.indent++

	if stmt.Body != nil {
		g.generateBlock(stmt.Body)
	}

	g.indent--
	g.writeIndent()
	g.write("}")

	if stmt.Else != nil {
		if elseIf, ok := stmt.Else.(*ast.IfStmt); ok {
			g.write(" else ")
			g.generateIfStmt(elseIf)
		} else if elseBlock, ok := stmt.Else.(*ast.Block); ok {
			g.write(" else {\n")
			g.indent++
			g.generateBlock(elseBlock)
			g.indent--
			g.writeIndent()
			g.write("}\n")
		}
	} else {
		g.write("\n")
	}
}

func (g *Generator) generateForStmt(stmt *ast.ForStmt) {
	// Ferret: for [let] i in range { }
	// C: for (int i = start; i < end; i += incr) { }

	// Check if range is a RangeExpr or array/map expression
	rangeExpr, isRange := stmt.Range.(*ast.RangeExpr)
	if !isRange {
		// Try to handle array/map iteration
		rangeType := g.inferExprType(stmt.Range)
		if rangeType == nil || rangeType.Equals(types.TypeUnknown) {
			g.ctx.ReportError("codegen: cannot determine type of for loop range", nil)
			g.writeIndent()
			g.write("/* unsupported for loop range */\n")
			return
		}

		// Check if it's an array type
		if arrayType, ok := rangeType.(*types.ArrayType); ok {
			g.generateForArray(stmt, arrayType)
			return
		}

		// Check if it's a map type
		if mapType, ok := rangeType.(*types.MapType); ok {
			g.generateForMap(stmt, mapType)
			return
		}

		// Unsupported type
		g.ctx.ReportError("codegen: for loop range must be a range, array, or map", nil)
		g.writeIndent()
		g.write("/* unsupported for loop range type */\n")
		return
	}

	// Extract iterator variable name(s)
	// Iterator is always VarDecl (always binds new variables)
	varDecl, ok := stmt.Iterator.(*ast.VarDecl)
	if !ok {
		g.ctx.ReportError("codegen: iterator must be VarDecl", nil)
		g.writeIndent()
		g.write("/* invalid iterator type */\n")
		return
	}

	var iterVarName string
	var valueVarName string
	hasValueVar := false

	if len(varDecl.Decls) > 0 {
		iterVarName = varDecl.Decls[0].Name.Name
	}
	if len(varDecl.Decls) > 1 {
		valueVarName = varDecl.Decls[1].Name.Name
		hasValueVar = true
	}

	// Check if iterator variable is discard (_) - if so, generate a temp variable
	isDiscardIter := (iterVarName == "_")
	isDiscardValue := (valueVarName == "_")

	// If iterator is discard, generate a unique temp variable name
	actualIterVarName := iterVarName
	if isDiscardIter {
		g.counter++
		actualIterVarName = fmt.Sprintf("__loop_idx_%d", g.counter)
	}

	// Iterator always binds new variables (always VarDecl)
	iterType := "int32_t"
	if len(varDecl.Decls) > 0 {
		if sym, found := g.mod.CurrentScope.Lookup(iterVarName); found && sym != nil && sym.Type != nil {
			iterType = g.typeToC(sym.Type)
		}
	}

	// Enter for loop scope if available (for variable declarations)
	if stmt.Scope != nil {
		if forScope, ok := stmt.Scope.(*table.SymbolTable); ok {
			restore := g.mod.EnterScope(forScope)
			defer restore()
		}
	}

	// Generate C for loop
	g.writeIndent()
	if hasValueVar {
		// Dual variables: i is index (0, 1, 2, ...), v is value (start + i)
		// Generate loop from 0 to (end - start) for exclusive, or (end - start) for inclusive
		// For inclusive: i <= (end - start) gives indices 0..(end-start), which is (end-start+1) iterations
		// For exclusive: i < (end - start) gives indices 0..(end-start-1), which is (end-start) iterations
		g.write("for (%s %s = 0; %s ", iterType, actualIterVarName, actualIterVarName)
		if rangeExpr.Inclusive {
			g.write("<=")
		} else {
			g.write("<")
		}
		g.write(" (")
		g.generateExpr(rangeExpr.End)
		g.write(" - ")
		g.generateExpr(rangeExpr.Start)
		g.write("); %s += ", actualIterVarName)
		// Increment value (default to 1 if not specified)
		if rangeExpr.Incr != nil {
			g.generateExpr(rangeExpr.Incr)
		} else {
			g.write("1")
		}
		g.write(") {\n")
		g.indent++

		// Assign value: v = start + i
		if !isDiscardValue {
			valueType := "int32_t" // Default
			if sym, found := g.mod.CurrentScope.Lookup(valueVarName); found && sym != nil && sym.Type != nil {
				valueType = g.typeToC(sym.Type)
			}
			g.writeIndent()
			g.write("%s %s = ", valueType, g.sanitizeName(valueVarName))
			g.generateExpr(rangeExpr.Start)
			g.write(" + %s;\n", actualIterVarName)
		}
	} else {
		// Single variable: it's the value, iterate from start to end
		g.write("for (%s %s = ", iterType, actualIterVarName)
		g.generateExpr(rangeExpr.Start)
		g.write("; %s ", actualIterVarName)

		// Determine comparison operator based on inclusive flag
		if rangeExpr.Inclusive {
			g.write("<=")
		} else {
			g.write("<")
		}

		g.write(" ")
		g.generateExpr(rangeExpr.End)
		g.write("; %s += ", actualIterVarName)

		// Increment value (default to 1 if not specified)
		if rangeExpr.Incr != nil {
			g.generateExpr(rangeExpr.Incr)
		} else {
			g.write("1")
		}

		g.write(") {\n")
		g.indent++
	}

	// Generate loop body
	if stmt.Body != nil {
		g.generateBlock(stmt.Body)
	}

	g.indent--
	g.writeIndent()
	g.write("}\n")
}

// generateForArray generates C code for iterating over an array
// Ferret: for [let] i, v in array { }
// C: for (int i = 0; i < array_length; i++) { v = array[i]; ... }
func (g *Generator) generateForArray(stmt *ast.ForStmt, arrayType *types.ArrayType) {
	// Extract iterator variable name(s)
	// Iterator is always VarDecl (always binds new variables)
	varDecl, ok := stmt.Iterator.(*ast.VarDecl)
	if !ok {
		g.ctx.ReportError("codegen: iterator must be VarDecl", nil)
		return
	}

	var iterVarName string
	var valueVarName string
	hasValueVar := false

	if len(varDecl.Decls) > 0 {
		iterVarName = varDecl.Decls[0].Name.Name
	}
	if len(varDecl.Decls) > 1 {
		valueVarName = varDecl.Decls[1].Name.Name
		hasValueVar = true
	}

	// Determine semantics: single variable = value (need hidden index), dual = index, value
	isSingleVar := !hasValueVar
	var actualValueVarName string
	var originalIterVarName string
	if isSingleVar {
		// Single variable: it's the value, not the index
		actualValueVarName = iterVarName
		originalIterVarName = iterVarName
		// Generate a hidden index variable
		g.counter++
		iterVarName = fmt.Sprintf("__loop_idx_%d", g.counter)
		hasValueVar = true // Treat as if we have a value variable
		valueVarName = actualValueVarName
	} else {
		originalIterVarName = iterVarName
	}

	// Check if iterator variable is discard (_) - if so, generate a temp variable
	// For single var, check the original name (before we changed it to hidden index)
	isDiscardIter := (originalIterVarName == "_")
	isDiscardValue := (valueVarName == "_")

	// If iterator is discard, generate a unique temp variable name
	actualIterVarName := iterVarName
	if isDiscardIter {
		// If single var and discard, we already have a hidden index, so use it
		if !isSingleVar {
			g.counter++
			actualIterVarName = fmt.Sprintf("__loop_idx_%d", g.counter)
		}
		// For single var discard case, iterVarName is already the hidden index, so use it
	} else if actualIterVarName == "" {
		// Fallback: if iterVarName is empty for some reason, generate a temp
		g.counter++
		actualIterVarName = fmt.Sprintf("__loop_idx_%d", g.counter)
	}

	// Index is always new (hidden for single var, or VarDecl for dual var)
	// Iterator always binds new variables
	iterType := "int32_t" // Index is always i32

	// Generate array length variable name
	g.counter++
	arrayLenVar := fmt.Sprintf("__array_len_%d", g.counter)

	// Get array length and check for empty arrays first
	var arrayLength int = -1

	// Helper to unwrap CastExpr and get CompositeLit
	var compLit *ast.CompositeLit
	if castExpr, ok := stmt.Range.(*ast.CastExpr); ok {
		// Unwrap cast: [] as []i32 -> the CompositeLit is inside the cast
		if cl, ok := castExpr.X.(*ast.CompositeLit); ok {
			compLit = cl
		}
	} else if cl, ok := stmt.Range.(*ast.CompositeLit); ok {
		compLit = cl
	}

	// First, check if it's an array literal (including empty ones)
	if compLit != nil {
		// Array literal: use the number of elements directly
		arrayLength = len(compLit.Elts)
	} else if arrayType.Length >= 0 {
		// Fixed-size array
		arrayLength = arrayType.Length
	} else if ident, ok := stmt.Range.(*ast.IdentifierExpr); ok {
		// Check if we stored the array length for this variable
		if storedLen, found := g.arrayLengths[ident.Name]; found {
			arrayLength = storedLen
		}
	}

	// Handle empty arrays - generate a loop that never executes
	// This handles both compile-time known empty arrays and runtime empty arrays
	if arrayLength == 0 {
		// Empty array - generate loop that never executes: for (int i = 0; i < 0; i++)
		g.writeIndent()
		g.write("for (%s %s = 0; %s < 0; %s++) {\n", iterType, actualIterVarName, actualIterVarName, actualIterVarName)
		g.indent++
		// Enter scope for variable declarations (even though loop won't run)
		if stmt.Scope != nil {
			if forScope, ok := stmt.Scope.(*table.SymbolTable); ok {
				restore := g.mod.EnterScope(forScope)
				defer restore()
			}
		}
		// For empty arrays, we still need to declare the value variable if it exists
		// But we won't assign anything since the loop never runs
		if hasValueVar && !isDiscardValue {
			valueType := g.typeToC(arrayType.Element)
			if sym, found := g.mod.CurrentScope.Lookup(valueVarName); found && sym != nil && sym.Type != nil {
				valueType = g.typeToC(sym.Type)
			}
			// Declare but don't initialize (will never be used since loop doesn't run)
			g.writeIndent()
			g.write("%s %s;\n", valueType, g.sanitizeName(valueVarName))
		}
		// Generate loop body (will never execute, but needed for scope)
		if stmt.Body != nil {
			g.generateBlock(stmt.Body)
		}
		g.indent--
		g.writeIndent()
		g.write("}\n")
		return
	}

	// Generate for loop for non-empty arrays
	if arrayLength >= 0 {
		// We know the length (fixed-size or from literal)
		g.writeIndent()
		// Index is always new (hidden for single var, or VarDecl for dual var)
		g.write("for (%s %s = 0; %s < %d; %s++) {\n", iterType, actualIterVarName, actualIterVarName, arrayLength, actualIterVarName)
		g.indent++
	} else {
		// Variable or expression - try to calculate length
		// For variables, sizeof won't work on pointers, so we need a better solution
		// For now, try sizeof but warn that it may not work for pointers
		g.writeIndent()
		g.write("int %s = sizeof(", arrayLenVar)
		g.generateExpr(stmt.Range)
		g.write(") / sizeof(")
		g.generateExpr(stmt.Range)
		g.write("[0]);\n")
		// Now generate the for loop
		g.writeIndent()
		// Index is always new
		g.write("for (%s %s = 0; %s < %s; %s++) {\n", iterType, actualIterVarName, actualIterVarName, arrayLenVar, actualIterVarName)
		g.indent++
	}

	// Enter for loop scope
	if stmt.Scope != nil {
		if forScope, ok := stmt.Scope.(*table.SymbolTable); ok {
			restore := g.mod.EnterScope(forScope)
			defer restore()
		}
	}

	// If we have a value variable and it's not discard, assign array[i] to it
	if hasValueVar && !isDiscardValue {
		// Value variable is always new (always VarDecl now, always binds new variables)
		valueType := g.typeToC(arrayType.Element)
		// Look up the value variable type
		if sym, found := g.mod.CurrentScope.Lookup(valueVarName); found && sym != nil && sym.Type != nil {
			valueType = g.typeToC(sym.Type)
		}

		g.writeIndent()
		g.write("%s %s = ", valueType, g.sanitizeName(valueVarName))
		g.generateExpr(stmt.Range)
		g.write("[%s];\n", actualIterVarName)
	}

	// Generate loop body
	if stmt.Body != nil {
		g.generateBlock(stmt.Body)
	}

	g.indent--
	g.writeIndent()
	g.write("}\n")
}

// generateForMap generates C code for iterating over a map
// Ferret: for [let] k, v in map { }
// C: (simplified - would need proper map implementation)
func (g *Generator) generateForMap(stmt *ast.ForStmt, mapType *types.MapType) {
	// Iterator is always VarDecl (binds new variables)
	varDecl, ok := stmt.Iterator.(*ast.VarDecl)
	if !ok {
		g.ctx.ReportError("codegen: iterator must be VarDecl", nil)
		g.writeIndent()
		g.write("/* invalid iterator type */\n")
		return
	}

	var keyVarName string
	var valueVarName string
	hasValueVar := false
	if len(varDecl.Decls) > 0 {
		keyVarName = varDecl.Decls[0].Name.Name
	}
	if len(varDecl.Decls) > 1 {
		valueVarName = varDecl.Decls[1].Name.Name
		hasValueVar = true
	}

	isDiscardKey := keyVarName == "_"
	isDiscardValue := valueVarName == "_"

	g.counter++
	mapTemp := fmt.Sprintf("__map_iter_%d", g.counter)

	keyCType := g.typeToC(mapType.Key)
	valCType := g.typeToC(mapType.Value)

	// Scope the loop variables to avoid leaking
	g.writeIndent()
	g.write("{\n")
	g.indent++

	g.writeIndent()
	g.write("ferret_map_t* %s = ", mapTemp)
	g.generateExpr(stmt.Range)
	g.write(";\n")

	g.writeIndent()
	g.write("ferret_map_iter_t __it;\n")
	g.writeIndent()
	g.write("void* __it_key;\n")
	g.writeIndent()
	g.write("void* __it_val;\n")

	g.writeIndent()
	g.write("if (%s && ferret_map_iter_begin(%s, &__it) && ferret_map_iter_next(%s, &__it, &__it_key, &__it_val)) {\n", mapTemp, mapTemp, mapTemp)
	g.indent++
	g.writeIndent()
	g.write("do {\n")
	g.indent++

	// Bind key/value variables if not discarded
	if !isDiscardKey && keyVarName != "" {
		g.writeIndent()
		g.write("%s %s = *(%s*)__it_key;\n", keyCType, g.sanitizeName(keyVarName), keyCType)
	}
	if hasValueVar && !isDiscardValue {
		g.writeIndent()
		g.write("%s %s = *(%s*)__it_val;\n", valCType, g.sanitizeName(valueVarName), valCType)
	}

	// Generate loop body
	if stmt.Body != nil {
		g.generateBlock(stmt.Body)
	}

	g.writeIndent()
	g.write("} while (ferret_map_iter_next(%s, &__it, &__it_key, &__it_val));\n", mapTemp)
	g.indent--
	g.writeIndent()
	g.write("}\n")

	g.indent--
	g.writeIndent()
	g.write("}\n")
}

func (g *Generator) generateWhileStmt(stmt *ast.WhileStmt) {
	// Ferret: while [cond] { }
	// C: while (cond) { } or for (;;) { } for infinite loops

	if stmt.Cond == nil {
		// Infinite loop: while { } -> for (;;) { }
		g.writeIndent()
		g.write("for (;;) {\n")
	} else {
		// Regular while loop: while (cond) { }
		g.writeIndent()
		g.write("while (")
		g.generateExpr(stmt.Cond)
		g.write(") {\n")
	}

	g.indent++

	// Generate loop body
	if stmt.Body != nil {
		g.generateBlock(stmt.Body)
	}

	g.indent--
	g.writeIndent()
	g.write("}\n")
}

func (g *Generator) generateExpr(expr ast.Expression) {
	switch e := expr.(type) {
	case *ast.BasicLit:
		g.generateLiteral(e)
	case *ast.IdentifierExpr:
		g.generateIdentifierExpr(e)
	case *ast.BinaryExpr:
		g.generateBinaryExpr(e)
	case *ast.UnaryExpr:
		g.generateUnaryExpr(e)
	case *ast.PostfixExpr:
		g.generatePostfixExpr(e)
	case *ast.CallExpr:
		g.generateCallExpr(e)
	case *ast.ParenExpr:
		g.write("(")
		g.generateExpr(e.X)
		g.write(")")
	case *ast.ScopeResolutionExpr:
		g.generateScopeResolutionExpr(e)
	case *ast.SelectorExpr:
		g.generateSelectorExpr(e)
	case *ast.IndexExpr:
		g.generateIndexExpr(e)
	case *ast.CastExpr:
		g.generateCastExpr(e)
	case *ast.CompositeLit:
		g.generateCompositeLit(e)
	case *ast.CoalescingExpr:
		g.generateCoalescingExpr(e)
	case *ast.RangeExpr:
		// RangeExpr as standalone expression (e.g., let arr := 0..10)
		// For now, we'll generate a placeholder - ranges in assignments might need array generation
		// TODO: Support range expressions that generate arrays
		g.ctx.ReportError("codegen: range expressions as values not yet fully supported", nil)
		g.write("/* range: ")
		g.generateExpr(e.Start)
		g.write("..")
		if e.Inclusive {
			g.write("=")
		}
		g.generateExpr(e.End)
		if e.Incr != nil {
			g.write(":")
			g.generateExpr(e.Incr)
		}
		g.write(" */")
	default:
		// Unsupported expression type
		g.ctx.ReportError(fmt.Sprintf("codegen: unsupported expression type %T (not yet implemented)", e), nil)
		g.write("/* unsupported: %T */", e)
	}
}

func (g *Generator) generateLiteral(lit *ast.BasicLit) {
	switch lit.Kind {
	case ast.INT:
		g.write("%s", lit.Value)
	case ast.FLOAT:
		g.write("%s", lit.Value)
	case ast.STRING:
		// Escape string for C
		str := strings.ReplaceAll(lit.Value, `"`, `\"`)
		str = strings.ReplaceAll(str, "\n", "\\n")
		g.write(`"%s"`, str)
	case ast.BOOL:
		if lit.Value == "true" {
			g.write("1")
		} else {
			g.write("0")
		}
	case ast.NONE:
		// Generate none constant - use the none constant defined for optional types
		// The none constant is defined for each optional type in the header file
		g.write("none")
	}
}

func (g *Generator) generateBinaryExpr(expr *ast.BinaryExpr) {
	// Handle string concatenation with + operator
	if expr.Op.Value == "+" {
		// Check if both operands are strings (or string expressions)
		xType := g.inferExprType(expr.X)
		yType := g.inferExprType(expr.Y)

		// Check if X is already a string concatenation (BinaryExpr with +)
		xIsConcat := false
		if binExpr, ok := expr.X.(*ast.BinaryExpr); ok && binExpr.Op.Value == "+" {
			xLeftType := g.inferExprType(binExpr.X)
			xRightType := g.inferExprType(binExpr.Y)
			if g.isStringType(xLeftType) && g.isStringType(xRightType) {
				xIsConcat = true
			}
		}

		if (g.isStringType(xType) || xIsConcat) && g.isStringType(yType) {
			// String concatenation: use runtime helper function
			// Handle chained concatenation by nesting ConcatStrings calls
			g.write("ferret_io_ConcatStrings(")
			if xIsConcat {
				// Left side is already a concatenation - wrap it in another ConcatStrings
				g.write("ferret_io_ConcatStrings(")
				if binExpr, ok := expr.X.(*ast.BinaryExpr); ok {
					g.generateExpr(binExpr.X)
					g.write(", ")
					g.generateExpr(binExpr.Y)
				} else {
					g.generateExpr(expr.X)
				}
				g.write(")")
			} else {
				g.generateExpr(expr.X)
			}
			g.write(", ")
			g.generateExpr(expr.Y)
			g.write(")")
			return
		}
	}

	// Handle power operator ** (needs pow() from math.h)
	if expr.Op.Value == "**" {
		// Use pow() function from math.h - returns double
		// Cast to appropriate type based on operands
		xType := g.inferExprType(expr.X)
		yType := g.inferExprType(expr.Y)

		// If both are integers, cast result to integer type
		if g.isIntegerType(xType) && g.isIntegerType(yType) {
			g.write("(int32_t)pow((double)(")
			g.generateExpr(expr.X)
			g.write("), (double)(")
			g.generateExpr(expr.Y)
			g.write("))")
		} else {
			// At least one is float, result is double
			g.write("pow((double)(")
			g.generateExpr(expr.X)
			g.write("), (double)(")
			g.generateExpr(expr.Y)
			g.write("))")
		}
		return
	}

	// Handle none comparisons (== none, != none)
	if expr.Op.Kind == tokens.DOUBLE_EQUAL_TOKEN || expr.Op.Kind == tokens.NOT_EQUAL_TOKEN {
		xType := g.inferExprType(expr.X)
		yType := g.inferExprType(expr.Y)

		// Check if comparing with none
		var optType *types.OptionalType
		var isNoneComparison bool

		if xType != nil {
			if opt, ok := xType.(*types.OptionalType); ok {
				optType = opt
				isNoneComparison = true
			} else if named, ok := xType.(*types.NamedType); ok {
				if opt, ok := named.Underlying.(*types.OptionalType); ok {
					optType = opt
					isNoneComparison = true
				}
			}
		}
		if !isNoneComparison && yType != nil {
			if opt, ok := yType.(*types.OptionalType); ok {
				optType = opt
				isNoneComparison = true
			} else if named, ok := yType.(*types.NamedType); ok {
				if opt, ok := named.Underlying.(*types.OptionalType); ok {
					optType = opt
					isNoneComparison = true
				}
			}
		}

		// Check if one side is a none literal (check both AST node and type)
		var noneSide ast.Expression
		var otherSide ast.Expression
		if lit, ok := expr.X.(*ast.BasicLit); ok && lit.Kind == ast.NONE {
			noneSide = expr.X
			otherSide = expr.Y
			isNoneComparison = true
		} else if xType != nil && xType.Equals(types.TypeNone) {
			// Check if type is TypeNone
			noneSide = expr.X
			otherSide = expr.Y
			isNoneComparison = true
		} else if lit, ok := expr.Y.(*ast.BasicLit); ok && lit.Kind == ast.NONE {
			noneSide = expr.Y
			otherSide = expr.X
			isNoneComparison = true
		} else if yType != nil && yType.Equals(types.TypeNone) {
			// Check if type is TypeNone
			noneSide = expr.Y
			otherSide = expr.X
			isNoneComparison = true
		}

		if isNoneComparison {
			// Generate: opt.is_some == 0 (for == none) or opt.is_some != 0 (for != none)
			if noneSide != nil {
				// Comparing with none literal - check is_some field
				// For identifier expressions, we need the variable name itself (not .value)
				// because we're comparing the optional, not the narrowed value
				if ident, ok := otherSide.(*ast.IdentifierExpr); ok {
					// Use the variable name directly for comparison
					varName := g.sanitizeName(ident.Name)
					if expr.Op.Kind == tokens.DOUBLE_EQUAL_TOKEN {
						g.write("%s.is_some == 0", varName)
					} else {
						g.write("%s.is_some != 0", varName)
					}
				} else {
					// For non-identifier expressions, generate normally and add .is_some
					if expr.Op.Kind == tokens.DOUBLE_EQUAL_TOKEN {
						g.generateExpr(otherSide)
						g.write(".is_some == 0")
					} else {
						g.generateExpr(otherSide)
						g.write(".is_some != 0")
					}
				}
				return
			} else if optType != nil {
				// Both sides are optionals - compare is_some fields
				// Check if one side is none constant
				if lit, ok := expr.X.(*ast.BasicLit); ok && lit.Kind == ast.NONE {
					// X is none, Y is optional - compare Y.is_some with 0
					if expr.Op.Kind == tokens.DOUBLE_EQUAL_TOKEN {
						g.generateExpr(expr.Y)
						g.write(".is_some == 0")
					} else {
						g.generateExpr(expr.Y)
						g.write(".is_some != 0")
					}
				} else if lit, ok := expr.Y.(*ast.BasicLit); ok && lit.Kind == ast.NONE {
					// Y is none, X is optional - compare X.is_some with 0
					if expr.Op.Kind == tokens.DOUBLE_EQUAL_TOKEN {
						g.generateExpr(expr.X)
						g.write(".is_some == 0")
					} else {
						g.generateExpr(expr.X)
						g.write(".is_some != 0")
					}
				} else {
					// Both sides are optionals - compare is_some fields
					g.generateExpr(expr.X)
					g.write(".is_some")
					g.write(" %s ", expr.Op.Value)
					g.generateExpr(expr.Y)
					g.write(".is_some")
				}
				return
			}
		}
	}

	// Regular binary expression (numeric operations: +, -, *, /, %, comparisons: >, <, <=, >=, ==, !=)
	g.generateExpr(expr.X)
	g.write(" %s ", expr.Op.Value)
	g.generateExpr(expr.Y)
}

// generateIdentifierExpr generates code for an identifier expression.
// Handles type narrowing: if a variable was originally optional but has been narrowed
// to the inner type, it generates `.value` to unwrap the optional.
func (g *Generator) generateIdentifierExpr(expr *ast.IdentifierExpr) {
	varName := g.sanitizeName(expr.Name)

	// Get the type from the expression (may be narrowed)
	exprType := g.inferExprType(expr)

	// Get the original declared type from the symbol table
	// The symbol table should have the original type (not the narrowed one)
	var originalType types.SemType
	if sym, ok := g.mod.CurrentScope.Lookup(expr.Name); ok && sym != nil && sym.Type != nil {
		originalType = sym.Type
	}

	// Check if we need to unwrap an optional
	// If the original type is optional but the current (narrowed) type is not,
	// we need to access .value to unwrap it
	if originalType != nil && exprType != nil && !exprType.Equals(types.TypeUnknown) {
		// Check if original type is optional
		var originalOptType *types.OptionalType
		if opt, ok := originalType.(*types.OptionalType); ok {
			originalOptType = opt
		} else if named, ok := originalType.(*types.NamedType); ok {
			if opt, ok := named.Underlying.(*types.OptionalType); ok {
				originalOptType = opt
			}
		}

		// If original is optional, check if current type is the inner type (narrowed)
		if originalOptType != nil {
			// Check if the current type is not optional (has been narrowed)
			// Unwrap NamedType to compare with inner type
			unwrappedExprType := types.UnwrapType(exprType)
			unwrappedInnerType := types.UnwrapType(originalOptType.Inner)

			// If it's been narrowed to the inner type, we need to unwrap
			if unwrappedExprType.Equals(unwrappedInnerType) {
				// Variable has been narrowed from T? to T, need to unwrap
				g.write("%s.value", varName)
				return
			}
		}
	}

	// No narrowing or not an optional - just write the variable name
	g.write("%s", varName)
}

// inferExprType gets the type of an expression.
func (g *Generator) inferExprType(expr ast.Expression) types.SemType {
	return typechecker.ResolvedExprType(g.ctx, g.mod, expr)
}

// isStringType checks if a type is string
func (g *Generator) isStringType(typ types.SemType) bool {
	if typ == nil {
		return false
	}
	// Unwrap NamedType to check underlying type
	unwrapped := types.UnwrapType(typ)
	if prim, ok := unwrapped.(*types.PrimitiveType); ok {
		return prim.GetName() == types.TYPE_STRING
	}
	return false
}

// isIntegerType checks if a type is an integer type
func (g *Generator) isIntegerType(typ types.SemType) bool {
	if typ == nil {
		return false
	}
	// Unwrap NamedType to check underlying type
	unwrapped := types.UnwrapType(typ)
	if prim, ok := unwrapped.(*types.PrimitiveType); ok {
		name := prim.GetName()
		return name == types.TYPE_I8 || name == types.TYPE_I16 || name == types.TYPE_I32 || name == types.TYPE_I64 ||
			name == types.TYPE_U8 || name == types.TYPE_U16 || name == types.TYPE_U32 || name == types.TYPE_U64
	}
	return false
}

func (g *Generator) generateUnaryExpr(expr *ast.UnaryExpr) {
	g.write("%s", expr.Op.Value)
	g.generateExpr(expr.X)
}

func (g *Generator) generatePostfixExpr(expr *ast.PostfixExpr) {
	g.generateExpr(expr.X)
	g.write("%s", expr.Op.Value)
}

func (g *Generator) generateScopeResolutionExpr(expr *ast.ScopeResolutionExpr) {
	// Handle module::symbol (for constants, variables, etc.)
	if moduleIdent, ok := expr.X.(*ast.IdentifierExpr); ok {
		// Check if it's an imported module
		importPath, ok := g.mod.ImportAliasMap[moduleIdent.Name]
		if ok {
			// Look up the imported module
			importedMod, exists := g.ctx.GetModule(importPath)
			if exists {
				sym, found := importedMod.ModuleScope.GetSymbol(expr.Selector.Name)
				if found && sym != nil {
					// Generate the symbol name with module prefix
					modulePrefix := strings.ReplaceAll(strings.ReplaceAll(importPath, "/", "_"), "-", "_")
					symbolName := g.sanitizeName(modulePrefix) + "_" + g.sanitizeName(expr.Selector.Name)

					// For constants, use the prefixed name
					if sym.Kind == symbols.SymbolConstant {
						g.write("%s", symbolName)
						return
					}
					// For variables, also use prefixed name
					if sym.Kind == symbols.SymbolVariable {
						g.write("%s", symbolName)
						return
					}
					// For functions, this shouldn't happen here (should be in CallExpr)
					// But handle it anyway
					if sym.Kind == symbols.SymbolFunction {
						g.write("%s", symbolName)
						return
					}
				}
			}
		}
	}
	// Fallback: generate error comment
	g.ctx.ReportError(fmt.Sprintf("codegen: unsupported scope resolution"), nil)
	g.write("/* error: scope resolution */")
}

func (g *Generator) generateSelectorExpr(expr *ast.SelectorExpr) {
	// SelectorExpr: obj.field or obj.method
	// For field access: obj.field -> obj.field in C (or obj->field if obj is a pointer)
	// For method calls: handled in CallExpr with SelectorExpr as Fun

	// Check if the base expression is a pointer type
	baseType := g.inferExprType(expr.X)
	isPointer := false
	if baseType != nil && !baseType.Equals(types.TypeUnknown) {
		// Check if it's a reference type or pointer type
		if _, ok := baseType.(*types.ReferenceType); ok {
			isPointer = true
		}
	}

	// Generate the object expression
	g.generateExpr(expr.X)

	// Use -> for pointers, . for values
	if isPointer {
		g.write("->")
	} else {
		g.write(".")
	}

	// Generate the field name
	if expr.Field != nil {
		g.write("%s", g.sanitizeName(expr.Field.Name))
	} else {
		g.write("/* unknown field */")
	}
}

func (g *Generator) generateIndexExpr(expr *ast.IndexExpr) {
	// IndexExpr: array[index] or map[key]
	baseType := types.UnwrapType(g.inferExprType(expr.X))
	if mapType, ok := baseType.(*types.MapType); ok {
		// Map indexing returns optional value type
		g.trackOptional(mapType.Value)
		g.counter++
		tmpMap := fmt.Sprintf("__map_tmp_%d", g.counter)
		g.counter++
		tmpKey := fmt.Sprintf("__map_key_%d", g.counter)
		optTypeName := fmt.Sprintf("ferret_optional_%s", g.getOptionalTypeName(mapType.Value))
		keyCType := g.typeToC(mapType.Key)
		valueCType := g.typeToC(mapType.Value)

		// Use GNU statement expression for single-eval of map/key
		g.write("({ ferret_map_t* %s = ", tmpMap)
		g.generateExpr(expr.X)
		g.write("; %s %s = ", keyCType, tmpKey)
		g.generateExpr(expr.Index)
		g.write("; FERRET_MAP_GET_OPTIONAL(%s, &%s, %s, %s); })", tmpMap, tmpKey, optTypeName, valueCType)
		return
	}

	// Array indexing
	g.generateExpr(expr.X)
	g.write("[")
	g.generateExpr(expr.Index)
	g.write("]")
}

func (g *Generator) generateCastExpr(expr *ast.CastExpr) {
	// CastExpr: expr as Type
	// Generate: (Type)(expr)
	targetType := g.typeFromTypeNode(expr.Type)
	if targetType == nil || targetType.Equals(types.TypeUnknown) {
		g.ctx.ReportError("codegen: cannot determine cast target type", expr.Loc())
		g.write("/* error: unknown cast type */")
		return
	}

	cType := g.typeToC(targetType)

	// If the expression being cast is a CompositeLit, generate it with the target type directly
	// This avoids double casts like (Point)((Point){...}) -> (Point){...}
	if compLit, ok := expr.X.(*ast.CompositeLit); ok {
		// For named types, generateCompositeLitWithType already includes the cast
		// For anonymous structs, we need to add the cast prefix
		if _, ok := targetType.(*types.NamedType); ok {
			// Named type: generateCompositeLitWithType will produce (TypeName){...}
			// So we just call it without adding another cast prefix
			g.generateCompositeLitWithType(compLit, targetType, false)
		} else {
			// Anonymous struct or other type: need cast prefix
			g.write("(%s)", cType)
			g.generateCompositeLitWithType(compLit, targetType, false)
		}
		return
	}

	// For other expressions, use normal cast syntax
	g.write("(%s)(", cType)
	g.generateExpr(expr.X)
	g.write(")")
}

// generateCoalescingExpr generates code for the coalescing operator (a ?? b)
// If a is none (is_some == 0), returns b, otherwise returns a.value
func (g *Generator) generateCoalescingExpr(expr *ast.CoalescingExpr) {
	// Get the type of the condition (should be optional)
	condType := g.inferExprType(expr.Cond)

	// Check if condition is an optional type
	if _, ok := condType.(*types.OptionalType); ok {
		g.trackOptional(condType.(*types.OptionalType).Inner)
		// Generate: (cond.is_some ? cond.value : default)
		g.write("(")
		g.generateExpr(expr.Cond)
		g.write(".is_some ? ")
		g.generateExpr(expr.Cond)
		g.write(".value : ")
		g.generateExpr(expr.Default)
		g.write(")")
	} else if named, ok := condType.(*types.NamedType); ok {
		// Check if underlying is optional
		if _, ok := named.Underlying.(*types.OptionalType); ok {
			if opt, ok := named.Underlying.(*types.OptionalType); ok {
				g.trackOptional(opt.Inner)
			}
			g.write("(")
			g.generateExpr(expr.Cond)
			g.write(".is_some ? ")
			g.generateExpr(expr.Cond)
			g.write(".value : ")
			g.generateExpr(expr.Default)
			g.write(")")
		} else {
			// Not an optional - this shouldn't happen if type checking worked
			g.ctx.ReportError("codegen: coalescing operator requires optional type", nil)
			g.write("/* error: not optional */")
		}
	} else {
		// Fallback: assume it's an optional and generate the code anyway
		// Type checker should have caught this, but be defensive
		g.write("(")
		g.generateExpr(expr.Cond)
		g.write(".is_some ? ")
		g.generateExpr(expr.Cond)
		g.write(".value : ")
		g.generateExpr(expr.Default)
		g.write(")")
	}
}

func (g *Generator) generateCompositeLit(expr *ast.CompositeLit) {
	// CompositeLit: struct literals, array literals, map literals
	// Examples:
	//   - Struct: Point{.x = 1, .y = 2}
	//   - Array:  []i32{1, 2, 3}
	//   - Map:    map[str]i32{"a" => 1, "b" => 2}

	// Get the type of the composite literal
	// First try to get type from Type field if present (explicit type annotation or from cast)
	var litType types.SemType
	if expr.Type != nil {
		litType = g.typeFromTypeNode(expr.Type)
		if litType != nil && !litType.Equals(types.TypeUnknown) {
			// Use the explicit type - continue to generate with this type
		} else {
			// Type field exists but couldn't resolve - try to infer
			litType = g.inferExprType(expr)
		}
	} else {
		// Try to infer from the expression itself
		litType = g.inferExprType(expr)
	}

	// If still unknown, try to get type from parent context (e.g., cast expression)
	// This handles cases like: {.x = 1, .y = 2} as Point
	// The typechecker should have resolved this, but if not, we try to infer from context
	if litType == nil || litType.Equals(types.TypeUnknown) {
		// Try to infer from context - look for struct fields (KeyValueExpr with IdentifierExpr key)
		// This is a heuristic for struct literals
		if len(expr.Elts) > 0 {
			if kv, ok := expr.Elts[0].(*ast.KeyValueExpr); ok {
				if fieldIdent, ok := kv.Key.(*ast.IdentifierExpr); ok {
					// This looks like a struct literal - try to find the type from the field name
					// For now, report error with more context
					g.ctx.ReportError(fmt.Sprintf("codegen: cannot determine type of composite literal (struct field: %s)", fieldIdent.Name), expr.Loc())
					g.write("/* error: unknown composite literal type */")
					return
				}
			}
		}
		g.ctx.ReportError("codegen: cannot determine type of composite literal", expr.Loc())
		g.write("/* error: unknown composite literal type */")
		return
	}

	// Generate the composite literal with the inferred type
	// For standalone expressions, use cast for anonymous structs
	g.generateCompositeLitWithType(expr, litType, false)
}

func (g *Generator) generateCompositeLitWithType(expr *ast.CompositeLit, litType types.SemType, isInitializer bool) {
	// Check if it's a struct type
	if namedType, ok := litType.(*types.NamedType); ok {
		if _, ok := namedType.Underlying.(*types.StructType); ok {
			// Struct literal: Point{.x = 1, .y = 2}
			cTypeName := g.sanitizeName(namedType.Name)
			g.write("(%s){", cTypeName)

			// Generate field initializers
			for i, elt := range expr.Elts {
				if i > 0 {
					g.write(", ")
				}
				if kv, ok := elt.(*ast.KeyValueExpr); ok {
					// KeyValueExpr: .field = value (for structs) or key => value (for maps)
					if fieldIdent, ok := kv.Key.(*ast.IdentifierExpr); ok {
						fieldName := fieldIdent.Name
						g.write(".%s = ", g.sanitizeName(fieldName))
						g.generateExpr(kv.Value)
					} else {
						// Map entry: key => value
						g.generateExpr(kv.Key)
						g.write(" => ")
						g.generateExpr(kv.Value)
					}
				} else {
					// Plain expression (for array literals)
					g.generateExpr(elt)
				}
			}

			g.write("}")
			return
		}
	}

	// Check if it's an anonymous struct type (direct StructType, not wrapped in NamedType)
	if structType, ok := litType.(*types.StructType); ok {
		// Anonymous struct literal: {.X = 3, .Y = 4}
		// For variable initialization, don't use cast (C allows direct initialization)
		// For expressions, use cast with compound literal syntax
		if !isInitializer {
			g.write("(")
			g.write("%s", g.structTypeToC(structType))
			g.write(")")
		}
		g.write("{")

		// Generate field initializers
		for i, elt := range expr.Elts {
			if i > 0 {
				g.write(", ")
			}
			if kv, ok := elt.(*ast.KeyValueExpr); ok {
				if fieldIdent, ok := kv.Key.(*ast.IdentifierExpr); ok {
					fieldName := fieldIdent.Name
					g.write(".%s = ", g.sanitizeName(fieldName))
					g.generateExpr(kv.Value)
				} else {
					// Should not happen for struct literals, but handle gracefully
					g.generateExpr(kv.Key)
					g.write(" => ")
					g.generateExpr(kv.Value)
				}
			} else {
				// Plain expression (should not happen for structs, but handle gracefully)
				g.generateExpr(elt)
			}
		}

		g.write("}")
		return
	}

	// Check if it's an array type
	if arrayType, ok := litType.(*types.ArrayType); ok {
		// Array literal: []i32{1, 2, 3} or [5]i32{1, 2, 3}
		elType := g.typeToC(arrayType.Element)

		if arrayType.Length >= 0 {
			// Fixed-size array: [5]i32{1, 2, 3}
			g.write("(%s[%d]){", elType, arrayType.Length)
		} else {
			// Dynamic array: []str{"apple", "banana"}
			// For dynamic arrays, we need to create an array with the number of elements
			// C doesn't support variable-length array literals, so we use a compound literal with explicit size
			numElements := len(expr.Elts)
			g.write("(%s[%d]){", elType, numElements)
		}

		// Generate elements
		for i, elt := range expr.Elts {
			if i > 0 {
				g.write(", ")
			}
			g.generateExpr(elt)
		}

		g.write("}")
		return
	}

	// Check if it's a map type
	if mapType, ok := litType.(*types.MapType); ok {
		g.trackOptional(mapType.Value)
		hashFn, eqFn := g.mapHashEqualsForType(mapType.Key)
		keyCType := g.typeToC(mapType.Key)
		valCType := g.typeToC(mapType.Value)
		keySize := g.cSizeOfType(mapType.Key)
		valSize := g.cSizeOfType(mapType.Value)

		if len(expr.Elts) == 0 {
			// Empty map literal
			g.write("ferret_map_new(%s, %s, %s, %s)", keySize, valSize, hashFn, eqFn)
			return
		}

		// Non-empty map literal
		g.write("ferret_map_from_pairs(%s, %s, (const %s[]){", keySize, valSize, keyCType)
		for i, elt := range expr.Elts {
			if i > 0 {
				g.write(", ")
			}
			if kv, ok := elt.(*ast.KeyValueExpr); ok {
				g.generateExpr(kv.Key)
			} else {
				g.ctx.ReportError("codegen: expected key => value in map literal", expr.Loc())
				g.write("0")
			}
		}
		g.write("}, (const %s[]){", valCType)
		for i, elt := range expr.Elts {
			if i > 0 {
				g.write(", ")
			}
			if kv, ok := elt.(*ast.KeyValueExpr); ok {
				g.generateExpr(kv.Value)
			} else {
				g.write("0")
			}
		}
		g.write("}, %d, %s, %s)", len(expr.Elts), hashFn, eqFn)
		return
	}

	// Unknown composite literal type
	g.ctx.ReportError("codegen: unsupported composite literal type", expr.Loc())
	g.write("/* unsupported composite literal */")
}

func (g *Generator) generateCallExpr(expr *ast.CallExpr) {
	// Handle scope resolution first (module::function)
	if scopeRes, ok := expr.Fun.(*ast.ScopeResolutionExpr); ok {
		if moduleIdent, ok := scopeRes.X.(*ast.IdentifierExpr); ok {
			// Check if it's a native module function
			importPath, ok := g.mod.ImportAliasMap[moduleIdent.Name]
			if ok {
				// Look up the imported module
				importedMod, exists := g.ctx.GetModule(importPath)
				if exists {
					sym, found := importedMod.ModuleScope.GetSymbol(scopeRes.Selector.Name)
					if found && sym != nil {
						if sym.IsNative {
							// Native function call - special handling for Print/Println
							funcName := scopeRes.Selector.Name
							if funcName == "Print" || funcName == "Println" {
								if len(expr.Args) == 0 {
									// No arguments - just print newline for Println
									if funcName == "Println" {
										g.writeIndent()
										g.write("ferret_io_Println(\"\");\n")
									}
									return
								} else if len(expr.Args) == 1 {
									// Single argument - use type-specific function
									// Check if argument is optional and unwrap it
									argType := g.inferExprType(expr.Args[0])
									g.writeIndent()
									if optType, ok := argType.(*types.OptionalType); ok {
										// Optional type - unwrap it for printing
										innerType := optType.Inner
										// Generate code to unwrap and print
										g.write("if (")
										g.generateExpr(expr.Args[0])
										g.write(".is_some) { ")
										// Print the value
										g.write("%s(", g.getPrintFunctionNameForType(funcName, innerType))
										g.generateExpr(expr.Args[0])
										g.write(".value); } else { ferret_io_%s(\"<none>\"); }\n", funcName)
									} else if named, ok := argType.(*types.NamedType); ok {
										if optType, ok := named.Underlying.(*types.OptionalType); ok {
											innerType := optType.Inner
											g.write("if (")
											g.generateExpr(expr.Args[0])
											g.write(".is_some) { ")
											g.write("%s(", g.getPrintFunctionNameForType(funcName, innerType))
											g.generateExpr(expr.Args[0])
											g.write(".value); } else { ferret_io_%s(\"<none>\"); }\n", funcName)
										} else {
											g.write("%s(", g.getPrintFunctionName(funcName, expr.Args[0]))
											g.generateExpr(expr.Args[0])
											g.write(");\n")
										}
									} else {
										g.write("%s(", g.getPrintFunctionName(funcName, expr.Args[0]))
										g.generateExpr(expr.Args[0])
										g.write(");\n")
									}
									return
								} else {
									// Multiple arguments - format and concatenate
									g.generatePrintWithMultipleArgs(funcName, expr.Args)
									return
								}
							} else {
								g.write("%s(", sym.NativeName)
							}
						} else if sym.Kind == symbols.SymbolFunction {
							// Regular Ferret function from imported module
							// Generate function name with module prefix to avoid conflicts
							// Use import path to create unique function name
							modulePrefix := strings.ReplaceAll(strings.ReplaceAll(importPath, "/", "_"), "-", "_")
							funcName := g.sanitizeName(modulePrefix) + "_" + g.sanitizeName(scopeRes.Selector.Name)
							g.write("%s(", funcName)
						} else {
							// Not a function, can't call it
							g.ctx.ReportError(fmt.Sprintf("codegen: '%s::%s' is not a function", moduleIdent.Name, scopeRes.Selector.Name), nil)
							g.write("/* error: not a function */")
							return
						}
						for i, arg := range expr.Args {
							if i > 0 {
								g.write(", ")
							}
							g.generateExpr(arg)
						}
						g.write(")")
						return
					}
				}
			}
		}
		// Fallback: regular scope resolution (not implemented fully yet)
		g.write("/* scope resolution: ")
		g.generateExpr(scopeRes.X)
		g.write("::")
		g.generateExpr(scopeRes.Selector)
		g.write(" */")
		return
	}

	// Handle method calls: obj.method(...) -> TypeName_method(obj, ...)
	if selector, ok := expr.Fun.(*ast.SelectorExpr); ok {
		// Get the type of the object
		objType := g.inferExprType(selector.X)
		if objType == nil || objType.Equals(types.TypeUnknown) {
			g.ctx.ReportError("codegen: cannot determine type for method call", nil)
			g.write("/* error: method call */")
			return
		}

		// Get the method name
		methodName := selector.Field.Name

		// Check if calling method on interface - use vtable dispatch
		if g.isInterfaceType(objType) {
			g.generateInterfaceMethodCall(selector.X, methodName, expr.Args)
			return
		}

		// Find the type name and method
		// Handle reference types: &Point -> Point
		var typeName string
		var funcType *types.FunctionType
		var needsAddressOf bool

		var namedType *types.NamedType
		if refType, ok := objType.(*types.ReferenceType); ok {
			if nt, ok := refType.Inner.(*types.NamedType); ok {
				namedType = nt
			}
		} else if nt, ok := objType.(*types.NamedType); ok {
			namedType = nt
		}

		if namedType != nil {
			typeName = namedType.Name
			if typeSym, found := g.mod.ModuleScope.GetSymbol(typeName); found && typeSym != nil {
				if typeSym.Methods != nil {
					if methodInfo, exists := typeSym.Methods[methodName]; exists && methodInfo != nil {
						funcType = methodInfo.FuncType
						// Find the method declaration in AST to check receiver type
						if g.mod.AST != nil {
							for _, node := range g.mod.AST.Nodes {
								if methodDecl, ok := node.(*ast.MethodDecl); ok {
									if methodDecl.Name != nil && methodDecl.Name.Name == methodName {
										if methodDecl.Receiver != nil && methodDecl.Receiver.Type != nil {
											// Check if receiver type is a reference type
											if _, ok := methodDecl.Receiver.Type.(*ast.ReferenceType); ok {
												// Method expects &T receiver, and we're calling on value type T
												// So we need to pass &obj
												if _, ok := objType.(*types.NamedType); ok {
													needsAddressOf = true
													break
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}

		if funcType == nil {
			g.ctx.ReportError(fmt.Sprintf("codegen: method '%s' not found", methodName), nil)
			g.write("/* error: method not found */")
			return
		}

		// Generate method call: TypeName_methodName(obj, args...)
		cFuncName := g.sanitizeName(typeName) + "_" + g.sanitizeName(methodName)
		g.write("%s(", cFuncName)

		// First argument is the receiver (the object)
		// If needsAddressOf is true, pass &obj (for methods with &T receiver called on value)
		if needsAddressOf {
			g.write("&")
		}
		g.generateExpr(selector.X)

		// Generate other arguments
		for i, arg := range expr.Args {
			g.write(", ")
			g.generateExpr(arg)
			_ = i // Avoid unused variable warning
		}

		g.write(")")
		return
	}

	// Check if it's a native function call (local scope)
	if ident, ok := expr.Fun.(*ast.IdentifierExpr); ok {
		sym, ok := g.mod.CurrentScope.Lookup(ident.Name)
		if ok && sym != nil && sym.IsNative {
			// Native function call - special handling for Print/Println
			funcName := ident.Name
			if funcName == "Print" || funcName == "Println" {
				if len(expr.Args) == 0 {
					// No arguments - just print newline for Println
					if funcName == "Println" {
						g.writeIndent()
						g.write("ferret_io_Println(\"\");\n")
					}
					return
				} else if len(expr.Args) == 1 {
					// Single argument - use type-specific function
					// Check if argument is optional and unwrap it
					argType := g.inferExprType(expr.Args[0])
					g.writeIndent()
					if optType, ok := argType.(*types.OptionalType); ok {
						// Optional type - unwrap it for printing
						innerType := optType.Inner
						// Generate code to unwrap and print
						g.write("if (")
						g.generateExpr(expr.Args[0])
						g.write(".is_some) { ")
						// Print the value
						g.write("%s(", g.getPrintFunctionNameForType(funcName, innerType))
						g.generateExpr(expr.Args[0])
						g.write(".value); } else { ferret_io_%s(\"<none>\"); }\n", funcName)
					} else if named, ok := argType.(*types.NamedType); ok {
						if optType, ok := named.Underlying.(*types.OptionalType); ok {
							innerType := optType.Inner
							g.write("if (")
							g.generateExpr(expr.Args[0])
							g.write(".is_some) { ")
							g.write("%s(", g.getPrintFunctionNameForType(funcName, innerType))
							g.generateExpr(expr.Args[0])
							g.write(".value); } else { ferret_io_%s(\"<none>\"); }\n", funcName)
						} else {
							g.write("%s(", g.getPrintFunctionName(funcName, expr.Args[0]))
							g.generateExpr(expr.Args[0])
							g.write(");\n")
						}
					} else {
						g.write("%s(", g.getPrintFunctionName(funcName, expr.Args[0]))
						g.generateExpr(expr.Args[0])
						g.write(");\n")
					}
					return
				} else {
					// Multiple arguments - format and concatenate
					g.generatePrintWithMultipleArgs(funcName, expr.Args)
					return
				}
			} else {
				g.write("%s(", sym.NativeName)
			}
			for i, arg := range expr.Args {
				if i > 0 {
					g.write(", ")
				}
				g.generateExpr(arg)
			}
			g.write(")")
			return
		}

		// Regular function call - check if function is from current module
		funcName := ident.Name
		cFuncName := g.sanitizeName(funcName)

		// Check if function is in current module's scope
		sym, found := g.mod.ModuleScope.GetSymbol(funcName)
		if found && sym != nil && sym.Kind == symbols.SymbolFunction {
			// Function is in current module - use same prefixing logic as function definition
			if funcName != "main" && g.mod.ImportPath != g.ctx.EntryModule {
				// Prefix with module path to avoid conflicts
				modulePrefix := strings.ReplaceAll(strings.ReplaceAll(g.mod.ImportPath, "/", "_"), "-", "_")
				cFuncName = g.sanitizeName(modulePrefix) + "_" + cFuncName
			}
		}
		// If not found in current module, it might be from imported module
		// but that should be handled by scope resolution (module::function)
		// For now, use the name as-is (will fail at compile time if wrong)

		g.write("%s(", cFuncName)
		for i, arg := range expr.Args {
			if i > 0 {
				g.write(", ")
			}
			g.generateExpr(arg)
		}
		g.write(")")
	}
}

func (g *Generator) typeToC(typ types.SemType) string {
	switch t := typ.(type) {
	case *types.PrimitiveType:
		switch t.GetName() {
		case types.TYPE_I8:
			return "int8_t"
		case types.TYPE_I16:
			return "int16_t"
		case types.TYPE_I32:
			return "int32_t"
		case types.TYPE_I64:
			return "int64_t"
		case types.TYPE_I128:
			return "ferret_i128"
		case types.TYPE_I256:
			return "ferret_i256"
		case types.TYPE_U8:
			return "uint8_t"
		case types.TYPE_U16:
			return "uint16_t"
		case types.TYPE_U32:
			return "uint32_t"
		case types.TYPE_U64:
			return "uint64_t"
		case types.TYPE_U128:
			return "ferret_u128"
		case types.TYPE_U256:
			return "ferret_u256"
		case types.TYPE_F32:
			return "float"
		case types.TYPE_F64:
			return "double"
		case types.TYPE_F128:
			return "ferret_f128"
		case types.TYPE_F256:
			return "ferret_f256"
		case types.TYPE_STRING:
			return "const char*"
		case types.TYPE_BOOL:
			return "int"
		case types.TYPE_VOID:
			return "void"
		}
	case *types.ResultType:
		// Result types: for now, return success type
		// In full implementation, would need to handle error case
		return g.typeToC(t.Ok)
	case *types.InterfaceType:
		// Empty interface (interface{}) - use void* as generic pointer type
		if len(t.Methods) == 0 {
			return "void*"
		}
		// Non-empty interfaces use ferret_interface_t structure
		// The vtable type will be generated separately
		return "ferret_interface_t"
	case *types.NamedType:
		// For named types, generate a struct name or use underlying type
		// If it's a struct, we'll generate a struct definition
		if _, ok := t.Underlying.(*types.StructType); ok {
			// Generate struct name from type name
			return g.sanitizeName(t.Name)
		}
		// If it's an interface, use ferret_interface_t (vtable type generated separately)
		if iface, ok := t.Underlying.(*types.InterfaceType); ok {
			if len(iface.Methods) == 0 {
				return "void*"
			}
			return "ferret_interface_t"
		}
		// For other named types, unwrap to underlying
		return g.typeToC(t.Underlying)
	case *types.ReferenceType:
		// Reference type &T -> T* in C
		innerType := g.typeToC(t.Inner)
		return fmt.Sprintf("%s*", innerType)
	case *types.StructType:
		// Anonymous struct - emit inline struct definition
		return g.structTypeToC(t)
	case *types.MapType:
		return "ferret_map_t*"
	case *types.ArrayType:
		// Array types: [N]T or []T
		elType := g.typeToC(t.Element)
		if t.Length >= 0 {
			// Fixed-size array: [N]T
			return fmt.Sprintf("%s[%d]", elType, t.Length)
		}
		// Dynamic array: []T - use pointer for now
		// TODO: Implement proper dynamic array support
		return fmt.Sprintf("%s*", elType)
	case *types.OptionalType:
		// Optional types: T? -> ferret_optional_T
		g.trackOptional(t.Inner)
		return fmt.Sprintf("ferret_optional_%s", g.getOptionalTypeName(t.Inner))
	}
	return "void"
}

// trackOptional records an optional inner type for typedef emission.
func (g *Generator) trackOptional(inner types.SemType) {
	name := g.getOptionalTypeName(inner)
	if _, ok := g.optionalTyps[name]; ok {
		return
	}
	g.optionalTyps[name] = inner
}

func (g *Generator) generateOptionalTypeDefinitions() {
	// Build a complete view of optional types: tracked + AST/symbol scan
	g.collectOptionalTypes()

	optionalTypes := make(map[string]types.SemType, len(g.optionalTyps))
	for k, v := range g.optionalTyps {
		optionalTypes[k] = v
	}

	addOptional := func(t types.SemType) {
		if t == nil {
			return
		}
		if opt, ok := types.UnwrapType(t).(*types.OptionalType); ok {
			name := g.getOptionalTypeName(opt.Inner)
			if _, exists := optionalTypes[name]; !exists {
				optionalTypes[name] = opt.Inner
			}
		}
	}

	var visitTypeNode func(ast.TypeNode)
	visitTypeNode = func(node ast.TypeNode) {
		if node == nil {
			return
		}
		switch n := node.(type) {
		case *ast.OptionalType:
			visitTypeNode(n.Base)
			if sem := g.typeFromTypeNode(n); sem != nil && !sem.Equals(types.TypeUnknown) {
				addOptional(sem)
			} else {
				if ident, ok := n.Base.(*ast.IdentifierExpr); ok {
					addOptional(types.NewOptional(types.FromTypeName(types.TYPE_NAME(ident.Name))))
				}
			}
		case *ast.ReferenceType:
			visitTypeNode(n.Base)
		case *ast.ArrayType:
			visitTypeNode(n.ElType)
		case *ast.MapType:
			visitTypeNode(n.Key)
			visitTypeNode(n.Value)
		case *ast.StructType:
			for i := range n.Fields {
				visitTypeNode(n.Fields[i].Type)
			}
		case *ast.InterfaceType:
			for i := range n.Methods {
				visitTypeNode(n.Methods[i].Type)
			}
		case *ast.ResultType:
			visitTypeNode(n.Value)
			visitTypeNode(n.Error)
		case *ast.FuncType:
			for i := range n.Params {
				visitTypeNode(n.Params[i].Type)
			}
			visitTypeNode(n.Result)
		}
	}

	var visitNode func(ast.Node)
	visitNode = func(node ast.Node) {
		if node == nil {
			return
		}
		switch n := node.(type) {
		case *ast.VarDecl:
			for _, item := range n.Decls {
				visitTypeNode(item.Type)
			}
		case *ast.ConstDecl:
			for _, item := range n.Decls {
				visitTypeNode(item.Type)
			}
		case *ast.AssignStmt:
			visitNode(n.Lhs)
			visitNode(n.Rhs)
		case *ast.IfStmt:
			if n.Body != nil {
				for _, stmt := range n.Body.Nodes {
					visitNode(stmt)
				}
			}
			if n.Else != nil {
				if block, ok := n.Else.(*ast.Block); ok {
					for _, stmt := range block.Nodes {
						visitNode(stmt)
					}
				}
			}
		case *ast.ForStmt:
			if n.Iterator != nil {
				visitNode(n.Iterator)
			}
			if n.Body != nil {
				for _, stmt := range n.Body.Nodes {
					visitNode(stmt)
				}
			}
		case *ast.WhileStmt:
			if n.Body != nil {
				for _, stmt := range n.Body.Nodes {
					visitNode(stmt)
				}
			}
		case *ast.Block:
			for _, stmt := range n.Nodes {
				visitNode(stmt)
			}
		}
	}

	// Symbol table types
	for _, sym := range g.mod.ModuleScope.GetAllSymbols() {
		if sym == nil || sym.Type == nil {
			continue
		}
		addOptional(sym.Type)
	}

	// AST types
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.TypeDecl:
				visitTypeNode(n.Type)
			case *ast.VarDecl:
				visitNode(n)
			case *ast.ConstDecl:
				visitNode(n)
			case *ast.FuncDecl:
				if n.Type != nil {
					visitTypeNode(n.Type.Result)
					for _, p := range n.Type.Params {
						visitTypeNode(p.Type)
					}
				}
				if n.Body != nil {
					for _, stmt := range n.Body.Nodes {
						visitNode(stmt)
					}
				}
			case *ast.MethodDecl:
				if n.Receiver != nil {
					visitTypeNode(n.Receiver.Type)
				}
				if n.Type != nil {
					visitTypeNode(n.Type.Result)
					for _, p := range n.Type.Params {
						visitTypeNode(p.Type)
					}
				}
				if n.Body != nil {
					for _, stmt := range n.Body.Nodes {
						visitNode(stmt)
					}
				}
			}
		}
	}

	// Persist discovered optional types for reuse by other generators/phases
	codegen.StoreOptionalTypes(g.mod, optionalTypes)

	keys := make([]string, 0, len(optionalTypes))
	for k := range optionalTypes {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	noneGenerated := false
	for _, typeName := range keys {
		innerType := optionalTypes[typeName]
		innerCType := g.typeToC(innerType)
		g.write("typedef struct {\n")
		g.write("    %s value;\n", innerCType)
		g.write("    int is_some;\n")
		g.write("} ferret_optional_%s;\n", typeName)

		if !noneGenerated {
			g.write("static const ferret_optional_%s none = ((ferret_optional_%s){.value = 0, .is_some = 0});\n", typeName, typeName)
			noneGenerated = true
		}
		g.write("\n")
	}
}

// structTypeToC generates an inline struct type definition for anonymous structs
// Example: struct { int32_t X; int32_t Y; }
func (g *Generator) structTypeToC(structType *types.StructType) string {
	var buf strings.Builder
	buf.WriteString("struct {")
	for i, field := range structType.Fields {
		if i > 0 {
			buf.WriteString(";")
		}
		fieldTypeC := g.typeToC(field.Type)
		buf.WriteString(fmt.Sprintf(" %s %s", fieldTypeC, g.sanitizeName(field.Name)))
	}
	if len(structType.Fields) > 0 {
		buf.WriteString(";")
	}
	buf.WriteString(" }")
	return buf.String()
}

func (g *Generator) typeFromTypeNode(node ast.TypeNode) types.SemType {
	// Use the typechecker's implementation with proper context
	// This allows resolving user-defined types, not just primitives
	return typechecker.TypeFromTypeNodeWithContext(g.ctx, g.mod, node)
}

func (g *Generator) sanitizeName(name string) string {
	// Replace invalid C identifier characters
	return strings.ReplaceAll(name, "::", "_")
}

// getOptionalTypeName generates a C identifier name for an optional type
// e.g., i32 -> int32_t, str -> str (for optional struct names)
func (g *Generator) getOptionalTypeName(innerType types.SemType) string {
	// Get the base C type name
	baseType := g.typeToC(innerType)
	// Sanitize it for use in struct name (remove *, [], etc.)
	// For simple types like int32_t, use as-is
	// For complex types, we need to create a valid identifier
	baseType = strings.ReplaceAll(baseType, "*", "_ptr")
	baseType = strings.ReplaceAll(baseType, "[", "_arr")
	baseType = strings.ReplaceAll(baseType, "]", "")
	baseType = strings.ReplaceAll(baseType, " ", "_")
	return baseType
}

// collectOptionalTypes pre-scans module types to populate optionalTyps.
func (g *Generator) collectOptionalTypes() {
	trackSem := func(t types.SemType) {
		if t == nil {
			return
		}
		if opt, ok := types.UnwrapType(t).(*types.OptionalType); ok {
			g.trackOptional(opt.Inner)
		}
	}

	var visitTypeNode func(ast.TypeNode)
	visitTypeNode = func(node ast.TypeNode) {
		if node == nil {
			return
		}
		switch n := node.(type) {
		case *ast.OptionalType:
			visitTypeNode(n.Base)
			inner := g.typeFromTypeNode(n.Base)
			if inner != nil {
				g.trackOptional(inner)
			} else {
				trackSem(g.typeFromTypeNode(n))
			}
		case *ast.ReferenceType:
			visitTypeNode(n.Base)
		case *ast.ArrayType:
			visitTypeNode(n.ElType)
		case *ast.MapType:
			visitTypeNode(n.Key)
			visitTypeNode(n.Value)
		case *ast.StructType:
			for i := range n.Fields {
				visitTypeNode(n.Fields[i].Type)
			}
		case *ast.InterfaceType:
			for i := range n.Methods {
				visitTypeNode(n.Methods[i].Type)
			}
		case *ast.ResultType:
			visitTypeNode(n.Value)
			visitTypeNode(n.Error)
		case *ast.FuncType:
			for i := range n.Params {
				visitTypeNode(n.Params[i].Type)
			}
			visitTypeNode(n.Result)
		}
	}

	// Types from symbol table
	for _, sym := range g.mod.ModuleScope.GetAllSymbols() {
		if sym == nil || sym.Type == nil {
			continue
		}
		trackSem(sym.Type)
	}

	// Types from AST
	if g.mod.AST != nil {
		var visitNode func(ast.Node)
		visitNode = func(node ast.Node) {
			if node == nil {
				return
			}
			switch n := node.(type) {
			case *ast.VarDecl:
				for _, item := range n.Decls {
					visitTypeNode(item.Type)
				}
			case *ast.ConstDecl:
				for _, item := range n.Decls {
					visitTypeNode(item.Type)
				}
			case *ast.AssignStmt:
				visitNode(n.Lhs)
				visitNode(n.Rhs)
			case *ast.IfStmt:
				if n.Body != nil {
					for _, stmt := range n.Body.Nodes {
						visitNode(stmt)
					}
				}
				if n.Else != nil {
					if block, ok := n.Else.(*ast.Block); ok {
						for _, stmt := range block.Nodes {
							visitNode(stmt)
						}
					}
				}
			case *ast.ForStmt:
				if n.Iterator != nil {
					visitNode(n.Iterator)
				}
				if n.Body != nil {
					for _, stmt := range n.Body.Nodes {
						visitNode(stmt)
					}
				}
			case *ast.WhileStmt:
				if n.Body != nil {
					for _, stmt := range n.Body.Nodes {
						visitNode(stmt)
					}
				}
			case *ast.Block:
				for _, stmt := range n.Nodes {
					visitNode(stmt)
				}
			}
		}

		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.TypeDecl:
				visitTypeNode(n.Type)
			case *ast.VarDecl:
				for _, item := range n.Decls {
					visitTypeNode(item.Type)
				}
			case *ast.ConstDecl:
				for _, item := range n.Decls {
					visitTypeNode(item.Type)
				}
			case *ast.FuncDecl:
				if n.Type != nil {
					visitTypeNode(n.Type.Result)
					for _, p := range n.Type.Params {
						visitTypeNode(p.Type)
					}
				}
				if n.Body != nil {
					for _, stmt := range n.Body.Nodes {
						visitNode(stmt)
					}
				}
			case *ast.MethodDecl:
				if n.Receiver != nil {
					visitTypeNode(n.Receiver.Type)
				}
				if n.Type != nil {
					visitTypeNode(n.Type.Result)
					for _, p := range n.Type.Params {
						visitTypeNode(p.Type)
					}
				}
				if n.Body != nil {
					for _, stmt := range n.Body.Nodes {
						visitNode(stmt)
					}
				}
			}
		}
	}
}

// mapHashEqualsForType picks runtime hash/equals helpers for a given key type.
// Falls back to byte-wise hash/equals when we don't have a specialized function.
func (g *Generator) mapHashEqualsForType(keyType types.SemType) (string, string) {
	keyType = types.UnwrapType(keyType)
	switch kt := keyType.(type) {
	case *types.PrimitiveType:
		switch kt.GetName() {
		case types.TYPE_I32:
			return "ferret_map_hash_i32", "ferret_map_equals_i32"
		case types.TYPE_I64:
			return "ferret_map_hash_i64", "ferret_map_equals_i64"
		case types.TYPE_STRING:
			return "ferret_map_hash_str", "ferret_map_equals_str"
		}
	case *types.NamedType:
		// Recurse on underlying type
		return g.mapHashEqualsForType(kt.Underlying)
	}
	// Default fallback for unsupported key types
	return "ferret_map_hash_bytes", "ferret_map_equals_bytes"
}

// cSizeOfType returns a sizeof expression for the C type backing the semantic type.
func (g *Generator) cSizeOfType(sem types.SemType) string {
	return fmt.Sprintf("sizeof(%s)", g.typeToC(sem))
}

// getPrintFunctionNameForType returns the appropriate Print/Println function name based on type
func (g *Generator) getPrintFunctionNameForType(funcName string, argType types.SemType) string {
	if argType == nil {
		return fmt.Sprintf("ferret_io_%s", funcName)
	}

	// Get the base function name (Print or Println)
	baseName := "Print"
	if funcName == "Println" {
		baseName = "Println"
	}

	// Map type to function suffix
	switch t := argType.(type) {
	case *types.PrimitiveType:
		switch t.GetName() {
		case types.TYPE_I8:
			return fmt.Sprintf("ferret_io_%s_i8", baseName)
		case types.TYPE_I16:
			return fmt.Sprintf("ferret_io_%s_i16", baseName)
		case types.TYPE_I32:
			return fmt.Sprintf("ferret_io_%s_i32", baseName)
		case types.TYPE_I64:
			return fmt.Sprintf("ferret_io_%s_i64", baseName)
		case types.TYPE_U8:
			return fmt.Sprintf("ferret_io_%s_u8", baseName)
		case types.TYPE_U16:
			return fmt.Sprintf("ferret_io_%s_u16", baseName)
		case types.TYPE_U32:
			return fmt.Sprintf("ferret_io_%s_u32", baseName)
		case types.TYPE_U64:
			return fmt.Sprintf("ferret_io_%s_u64", baseName)
		case types.TYPE_F32:
			return fmt.Sprintf("ferret_io_%s_f32", baseName)
		case types.TYPE_F64:
			return fmt.Sprintf("ferret_io_%s_f64", baseName)
		case types.TYPE_BOOL:
			return fmt.Sprintf("ferret_io_%s_bool", baseName)
		case types.TYPE_STRING:
			return fmt.Sprintf("ferret_io_%s", baseName)
		}
	case *types.NamedType:
		// Unwrap to underlying type
		return g.getPrintFunctionNameForType(funcName, t.Underlying)
	}

	// Fallback to string version
	return fmt.Sprintf("ferret_io_%s", baseName)
}

// getPrintFunctionName returns the appropriate Print/Println function name based on argument type
func (g *Generator) getPrintFunctionName(funcName string, arg ast.Expression) string {
	// Get the type of the first argument
	var argType types.SemType

	// Try to get type from identifier
	if ident, ok := arg.(*ast.IdentifierExpr); ok {
		if sym, found := g.mod.CurrentScope.Lookup(ident.Name); found && sym != nil && sym.Type != nil {
			argType = sym.Type
		}
	}

	// Try to get type from scope resolution (module::symbol)
	if argType == nil {
		if scopeRes, ok := arg.(*ast.ScopeResolutionExpr); ok {
			if moduleIdent, ok := scopeRes.X.(*ast.IdentifierExpr); ok {
				importPath, ok := g.mod.ImportAliasMap[moduleIdent.Name]
				if ok {
					importedMod, exists := g.ctx.GetModule(importPath)
					if exists {
						sym, found := importedMod.ModuleScope.GetSymbol(scopeRes.Selector.Name)
						if found && sym != nil && sym.Type != nil {
							argType = sym.Type
						}
					}
				}
			}
		}
	}

	// If not found, try to infer from literal
	if argType == nil {
		if lit, ok := arg.(*ast.BasicLit); ok {
			switch lit.Kind {
			case ast.INT:
				argType = types.TypeI32 // Default to i32 for integers
			case ast.FLOAT:
				argType = types.TypeF64 // Default to f64 for floats
			case ast.BOOL:
				argType = types.TypeBool
			case ast.STRING:
				argType = types.TypeString
			}
		}
	}

	// Default to string if type is still unknown
	if argType == nil {
		return fmt.Sprintf("ferret_io_%s", funcName)
	}

	// Get the base function name (Print or Println)
	baseName := "Print"
	if funcName == "Println" {
		baseName = "Println"
	}

	// Map type to function suffix
	switch t := argType.(type) {
	case *types.PrimitiveType:
		switch t.GetName() {
		case types.TYPE_I8:
			return fmt.Sprintf("ferret_io_%s_i8", baseName)
		case types.TYPE_I16:
			return fmt.Sprintf("ferret_io_%s_i16", baseName)
		case types.TYPE_I32:
			return fmt.Sprintf("ferret_io_%s_i32", baseName)
		case types.TYPE_I64:
			return fmt.Sprintf("ferret_io_%s_i64", baseName)
		case types.TYPE_U8:
			return fmt.Sprintf("ferret_io_%s_u8", baseName)
		case types.TYPE_U16:
			return fmt.Sprintf("ferret_io_%s_u16", baseName)
		case types.TYPE_U32:
			return fmt.Sprintf("ferret_io_%s_u32", baseName)
		case types.TYPE_U64:
			return fmt.Sprintf("ferret_io_%s_u64", baseName)
		case types.TYPE_F32:
			return fmt.Sprintf("ferret_io_%s_f32", baseName)
		case types.TYPE_F64:
			return fmt.Sprintf("ferret_io_%s_f64", baseName)
		case types.TYPE_BOOL:
			return fmt.Sprintf("ferret_io_%s_bool", baseName)
		case types.TYPE_STRING:
			return fmt.Sprintf("ferret_io_%s", baseName)
		}
	}

	// Fallback to string version
	return fmt.Sprintf("ferret_io_%s", baseName)
}

// generatePrintWithMultipleArgs generates code for Print/Println with multiple arguments
// It formats all arguments into a single string using sprintf, then calls the string version
func (g *Generator) generatePrintWithMultipleArgs(funcName string, args []ast.Expression) {
	// Generate a unique temporary buffer name
	g.counter++
	bufName := fmt.Sprintf("__print_buf_%d", g.counter)

	// Generate a temporary buffer and format string
	g.writeIndent()
	g.write("char %s[1024];\n", bufName)

	// Build format string
	formatParts := []string{}

	for _, arg := range args {
		// Use semantic analyzer's type inference which is comprehensive and handles
		// all expression types including literals, identifiers, function calls, etc.
		argType := g.inferExprType(arg)

		// If type is still unknown after inference, it's likely a real error.
		// The semantic analyzer should have caught this, but we handle it gracefully.
		// For identifiers, the type should always be available from the symbol table.

		if argType == nil || argType.Equals(types.TypeUnknown) {
			// Type inference failed - report error instead of defaulting to string
			// Defaulting to string causes segfaults when the value is not a string
			g.ctx.ReportError(fmt.Sprintf("codegen: cannot determine type for print argument (expression type: %T)", arg), nil)
			formatParts = append(formatParts, "%s") // Still add format to prevent syntax errors, but will likely crash
		} else if argType.Equals(types.TypeNone) {
			// none type - print as string
			formatParts = append(formatParts, "%s")
		} else {
			// Check if it's an optional type - if so, use %s format since we'll format as string
			var actualType types.SemType
			isOptional := false
			if optType, ok := argType.(*types.OptionalType); ok {
				actualType = optType.Inner
				isOptional = true
			} else if named, ok := argType.(*types.NamedType); ok {
				if optType, ok := named.Underlying.(*types.OptionalType); ok {
					actualType = optType.Inner
					isOptional = true
				} else {
					actualType = argType
				}
			} else {
				actualType = argType
			}

			// For optional types, always use %s format since we format as string ("value" or "none")
			if isOptional {
				formatParts = append(formatParts, "%s")
				continue // Skip the rest, format is set
			}

			// Unwrap NamedType to get underlying type for format specifier determination
			unwrappedType := types.UnwrapType(actualType)

			if g.isStringType(unwrappedType) {
				formatParts = append(formatParts, "%s")
			} else if prim, ok := unwrappedType.(*types.PrimitiveType); ok {
				switch prim.GetName() {
				case types.TYPE_I8, types.TYPE_I16, types.TYPE_I32:
					formatParts = append(formatParts, "%d")
				case types.TYPE_I64:
					formatParts = append(formatParts, "%ld")
				case types.TYPE_U8, types.TYPE_U16, types.TYPE_U32:
					formatParts = append(formatParts, "%u")
				case types.TYPE_U64:
					formatParts = append(formatParts, "%lu")
				case types.TYPE_F32:
					formatParts = append(formatParts, "%.6g")
				case types.TYPE_F64:
					formatParts = append(formatParts, "%.15g")
				case types.TYPE_BOOL:
					formatParts = append(formatParts, "%s") // Will format as "true"/"false"
				case types.TYPE_STRING:
					formatParts = append(formatParts, "%s")
				default:
					// Unknown primitive type - report error but use safe fallback
					g.ctx.ReportError(fmt.Sprintf("codegen: unsupported primitive type for printing: %s", prim.GetName()), nil)
					formatParts = append(formatParts, "%s")
				}
			} else if g.isIntegerType(unwrappedType) {
				// Fallback for integer types that aren't PrimitiveType (e.g., NamedType wrapping integer)
				formatParts = append(formatParts, "%d")
			} else {
				// Non-primitive type - report error
				g.ctx.ReportError(fmt.Sprintf("codegen: cannot print non-primitive type: %s", unwrappedType.String()), nil)
				formatParts = append(formatParts, "%s")
			}
		}
	}

	// Generate temp buffers for optional values BEFORE snprintf
	// We need to format optional values that have a value, using the same logic as their inner type T
	optionalTempBuffers := make(map[int]string) // map from arg index to temp buffer name

	for i, arg := range args {
		argType := g.inferExprType(arg)
		if argType == nil || argType.Equals(types.TypeUnknown) {
			if ident, ok := arg.(*ast.IdentifierExpr); ok {
				if sym, found := g.mod.CurrentScope.Lookup(ident.Name); found && sym != nil && sym.Type != nil {
					argType = sym.Type
				}
			}
		}

		// Check if this is an optional type that needs formatting
		var innerType types.SemType
		if optType, ok := argType.(*types.OptionalType); ok {
			innerType = optType.Inner
		} else if named, ok := argType.(*types.NamedType); ok {
			if optType, ok := named.Underlying.(*types.OptionalType); ok {
				innerType = optType.Inner
			}
		}

		if innerType != nil {
			// We need a temp buffer for this optional - format it before the main snprintf
			tempBuf := fmt.Sprintf("__opt_val_%d_%d", g.counter, i)
			optionalTempBuffers[i] = tempBuf
			g.writeIndent()
			g.write("char %s[64];\n", tempBuf)

			// Generate code to format the optional value: if has value, format inner type T, else "none"
			g.writeIndent()
			g.write("if (")
			g.generateExpr(arg)
			g.write(".is_some) {\n")
			g.indent++

			// Format using the same logic as the inner type T
			unwrappedInner := types.UnwrapType(innerType)
			if prim, ok := unwrappedInner.(*types.PrimitiveType); ok && prim.GetName() == types.TYPE_STRING {
				// String - just copy the value
				g.writeIndent()
				g.write("strncpy(%s, ", tempBuf)
				g.generateExpr(arg)
				g.write(".value, sizeof(%s) - 1); %s[sizeof(%s) - 1] = '\\0';", tempBuf, tempBuf, tempBuf)
			} else if prim, ok := unwrappedInner.(*types.PrimitiveType); ok && prim.GetName() == types.TYPE_BOOL {
				// Bool - format as "true" or "false"
				g.writeIndent()
				g.write("strcpy(%s, ", tempBuf)
				g.generateExpr(arg)
				g.write(".value ? \"true\" : \"false\");")
			} else {
				// Numeric or other types - use snprintf with appropriate format
				// Determine format specifier based on inner type T (reusing existing type handlers)
				g.writeIndent()
				g.buf.WriteString("snprintf(")
				g.buf.WriteString(tempBuf)
				g.buf.WriteString(", sizeof(")
				g.buf.WriteString(tempBuf)
				g.buf.WriteString("), \"")
				// Write format specifier directly (same logic as format string building)
				if g.isStringType(unwrappedInner) {
					g.buf.WriteString("%s")
				} else if prim, ok := unwrappedInner.(*types.PrimitiveType); ok {
					switch prim.GetName() {
					case types.TYPE_I8, types.TYPE_I16, types.TYPE_I32:
						g.buf.WriteString("%d")
					case types.TYPE_I64:
						g.buf.WriteString("%ld")
					case types.TYPE_U8, types.TYPE_U16, types.TYPE_U32:
						g.buf.WriteString("%u")
					case types.TYPE_U64:
						g.buf.WriteString("%lu")
					case types.TYPE_F32:
						g.buf.WriteString("%.6g")
					case types.TYPE_F64:
						g.buf.WriteString("%.15g")
					default:
						g.buf.WriteString("%d")
					}
				} else if g.isIntegerType(unwrappedInner) {
					g.buf.WriteString("%d")
				} else {
					g.buf.WriteString("%d")
				}
				g.buf.WriteString("\", ")
				g.generateExpr(arg)
				g.buf.WriteString(".value);")
			}
			g.write("\n")
			g.indent--
			g.writeIndent()
			g.write("} else {\n")
			g.indent++
			g.writeIndent()
			g.write("strcpy(%s, \"none\");\n", tempBuf)
			g.indent--
			g.writeIndent()
			g.write("}\n")
		}
	}

	formatStr := strings.Join(formatParts, "")

	// Generate: snprintf(bufName, sizeof(bufName), format, args...);
	g.writeIndent()
	g.write("snprintf(%s, sizeof(%s), \"%s\", ", bufName, bufName, formatStr)
	for i, arg := range args {
		if i > 0 {
			g.write(", ")
		}
		argType := g.inferExprType(arg)
		// If type is unknown, try to infer from the expression itself
		if argType == nil || argType.Equals(types.TypeUnknown) {
			// For identifiers, try harder to find the type
			if ident, ok := arg.(*ast.IdentifierExpr); ok {
				// Try looking up in current scope with more thorough search
				if sym, found := g.mod.CurrentScope.Lookup(ident.Name); found && sym != nil && sym.Type != nil {
					argType = sym.Type
				}
			}
		}

		// Check if we have a temp buffer for this optional
		if tempBuf, hasTempBuf := optionalTempBuffers[i]; hasTempBuf {
			// Use the pre-formatted temp buffer
			g.write("%s", tempBuf)
		} else if argType != nil && argType.Equals(types.TypeNone) {
			// Handle none type - print as string
			g.write("\"none\"")
		} else {
			// Check if it's an optional type (should have been handled above, but just in case)
			var isOptional bool
			if _, ok := argType.(*types.OptionalType); ok {
				isOptional = true
			} else if named, ok := argType.(*types.NamedType); ok {
				if _, ok := named.Underlying.(*types.OptionalType); ok {
					isOptional = true
				}
			}

			if !isOptional {
				// Not optional - handle normally using existing type handlers
				if prim, ok := argType.(*types.PrimitiveType); ok && prim.GetName() == types.TYPE_BOOL {
					g.write("(")
					g.generateExpr(arg)
					g.write(" ? \"true\" : \"false\")")
				} else {
					g.generateExpr(arg)
				}
			} else {
				// Fallback for optionals without temp buffer (shouldn't happen)
				g.write("\"none\"")
			}
		}
	}
	g.write(");\n")

	// Call the string version of Print/Println
	if funcName == "Println" {
		g.writeIndent()
		g.write("ferret_io_Println(%s);\n", bufName)
	} else {
		g.writeIndent()
		g.write("ferret_io_Print(%s);\n", bufName)
	}
}

func (g *Generator) write(format string, args ...interface{}) {
	g.buf.WriteString(fmt.Sprintf(format, args...))
}

func (g *Generator) writeIndent() {
	for i := 0; i < g.indent; i++ {
		g.buf.WriteString(g.indentStr)
	}
}

// GenerateModule generates C code for a module and writes it to a file
func GenerateModule(ctx *context_v2.CompilerContext, mod *context_v2.Module, outputPath string) error {
	gen := New(ctx, mod)
	code, err := gen.Generate()
	if err != nil {
		return err
	}

	// Ensure output directory exists
	dir := filepath.Dir(outputPath)
	if err := os.MkdirAll(dir, 0755); err != nil {
		return err
	}

	// Write C code
	if err := os.WriteFile(outputPath, []byte(code), 0644); err != nil {
		return err
	}

	if ctx.Config.Debug {
		colors.GREEN.Printf("  ✓ Generated: %s\n", outputPath)
	}

	return nil
}
