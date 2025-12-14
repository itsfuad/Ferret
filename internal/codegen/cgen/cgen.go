package cgen

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"compiler/colors"
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
}

// New creates a new C code generator
func New(ctx *context_v2.CompilerContext, mod *context_v2.Module) *Generator {
	return &Generator{
		ctx:          ctx,
		mod:          mod,
		indentStr:    "    ",
		arrayLengths: make(map[string]int),
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
	g.write("#include <stdio.h>\n")
	g.write("#include <stdlib.h>\n")
	g.write("#include <string.h>\n")
	g.write("#include <stdint.h>\n")
	g.write("#include <math.h>\n") // For pow() function
	g.write("\n")
}

func (g *Generator) writeRuntimeDeclarations() {
	// Include the runtime headers
	g.write("#include \"io.h\"\n")        // Runtime header for I/O functions
	g.write("#include \"interface.h\"\n") // Required for ferret_interface_t type
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

	// Generate header guard
	guardName := strings.ToUpper(strings.ReplaceAll(strings.ReplaceAll(g.mod.ImportPath, "/", "_"), "-", "_")) + "_H"
	g.write("#ifndef %s\n", guardName)
	g.write("#define %s\n\n", guardName)
	g.write("#include <stdint.h>\n")
	g.write("#include <stdbool.h>\n\n")

	// Include runtime headers if needed (for interface types)
	g.write("#include \"interface.h\"\n\n")

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

	// Don't write includes here - they're added by the pipeline when creating the .c file
	// This keeps the implementation clean and allows the .c file to include its .h file
	// Type definitions are in the header file, so don't duplicate them here

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
				g.writeIndent()
				g.generateInterfaceAssignment(item.Name.Name, varType, item.Value)
				g.write(";\n")
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
		g.writeIndent()
		g.generateExpr(s.X)
		g.write(";\n")
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

	// Regular assignment (x = y)
	g.generateExpr(stmt.Lhs)
	g.write(" = ")
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
	// For now, report error - map iteration needs proper map implementation
	g.ctx.ReportError("codegen: map iteration not yet implemented", nil)
	g.writeIndent()
	g.write("/* map iteration not yet implemented */\n")
	// TODO: Implement map iteration when map type is fully implemented
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
		g.write(g.sanitizeName(e.Name))
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
		g.write(lit.Value)
	case ast.FLOAT:
		g.write(lit.Value)
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

	// Regular binary expression (numeric operations: +, -, *, /, %, comparisons: >, <, <=, >=, ==, !=)
	g.generateExpr(expr.X)
	g.write(" %s ", expr.Op.Value)
	g.generateExpr(expr.Y)
}

// inferExprType gets the type of an expression.
// First checks if type is stored in the AST node (populated during semantic analysis),
// otherwise falls back to type inference from the typechecker.
func (g *Generator) inferExprType(expr ast.Expression) types.SemType {
	if expr == nil {
		return types.TypeUnknown
	}

	// First, check if type information is stored in the AST node (populated by semantic analysis)
	// This is the preferred path as it's faster and uses already-computed type information
	switch e := expr.(type) {
	case *ast.BasicLit:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.IdentifierExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.BinaryExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.UnaryExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.PostfixExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.PrefixExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.CallExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.SelectorExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.IndexExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.CastExpr:
		if e.TypeInfo != nil && !e.TypeInfo.Equals(types.TypeUnknown) {
			return e.TypeInfo
		}
	case *ast.ParenExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.ScopeResolutionExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.RangeExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.CoalescingExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.ForkExpr:
		if e.Type != nil && !e.Type.Equals(types.TypeUnknown) {
			return e.Type
		}
	case *ast.FuncLit:
		if e.TypeInfo != nil && !e.TypeInfo.Equals(types.TypeUnknown) {
			return e.TypeInfo
		}
	case *ast.CompositeLit:
		if e.TypeInfo != nil && !e.TypeInfo.Equals(types.TypeUnknown) {
			return e.TypeInfo
		}
	}

	// Fallback: use type inference from typechecker if type not stored in AST
	// This handles edge cases where type wasn't stored or wasn't computed yet
	return typechecker.InferExprType(g.ctx, g.mod, expr)
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
	g.write(expr.Op.Value)
	g.generateExpr(expr.X)
}

func (g *Generator) generatePostfixExpr(expr *ast.PostfixExpr) {
	g.generateExpr(expr.X)
	g.write(expr.Op.Value)
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
						g.write(symbolName)
						return
					}
					// For variables, also use prefixed name
					if sym.Kind == symbols.SymbolVariable {
						g.write(symbolName)
						return
					}
					// For functions, this shouldn't happen here (should be in CallExpr)
					// But handle it anyway
					if sym.Kind == symbols.SymbolFunction {
						g.write(symbolName)
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
		g.write(g.sanitizeName(expr.Field.Name))
	} else {
		g.write("/* unknown field */")
	}
}

func (g *Generator) generateIndexExpr(expr *ast.IndexExpr) {
	// IndexExpr: array[index] or map[key]
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
			g.write(g.structTypeToC(structType))
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
		// Map literal: map[str]i32{"a" => 1, "b" => 2}
		// For now, report error - maps need proper implementation
		g.ctx.ReportError("codegen: map literals not yet implemented", nil)
		g.write("/* map literal not yet implemented */")
		_ = mapType // Avoid unused variable warning
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
									g.writeIndent()
									g.write("%s(", g.getPrintFunctionName(funcName, expr.Args[0]))
									g.generateExpr(expr.Args[0])
									g.write(");\n")
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
					g.writeIndent()
					g.write("%s(", g.getPrintFunctionName(funcName, expr.Args[0]))
					g.generateExpr(expr.Args[0])
					g.write(");\n")
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
		case types.TYPE_U8:
			return "uint8_t"
		case types.TYPE_U16:
			return "uint16_t"
		case types.TYPE_U32:
			return "uint32_t"
		case types.TYPE_U64:
			return "uint64_t"
		case types.TYPE_F32:
			return "float"
		case types.TYPE_F64:
			return "double"
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
		// Anonymous struct - generate inline struct definition
		// For now, use a generic struct pointer (void*)
		// TODO: Generate proper struct definitions
		return "void*"
	case *types.MapType:
		// Maps are implemented as hash tables - use void* for now
		// TODO: Implement proper map type with key/value types
		return "void*"
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
	}
	return "void"
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
	g.write("char %s[1024]; ", bufName)

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
		} else {
			// Unwrap NamedType to get underlying type for format specifier determination
			unwrappedType := types.UnwrapType(argType)

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

	formatStr := strings.Join(formatParts, "")

	// Generate: snprintf(bufName, sizeof(bufName), format, args...);
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
		// For bool, convert to string first
		if prim, ok := argType.(*types.PrimitiveType); ok && prim.GetName() == types.TYPE_BOOL {
			g.write("(")
			g.generateExpr(arg)
			g.write(" ? \"true\" : \"false\")")
		} else {
			g.generateExpr(arg)
		}
	}
	g.write("); ")

	// Call the string version of Print/Println
	if funcName == "Println" {
		g.write("ferret_io_Println(%s);\n", bufName)
	} else {
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
