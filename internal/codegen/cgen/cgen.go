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
	"compiler/internal/types"
)

// Generator generates C code from Ferret AST
type Generator struct {
	ctx       *context_v2.CompilerContext
	mod       *context_v2.Module
	buf       strings.Builder
	indent    int
	indentStr string
	counter   int // Counter for generating unique temporary variable names
}

// New creates a new C code generator
func New(ctx *context_v2.CompilerContext, mod *context_v2.Module) *Generator {
	return &Generator{
		ctx:       ctx,
		mod:       mod,
		indentStr: "    ",
	}
}

// Generate generates C code for the module
func (g *Generator) Generate() (string, error) {
	g.writeHeader()
	g.writeIncludes()
	g.writeRuntimeDeclarations()

	// Generate code for each top-level declaration
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.FuncDecl:
				g.generateFunction(n)
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
	// Include the runtime header for IO functions
	g.write("#include \"io.h\"\n")
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
	// TODO: Properly handle 'export' keyword instead of prefixing all functions
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

	// Generate function and constant declarations
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

	g.write("\n#endif // %s\n", guardName)

	result := g.buf.String()
	g.buf = oldBuf
	return result
}

// GenerateImplementation generates C implementation code (with includes)
func (g *Generator) GenerateImplementation() string {
	var buf strings.Builder
	oldBuf := g.buf
	g.buf = buf

	// Write includes and runtime header
	g.writeIncludes()
	g.writeRuntimeDeclarations()

	// Generate code for each top-level declaration
	if g.mod.AST != nil {
		for _, node := range g.mod.AST.Nodes {
			switch n := node.(type) {
			case *ast.FuncDecl:
				g.generateFunction(n)
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
		cType := g.typeToC(varType)
		g.write("%s %s", cType, g.sanitizeName(item.Name.Name))

		if item.Value != nil {
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
				g.generateExpr(item.Value)
			}
		}

		g.write(";\n")
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
	case *ast.ExprStmt:
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
	case *ast.CallExpr:
		g.generateCallExpr(e)
	case *ast.ParenExpr:
		g.write("(")
		g.generateExpr(e.X)
		g.write(")")
	case *ast.ScopeResolutionExpr:
		g.generateScopeResolutionExpr(e)
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

// inferExprType infers the type of an expression (simplified version for codegen)
func (g *Generator) inferExprType(expr ast.Expression) types.SemType {
	// Try to get type from identifier
	if ident, ok := expr.(*ast.IdentifierExpr); ok {
		if sym, found := g.mod.CurrentScope.Lookup(ident.Name); found && sym != nil && sym.Type != nil {
			return sym.Type
		}
	}

	// Try to get type from literal
	if lit, ok := expr.(*ast.BasicLit); ok {
		switch lit.Kind {
		case ast.INT:
			return types.TypeI32
		case ast.FLOAT:
			return types.TypeF64
		case ast.BOOL:
			return types.TypeBool
		case ast.STRING:
			return types.TypeString
		}
	}

	// Try scope resolution
	if scopeRes, ok := expr.(*ast.ScopeResolutionExpr); ok {
		if moduleIdent, ok := scopeRes.X.(*ast.IdentifierExpr); ok {
			importPath, ok := g.mod.ImportAliasMap[moduleIdent.Name]
			if ok {
				importedMod, exists := g.ctx.GetModule(importPath)
				if exists {
					sym, found := importedMod.ModuleScope.GetSymbol(scopeRes.Selector.Name)
					if found && sym != nil && sym.Type != nil {
						return sym.Type
					}
				}
			}
		}
	}

	// Try binary expressions
	if binExpr, ok := expr.(*ast.BinaryExpr); ok {
		xType := g.inferExprType(binExpr.X)
		yType := g.inferExprType(binExpr.Y)

		// Power operation returns double (or int if both operands are int)
		if binExpr.Op.Value == "**" {
			if g.isIntegerType(xType) && g.isIntegerType(yType) {
				return types.TypeI32 // Integer power returns integer
			}
			return types.TypeF64 // Otherwise returns double
		}

		// Comparison operators return bool
		if binExpr.Op.Value == ">" || binExpr.Op.Value == "<" || binExpr.Op.Value == ">=" ||
			binExpr.Op.Value == "<=" || binExpr.Op.Value == "==" || binExpr.Op.Value == "!=" {
			return types.TypeBool
		}

		// Arithmetic operations: result type depends on operands
		if binExpr.Op.Value == "+" || binExpr.Op.Value == "-" || binExpr.Op.Value == "*" ||
			binExpr.Op.Value == "/" || binExpr.Op.Value == "%" {
			// If either is float, result is float
			if prim, ok := xType.(*types.PrimitiveType); ok {
				if prim.GetName() == types.TYPE_F32 || prim.GetName() == types.TYPE_F64 {
					return types.TypeF64
				}
			}
			if prim, ok := yType.(*types.PrimitiveType); ok {
				if prim.GetName() == types.TYPE_F32 || prim.GetName() == types.TYPE_F64 {
					return types.TypeF64
				}
			}
			// Both integers - result is integer (use larger type, or default to i32)
			if g.isIntegerType(xType) && g.isIntegerType(yType) {
				return types.TypeI32 // Default to i32 for now
			}
		}
	}

	return types.TypeUnknown
}

// isStringType checks if a type is string
func (g *Generator) isStringType(typ types.SemType) bool {
	if typ == nil {
		return false
	}
	if prim, ok := typ.(*types.PrimitiveType); ok {
		return prim.GetName() == types.TYPE_STRING
	}
	return false
}

// isIntegerType checks if a type is an integer type
func (g *Generator) isIntegerType(typ types.SemType) bool {
	if typ == nil {
		return false
	}
	if prim, ok := typ.(*types.PrimitiveType); ok {
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

		// Regular function call
		g.write("%s(", g.sanitizeName(ident.Name))
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
		// This allows storing any type (though we'd need type information at runtime for full support)
		if len(t.Methods) == 0 {
			return "void*"
		}
		// Non-empty interfaces would need more complex representation
		// For now, use void* as fallback
		return "void*"
	case *types.NamedType:
		// Unwrap NamedType to get underlying type
		return g.typeToC(t.Underlying)
	}
	return "void"
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
		argType := g.inferExprType(arg)
		if g.isStringType(argType) {
			formatParts = append(formatParts, "%s")
		} else if prim, ok := argType.(*types.PrimitiveType); ok {
			switch prim.GetName() {
			case types.TYPE_I32:
				formatParts = append(formatParts, "%d")
			case types.TYPE_I64:
				formatParts = append(formatParts, "%ld")
			case types.TYPE_F32:
				formatParts = append(formatParts, "%.6g")
			case types.TYPE_F64:
				formatParts = append(formatParts, "%.15g")
			case types.TYPE_BOOL:
				formatParts = append(formatParts, "%s") // Will format as "true"/"false"
			default:
				formatParts = append(formatParts, "%s")
			}
		} else {
			formatParts = append(formatParts, "%s")
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
