package cgen

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/frontend/ast"
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
	g.write("\n")
}

func (g *Generator) writeRuntimeDeclarations() {
	g.write("// Runtime function declarations\n")
	g.write("// Include runtime header: #include \"runtime/io.h\"\n")
	g.write("void ferret_io_Println(const char* msg);\n")
	g.write("void ferret_io_Print(const char* msg);\n")
	g.write("int32_t ferret_io_ReadInt(char** error);\n")
	g.write("double ferret_io_ReadFloat(char** error);\n")
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
	g.write("%s %s(", returnType, g.sanitizeName(funcName))

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
		if sym.Type != nil {
			varType = sym.Type
		} else if item.Type != nil {
			// Fallback to parsing type node (shouldn't happen if type checking worked)
			varType = g.typeFromTypeNode(item.Type)
			if varType == types.TypeUnknown {
				g.ctx.ReportError(fmt.Sprintf("codegen: could not resolve type for variable '%s'", item.Name.Name), nil)
				continue
			}
		} else {
			g.ctx.ReportError(fmt.Sprintf("codegen: variable '%s' has no type information", item.Name.Name), nil)
			continue
		}

		g.writeIndent()
		g.write("%s %s", g.typeToC(varType), g.sanitizeName(item.Name.Name))

		if item.Value != nil {
			g.write(" = ")
			g.generateExpr(item.Value)
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
		g.write("const %s %s", g.typeToC(varType), g.sanitizeName(item.Name.Name))

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
	g.generateExpr(expr.X)
	g.write(" %s ", expr.Op.Value)
	g.generateExpr(expr.Y)
}

func (g *Generator) generateUnaryExpr(expr *ast.UnaryExpr) {
	g.write(expr.Op.Value)
	g.generateExpr(expr.X)
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
					if found && sym != nil && sym.IsNative {
						// Native function call
						g.write("%s(", sym.NativeName)
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
			// Native function call
			g.write("%s(", sym.NativeName)
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

	if ctx.Debug {
		colors.GREEN.Printf("  ✓ Generated: %s\n", outputPath)
	}

	return nil
}
