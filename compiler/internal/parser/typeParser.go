package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/symboltable"
	"ferret/compiler/internal/types"
	"ferret/compiler/internal/utils"
	"ferret/compiler/report"
)

func parseIntegerType(p *Parser) (ast.DataType, bool) {
	token := p.advance()
	typename := types.TYPE_NAME(token.Value)
	bitSize, err := types.GetBitSize(typename)
	if err != nil {
		report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.INVALID_TYPE_NAME).SetLevel(report.SYNTAX_ERROR)
		return nil, false
	}
	return &ast.IntType{
		TypeName: typename,
		BitSize:  bitSize,
		Unsigned: types.IsUnsigned(typename),
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}, true
}

// user defined types are defined by the type keyword
// type NewType OldType;
func parseUserDefinedType(p *Parser) (ast.DataType, bool) {
	if p.match(lexer.IDENTIFIER_TOKEN) {
		token := p.advance()

		return &ast.UserDefinedType{
			TypeName: types.TYPE_NAME(token.Value),
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}, true
	}

	return nil, false
}

func parseFloatType(p *Parser) (ast.DataType, bool) {
	token := p.advance()
	typename := types.TYPE_NAME(token.Value)
	bitSize, err := types.GetBitSize(typename)
	if err != nil {
		report.Add(p.filePath, token.Start.Line, token.End.Line, token.Start.Column, token.End.Column, report.INVALID_TYPE_NAME).SetLevel(report.SYNTAX_ERROR)
		return nil, false
	}

	return &ast.FloatType{
		TypeName: typename,
		BitSize:  bitSize,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}, true
}

func parseStringType(p *Parser) (ast.DataType, bool) {
	token := p.advance()
	return &ast.StringType{
		TypeName: types.STRING,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}, true
}

func parseByteType(p *Parser) (ast.DataType, bool) {
	token := p.advance()
	return &ast.ByteType{
		TypeName: types.BYTE,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}, true
}

func parseBoolType(p *Parser) (ast.DataType, bool) {
	token := p.advance()
	return &ast.BoolType{
		TypeName: types.BOOL,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}, true
}

func parseArrayType(p *Parser) (ast.DataType, bool) {
	//consume the '[' token
	start := p.advance().Start
	// consume the ']' token
	p.consume(lexer.CLOSE_BRACKET, report.EXPECTED_CLOSE_BRACKET)

	//parse the type

	if elementType, ok := parseType(p); !ok {
		return nil, false
	} else {
		return &ast.ArrayType{
			ElementType: elementType,
			TypeName:    types.ARRAY,
			Location: ast.Location{
				Start: &start,
				End:   elementType.EndPos(),
			},
		}, true
	}
}

// parseStructField parses a single struct field
func parseStructField(p *Parser) *ast.StructField {
	// Parse field name
	nameToken := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_FIELD_NAME+" got "+p.peek().Value)
	fieldName := nameToken.Value

	// Expect colon
	p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)

	// Parse field type
	if fieldType, ok := parseType(p); !ok {
		return nil
	} else {
		return &ast.StructField{
			Field: ast.IdentifierExpr{
				Name: fieldName,
				Location: ast.Location{
					Start: &nameToken.Start,
					End:   &nameToken.End,
				},
			},
			Type: fieldType,
			Location: ast.Location{
				Start: &nameToken.Start,
				End:   fieldType.EndPos(),
			},
		}
	}
}

// parseStructType parses a struct type definition like struct { name: str, age: i32 }
func parseStructType(p *Parser) (ast.DataType, bool) {
	// Consume 'struct' keyword
	start := p.consume(lexer.STRUCT_TOKEN, report.EXPECTED_STRUCT_KEYWORD).Start

	// Create struct scope
	p.enterScope(symboltable.STRUCT_SCOPE)
	defer p.exitScope()

	// Consume opening brace
	p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE)

	// Check for empty struct
	if p.peek().Kind == lexer.CLOSE_CURLY {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			report.EMPTY_STRUCT_NOT_ALLOWED).SetLevel(report.SYNTAX_ERROR)
		return nil, false
	}

	fields := make([]ast.StructField, 0)
	fieldNames := make(map[string]bool)

	for !p.match(lexer.CLOSE_CURLY) {

		// Parse field
		field := parseStructField(p)
		if field == nil {
			return nil, false
		}

		// Check for duplicate field names
		if fieldNames[field.Field.Name] {
			report.Add(p.filePath, field.Location.Start.Line, field.Location.End.Line,
				field.Location.Start.Column, field.Location.End.Column,
				report.DUPLICATE_FIELD_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil, false
		}
		fieldNames[field.Field.Name] = true

		// Add field to struct scope
		sym := &symboltable.Symbol{
			Name:       field.Field.Name,
			SymbolKind: symboltable.STRUCT_FIELD_SYMBOL,
			Type:       field.Type.Type(),
			IsMutable:  true, // Fields are mutable by default
			Location: symboltable.SymbolLocation{
				File:   p.filePath,
				Line:   field.Location.Start.Line,
				Column: field.Location.Start.Column,
			},
		}

		if !p.currentScope.Define(sym) {
			report.ShowRedeclarationError(
				field.Field.Name,
				p.filePath,
				p.currentScope,
				field.Location.Start.Line,
				field.Location.End.Line,
				field.Location.Start.Column,
				field.Location.End.Column,
			)
			return nil, false
		}

		fields = append(fields, *field)

		if p.match(lexer.CLOSE_CURLY) {
			break
		} else {
			comma := p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_BRACE)
			if p.match(lexer.CLOSE_CURLY) {
				report.Add(p.filePath, comma.Start.Line, comma.End.Line, comma.Start.Column, comma.End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.WARNING)
				break
			}
		}
	}

	end := p.consume(lexer.CLOSE_CURLY, report.EXPECTED_CLOSE_BRACE).End

	return &ast.StructType{
		Fields:   fields,
		TypeName: types.STRUCT,
		Location: ast.Location{
			Start: &start,
			End:   &end,
		},
	}, true
}

func parseInterfaceType(p *Parser) (ast.DataType, bool) {

	start := p.consume(lexer.INTERFACE_TOKEN, report.EXPECTED_INTERFACE_KEYWORD)

	//create a new scope for the interface
	p.enterScope(symboltable.INTERFACE_SCOPE)
	defer p.exitScope()

	//consume the '{' token
	p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE)

	methods := make([]ast.InterfaceMethod, 0)

	for !p.match(lexer.CLOSE_CURLY) {

		start := p.consume(lexer.FUNCTION_TOKEN, report.EXPECTED_FUNCTION_KEYWORD).Start

		name := declareFunction(p)

		//start a new scope
		p.enterScope(symboltable.FUNCTION_SCOPE)
		defer p.exitScope()

		params, returnTypes := parseSignature(p, true)

		end := p.previous().End

		method := ast.InterfaceMethod{
			Name:       *name,
			Params:     params,
			ReturnType: returnTypes,
			Location:   ast.Location{Start: &start, End: &end},
		}

		// check if the method name is already declared in the interface
		if utils.Has(methods, method, func(a ast.InterfaceMethod, b ast.InterfaceMethod) bool {
			return a.Name.Name == b.Name.Name
		}) {
			report.Add(p.filePath, method.Location.Start.Line, method.Location.End.Line, method.Location.Start.Column, method.Location.End.Column, report.DUPLICATE_METHOD_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil, false
		}

		methods = append(methods, method)

		if p.match(lexer.CLOSE_CURLY) {
			break
		} else {
			//must be a comma
			comma := p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_BRACE)
			if p.match(lexer.CLOSE_CURLY) {
				report.Add(p.filePath, comma.Start.Line, comma.End.Line, comma.Start.Column, comma.End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.WARNING)
				break
			}
		}
	}

	end := p.consume(lexer.CLOSE_CURLY, "expected end of interface definition")

	return &ast.InterfaceType{
		Methods:  methods,
		TypeName: types.INTERFACE,
		Location: ast.Location{
			Start: &start.Start,
			End:   &end.End,
		},
	}, true
}

func parseFunctionTypeSignature(p *Parser) ([]ast.DataType, []ast.DataType) {
	// parse the parameters
	parameters := parseParameters(p)
	parameterTypes := make([]ast.DataType, len(parameters))
	// parse the return types
	returnTypes := parseReturnTypes(p)

	for i, parameter := range parameters {
		parameterTypes[i] = parameter.Type
	}

	return parameterTypes, returnTypes
}

func parseFunctionType(p *Parser) (ast.DataType, bool) {
	//consume the 'fn' token
	token := p.consume(lexer.FUNCTION_TOKEN, report.EXPECTED_FUNCTION_KEYWORD)

	// parse the parameters
	parameters, returnTypes := parseFunctionTypeSignature(p)

	return &ast.FunctionType{
		Parameters:  parameters,
		ReturnTypes: returnTypes,
		TypeName:    types.FUNCTION,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}, true
}

// parseType parses a type expression
func parseType(p *Parser) (ast.DataType, bool) {
	token := p.peek()
	switch token.Value {
	case string(types.INT8), string(types.INT16), string(types.INT32), string(types.INT64), string(types.UINT8), string(types.UINT16), string(types.UINT32), string(types.UINT64):
		return parseIntegerType(p)
	case string(types.FLOAT32), string(types.FLOAT64):
		return parseFloatType(p)
	case string(types.STRING):
		return parseStringType(p)
	case string(types.BYTE):
		return parseByteType(p)
	case string(types.BOOL):
		return parseBoolType(p)
	case string(lexer.OPEN_BRACKET):
		return parseArrayType(p)
	case string(types.STRUCT):
		return parseStructType(p)
	case string(types.INTERFACE):
		return parseInterfaceType(p)
	case string(types.FUNCTION):
		return parseFunctionType(p)
	default:
		return parseUserDefinedType(p)
	}
}

// parseTypeDecl parses type declarations like "type Integer i32;"
func parseTypeDecl(p *Parser) ast.Statement {
	start := p.advance() // consume the 'type' token

	typeName := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_TYPE_NAME)

	// Parse the underlying type
	underlyingType, ok := parseType(p)
	if !ok {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			report.EXPECTED_TYPE).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	// Add type to symbol table
	sym := &symboltable.Symbol{
		Name:       typeName.Value,
		SymbolKind: symboltable.TYPE_SYMBOL,
		Type:       underlyingType.Type(),
		IsMutable:  false,
		Location: symboltable.SymbolLocation{
			File:   p.filePath,
			Line:   typeName.Start.Line,
			Column: typeName.Start.Column,
		},
	}

	if !p.currentScope.Define(sym) {
		report.ShowRedeclarationError(
			typeName.Value,
			p.filePath,
			p.currentScope,
			typeName.Start.Line,
			typeName.End.Line,
			typeName.Start.Column,
			typeName.End.Column,
		)
		return nil
	}

	return &ast.TypeDeclStmt{
		Alias: &ast.IdentifierExpr{
			Name: typeName.Value,
			Location: ast.Location{
				Start: &typeName.Start,
				End:   &typeName.End,
			},
		},
		BaseType: underlyingType,
		Location: ast.Location{
			Start: &start.Start,
			End:   underlyingType.EndPos(),
		},
	}
}
