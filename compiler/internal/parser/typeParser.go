package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
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
func parseStructField(p *Parser) *ast.ObjectField {
	// Parse field name
	nameToken := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_FIELD_NAME+" got "+p.peek().Value)
	fieldName := nameToken.Value

	// Expect colon
	p.consume(lexer.COLON_TOKEN, report.EXPECTED_COLON)

	// Parse field type
	if fieldType, ok := parseType(p); !ok {
		return nil
	} else {
		return &ast.ObjectField{
			Name: fieldName,
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
	// Consume opening brace
	p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE)

	// Check for empty struct
	if p.peek().Kind == lexer.CLOSE_CURLY {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			report.EMPTY_STRUCT_NOT_ALLOWED).SetLevel(report.SYNTAX_ERROR)
		return nil, false
	}

	fields := make([]ast.ObjectField, 0)
	fieldNames := make(map[string]bool)

	for p.peek().Kind != lexer.CLOSE_CURLY {

		// Parse field
		field := parseStructField(p)
		if field == nil {
			return nil, false
		}

		// Check for duplicate field names
		if fieldNames[field.Name] {
			report.Add(p.filePath, field.Location.Start.Line, field.Location.End.Line,
				field.Location.Start.Column, field.Location.End.Column,
				report.DUPLICATE_FIELD_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil, false
		}
		fieldNames[field.Name] = true
		fields = append(fields, *field)

		if !p.match(lexer.CLOSE_CURLY) {
			p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_BRACE)
			if p.peek().Kind == lexer.CLOSE_CURLY {
				curr := p.peek()
				report.Add(p.filePath, curr.Start.Line, curr.End.Line, curr.Start.Column, curr.End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.SYNTAX_ERROR)
				return nil, false
			}
		} else {
			break
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
	token := p.advance()

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
