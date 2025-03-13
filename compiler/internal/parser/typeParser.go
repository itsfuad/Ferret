package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/lexer"
	"ferret/compiler/internal/types"
	"ferret/compiler/report"
	"fmt"
)

func parseIntegerType(p *Parser) ast.DataType {
	token := p.peek()

	switch token.Value {
	case string(types.INT8):
		p.advance()
		return &ast.IntType{
			TypeName: types.INT8,
			BitSize:  8,
			Unsigned: true,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	case string(types.INT16):
		p.advance()
		return &ast.IntType{
			TypeName: types.INT16,
			BitSize:  16,
			Unsigned: true,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	case string(types.INT32):
		p.advance()
		return &ast.IntType{
			TypeName: types.INT32,
			BitSize:  32,
			Unsigned: true,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	case string(types.INT64):
		p.advance()
		return &ast.IntType{
			TypeName: types.INT64,
			BitSize:  64,
			Unsigned: true,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	case string(types.UINT8):
		p.advance()
		return &ast.IntType{
			TypeName: types.UINT8,
			BitSize:  8,
			Unsigned: false,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	case string(types.UINT16):
		p.advance()
		return &ast.IntType{
			TypeName: types.UINT16,
			BitSize:  16,
			Unsigned: false,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	case string(types.UINT32):
		p.advance()
		return &ast.IntType{
			TypeName: types.UINT32,
			BitSize:  32,
			Unsigned: false,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	case string(types.UINT64):
		p.advance()
		return &ast.IntType{
			TypeName: types.UINT64,
			BitSize:  64,
			Unsigned: false,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	default:
		return nil
	}
}

// user defined types are defined by the type keyword
// type NewType OldType;
func parseUserDefinedType(p *Parser) ast.DataType {
	token := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_TYPE_NAME)
	return &ast.UserDefinedType{
		TypeName: types.TYPE_NAME(token.Value),
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}
}

func parseFloatType(p *Parser) ast.DataType {
	token := p.peek()

	switch token.Value {
	case string(types.FLOAT32):
		p.advance()
		return &ast.FloatType{
			TypeName: types.FLOAT32,
			BitSize:  32,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	case string(types.FLOAT64):
		p.advance()
		return &ast.FloatType{
			TypeName: types.FLOAT64,
			BitSize:  64,
			Location: ast.Location{
				Start: &token.Start,
				End:   &token.End,
			},
		}
	}

	return nil
}

func parseStringType(p *Parser) ast.DataType {
	token := p.advance()
	return &ast.StringType{
		TypeName: types.STRING,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}
}

func parseByteType(p *Parser) ast.DataType {
	token := p.advance()
	return &ast.ByteType{
		TypeName: types.BYTE,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}
}

func parseBoolType(p *Parser) ast.DataType {
	token := p.advance()
	return &ast.BoolType{
		TypeName: types.BOOL,
		Location: ast.Location{
			Start: &token.Start,
			End:   &token.End,
		},
	}
}

func parseArrayType(p *Parser) ast.DataType {
	//consume the '[' token
	start := p.advance().Start
	// consume the ']' token
	p.consume(lexer.CLOSE_BRACKET, report.EXPECTED_CLOSE_BRACKET)

	//parse the type
	elementType := parseType(p)
	if elementType == nil {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			report.INVALID_TYPE_NAME).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	return &ast.ArrayType{
		ElementType: elementType,
		TypeName:    types.ARRAY,
		Location: ast.Location{
			Start: &start,
			End:   elementType.EndPos(),
		},
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
	fieldType := parseType(p)
	if fieldType == nil {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			report.EXPECTED_FIELD_TYPE).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	return &ast.ObjectField{
		Name: fieldName,
		Type: fieldType,
		Location: ast.Location{
			Start: &nameToken.Start,
			End:   fieldType.EndPos(),
		},
	}
}

// parseStructType parses a struct type definition like struct { name: str, age: i32 }
func parseStructType(p *Parser) ast.DataType {
	// Consume 'struct' keyword
	fmt.Printf("Parsing struct type\nCurrent token: %v\n", p.peek())
	start := p.consume(lexer.STRUCT_TOKEN, report.EXPECTED_STRUCT_KEYWORD).Start

	// Consume opening brace
	p.consume(lexer.OPEN_CURLY, report.EXPECTED_OPEN_BRACE)

	// Check for empty struct
	if p.peek().Kind == lexer.CLOSE_CURLY {
		report.Add(p.filePath, p.peek().Start.Line, p.peek().End.Line,
			p.peek().Start.Column, p.peek().End.Column,
			report.EMPTY_STRUCT_NOT_ALLOWED).SetLevel(report.SYNTAX_ERROR)
		return nil
	}

	fields := make([]ast.ObjectField, 0)
	fieldNames := make(map[string]bool)

	for p.peek().Kind != lexer.CLOSE_CURLY {

		// Parse field
		field := parseStructField(p)
		if field == nil {
			return nil
		}

		// Check for duplicate field names
		if fieldNames[field.Name] {
			report.Add(p.filePath, field.Location.Start.Line, field.Location.End.Line,
				field.Location.Start.Column, field.Location.End.Column,
				report.DUPLICATE_FIELD_NAME).SetLevel(report.SYNTAX_ERROR)
			return nil
		}
		fieldNames[field.Name] = true
		fields = append(fields, *field)

		if !p.match(lexer.CLOSE_CURLY) {
			p.consume(lexer.COMMA_TOKEN, report.EXPECTED_COMMA_OR_CLOSE_BRACE)
			if p.peek().Kind == lexer.CLOSE_CURLY {
				curr := p.peek()
				report.Add(p.filePath, curr.Start.Line, curr.End.Line, curr.Start.Column, curr.End.Column, report.TRAILING_COMMA_NOT_ALLOWED).AddHint("Remove the trailing comma").SetLevel(report.SYNTAX_ERROR)
				return nil
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
	}
}

// parseType parses a type expression
func parseType(p *Parser) ast.DataType {
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
	default:
		return parseUserDefinedType(p)
	}
}

// parseTypeDecl parses type declarations like "type Integer i32;"
func parseTypeDecl(p *Parser) ast.Statement {
	start := p.advance() // consume the 'type' token

	typeName := p.consume(lexer.IDENTIFIER_TOKEN, report.EXPECTED_TYPE_NAME)
	// Parse the underlying type
	underlyingType := parseType(p)
	if underlyingType == nil {
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
