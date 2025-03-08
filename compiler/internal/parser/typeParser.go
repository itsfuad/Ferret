package parser

import (
	"ferret/compiler/internal/ast"
	"ferret/compiler/internal/types"
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
			Start:    token.Start,
			End:      token.End,
		}
	case string(types.INT16):
		p.advance()
		return &ast.IntType{
			TypeName: types.INT16,
			BitSize:  16,
			Unsigned: true,
			Start:    token.Start,
			End:      token.End,
		}
	case string(types.INT32):
		p.advance()
		return &ast.IntType{
			TypeName: types.INT32,
			BitSize:  32,
			Unsigned: true,
			Start:    token.Start,
			End:      token.End,
		}
	case string(types.INT64):
		p.advance()
		return &ast.IntType{
			TypeName: types.INT64,
			BitSize:  64,
			Unsigned: true,
			Start:    token.Start,
			End:      token.End,
		}
	case string(types.UINT8):
		p.advance()
		return &ast.IntType{
			TypeName: types.UINT8,
			BitSize:  8,
			Unsigned: false,
			Start:    token.Start,
			End:      token.End,
		}
	case string(types.UINT16):
		p.advance()
		return &ast.IntType{
			TypeName: types.UINT16,
			BitSize:  16,
			Unsigned: false,
			Start:    token.Start,
			End:      token.End,
		}
	case string(types.UINT32):
		p.advance()
		return &ast.IntType{
			TypeName: types.UINT32,
			BitSize:  32,
			Unsigned: false,
			Start:    token.Start,
			End:      token.End,
		}
	case string(types.UINT64):
		p.advance()
		return &ast.IntType{
			TypeName: types.UINT64,
			BitSize:  64,
			Unsigned: false,
			Start:    token.Start,
			End:      token.End,
		}
	default:
		return nil
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
			Start:    token.Start,
			End:      token.End,
		}
	case string(types.FLOAT64):
		p.advance()
		return &ast.FloatType{
			TypeName: types.FLOAT64,
			BitSize:  64,
			Start:    token.Start,
			End:      token.End,
		}
	}

	return nil
}

func parseStringType(p *Parser) ast.DataType {
	token := p.advance()
	return &ast.StringType{
		TypeName: types.STRING,
		Start:    token.Start,
		End:      token.End,
	}
}

func parseByteType(p *Parser) ast.DataType {
	token := p.advance()
	return &ast.ByteType{
		TypeName: types.BYTE,
		Start:    token.Start,
		End:      token.End,
	}
}

func parseBoolType(p *Parser) ast.DataType {
	token := p.advance()
	return &ast.BoolType{
		TypeName: types.BOOL,
		Start:    token.Start,
		End:      token.End,
	}
}

func parseArrayType(p *Parser) ast.DataType {
	//consume the '[' token
	start := p.advance().Start
	// consume the ']' token
	p.advance()

	//parse the type
	elementType := parseType(p)

	return &ast.ArrayType{
		ElementType: elementType,
		TypeName:    types.ARRAY,
		Start:       start,
		End:         elementType.EndPos(),
	}
}

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
	case "[":
		return parseArrayType(p)
	}
	return nil
}
