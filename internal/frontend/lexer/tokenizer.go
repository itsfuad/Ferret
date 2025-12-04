package lexer

import (
	"fmt"
	"regexp"

	"compiler/internal/diagnostics"
	"compiler/internal/source"
	"compiler/internal/tokens"
	"compiler/internal/utils/numeric"
)

type regexHandler func(lex *Lexer, regex *regexp.Regexp)

type regexPattern struct {
	regex   *regexp.Regexp
	handler regexHandler
}

type Lexer struct {
//	Errors     []error
	diagnostics *diagnostics.DiagnosticBag
	Tokens     []tokens.Token
	Position   source.Position
	sourceCode []byte
	patterns   []regexPattern
	FilePath   string
}

func (lex *Lexer) advance(match string) {
	lex.Position.Advance(match)
}

func (lex *Lexer) push(token tokens.Token) {
	lex.Tokens = append(lex.Tokens, token)
}

func (lex *Lexer) remainder() string {
	return string(lex.sourceCode)[lex.Position.Index:]
}

func (lex *Lexer) atEOF() bool {
	return lex.Position.Index >= len(lex.sourceCode)
}

func New(filepath, content string, diag *diagnostics.DiagnosticBag) *Lexer {
	//create the lexer
	lex := &Lexer{
		sourceCode: []byte(content),
		Tokens:     make([]tokens.Token, 0),
		Position: source.Position{
			Line:   1,
			Column: 1,
			Index:  0,
		},

		diagnostics: diag,

		FilePath: filepath,

		patterns: []regexPattern{
			//{regexp.MustCompile(`\n`), skipHandler}, // newlines
			{regexp.MustCompile(`\s+`), skipHandler},                          // whitespace
			{regexp.MustCompile(`\/\/.*`), skipHandler},                       // single line comments
			{regexp.MustCompile(`\/\*[\s\S]*?\*\/`), skipHandler},             // multi line comments
			{regexp.MustCompile(`"[^"]*"`), stringHandler},                    // string literals
			{regexp.MustCompile(`'[^']'`), byteHandler},                       // byte literals
			{regexp.MustCompile(numeric.NumberPattern), numberHandler},        // numbers (hex, octal, binary, float, integer)
			{regexp.MustCompile(`[a-zA-Z_][a-zA-Z0-9_]*`), identifierHandler}, // identifiers
			{regexp.MustCompile(`\+\+`), defaultHandler(tokens.PLUS_PLUS_TOKEN)},
			{regexp.MustCompile(`\-\-`), defaultHandler(tokens.MINUS_MINUS_TOKEN)},
			{regexp.MustCompile(`\->`), defaultHandler(tokens.ARROW_TOKEN)},
			{regexp.MustCompile(`\=>`), defaultHandler(tokens.FAT_ARROW_TOKEN)},
			{regexp.MustCompile(`::`), defaultHandler(tokens.SCOPE_TOKEN)}, // Add scope resolution operator before single colon
			{regexp.MustCompile(`!=`), defaultHandler(tokens.NOT_EQUAL_TOKEN)},
			{regexp.MustCompile(`\+=`), defaultHandler(tokens.PLUS_EQUALS_TOKEN)},
			{regexp.MustCompile(`-=`), defaultHandler(tokens.MINUS_EQUALS_TOKEN)},
			{regexp.MustCompile(`\*=`), defaultHandler(tokens.MUL_EQUALS_TOKEN)},
			{regexp.MustCompile(`/=`), defaultHandler(tokens.DIV_EQUALS_TOKEN)},
			{regexp.MustCompile(`%=`), defaultHandler(tokens.MOD_EQUALS_TOKEN)},
			{regexp.MustCompile(`\^=`), defaultHandler(tokens.EXP_EQUALS_TOKEN)},
			{regexp.MustCompile(`\*\*`), defaultHandler(tokens.EXP_TOKEN)},
			{regexp.MustCompile(`\.\.\.`), defaultHandler(tokens.THREE_DOT_TOKEN)},
			{regexp.MustCompile(`\.\.`), defaultHandler(tokens.RANGE_TOKEN)},
			{regexp.MustCompile(`&&`), defaultHandler(tokens.AND_TOKEN)},
			{regexp.MustCompile(`\|\|`), defaultHandler(tokens.OR_TOKEN)},
			{regexp.MustCompile(`&`), defaultHandler(tokens.BIT_AND_TOKEN)},
			{regexp.MustCompile(`\|`), defaultHandler(tokens.BIT_OR_TOKEN)},
			{regexp.MustCompile(`\^`), defaultHandler(tokens.BIT_XOR_TOKEN)},
			{regexp.MustCompile(`!`), defaultHandler(tokens.NOT_TOKEN)},
			{regexp.MustCompile(`\-`), defaultHandler(tokens.MINUS_TOKEN)},
			{regexp.MustCompile(`\+`), defaultHandler(tokens.PLUS_TOKEN)},
			{regexp.MustCompile(`\*`), defaultHandler(tokens.MUL_TOKEN)},
			{regexp.MustCompile(`/`), defaultHandler(tokens.DIV_TOKEN)},
			{regexp.MustCompile(`%`), defaultHandler(tokens.MOD_TOKEN)},
			{regexp.MustCompile(`<=`), defaultHandler(tokens.LESS_EQUAL_TOKEN)},
			{regexp.MustCompile(`<`), defaultHandler(tokens.LESS_TOKEN)},
			{regexp.MustCompile(`>=`), defaultHandler(tokens.GREATER_EQUAL_TOKEN)},
			{regexp.MustCompile(`>`), defaultHandler(tokens.GREATER_TOKEN)},
			{regexp.MustCompile(`==`), defaultHandler(tokens.DOUBLE_EQUAL_TOKEN)},
			{regexp.MustCompile(`:=`), defaultHandler(tokens.WALRUS_TOKEN)},
			{regexp.MustCompile(`=`), defaultHandler(tokens.EQUALS_TOKEN)},
			{regexp.MustCompile(`:`), defaultHandler(tokens.COLON_TOKEN)},
			{regexp.MustCompile(`;`), defaultHandler(tokens.SEMICOLON_TOKEN)},
			{regexp.MustCompile(`\(`), defaultHandler(tokens.OPEN_PAREN)},
			{regexp.MustCompile(`\)`), defaultHandler(tokens.CLOSE_PAREN)},
			{regexp.MustCompile(`\[`), defaultHandler(tokens.OPEN_BRACKET)},
			{regexp.MustCompile(`\]`), defaultHandler(tokens.CLOSE_BRACKET)},
			{regexp.MustCompile(`\{`), defaultHandler(tokens.OPEN_CURLY)},
			{regexp.MustCompile(`\}`), defaultHandler(tokens.CLOSE_CURLY)},
			{regexp.MustCompile(","), defaultHandler(tokens.COMMA_TOKEN)},
			{regexp.MustCompile(`\.`), defaultHandler(tokens.DOT_TOKEN)},
			{regexp.MustCompile(`\?:`), defaultHandler(tokens.ELVIS_TOKEN)},
			{regexp.MustCompile(`\?`), defaultHandler(tokens.QUESTION_TOKEN)},
		},
	}
	return lex
}

func defaultHandler(token tokens.TOKEN) regexHandler {

	return func(lex *Lexer, _ *regexp.Regexp) {

		start := lex.Position
		lex.advance(string(token))
		end := lex.Position

		lex.push(tokens.NewToken(token, string(token), start, end))
	}
}

func identifierHandler(lex *Lexer, regex *regexp.Regexp) {
	identifier := regex.FindString(lex.remainder())
	start := lex.Position
	lex.advance(identifier)
	end := lex.Position
	if tokens.IsKeyword(identifier) {
		lex.push(tokens.NewToken(tokens.TOKEN(identifier), identifier, start, end))
	} else {
		lex.push(tokens.NewToken(tokens.IDENTIFIER_TOKEN, identifier, start, end))
	}
}

func numberHandler(lex *Lexer, regex *regexp.Regexp) {
	match := regex.FindString(lex.remainder())
	start := lex.Position
	lex.advance(match)
	end := lex.Position
	//find the number is a float or an integer
	lex.push(tokens.NewToken(tokens.NUMBER_TOKEN, match, start, end))
}

func stringHandler(lex *Lexer, regex *regexp.Regexp) {
	match := regex.FindString(lex.remainder())
	//exclude the quotes
	stringLiteral := match[1 : len(match)-1]
	start := lex.Position
	lex.advance(match)
	end := lex.Position
	lex.push(tokens.NewToken(tokens.STRING_TOKEN, stringLiteral, start, end))
}

func byteHandler(lex *Lexer, regex *regexp.Regexp) {
	match := regex.FindString(lex.remainder())
	//exclude the quotes
	byteLiteral := match[1 : len(match)-1]
	start := lex.Position
	lex.advance(match)
	end := lex.Position
	lex.push(tokens.NewToken(tokens.BYTE_TOKEN, byteLiteral, start, end))
}

// skipHandler processes a token that should be skipped by the lexer.
func skipHandler(lex *Lexer, regex *regexp.Regexp) {
	match := regex.FindString(lex.remainder())
	lex.advance(match)
}

// Tokenize reads the source code from the specified file and tokenizes it.
func (lex *Lexer) Tokenize(debug bool) []tokens.Token {

	for !lex.atEOF() {

		matched := false

		for _, pattern := range lex.patterns {

			loc := pattern.regex.FindStringIndex(lex.remainder())

			if loc != nil && loc[0] == 0 {
				pattern.handler(lex, pattern.regex)
				matched = true
				break
			}
		}

		if !matched {
			// Add error diagnostic instead of panic
			tok := lex.remainder()[0]
			errMsg := fmt.Sprintf("unrecognized character '%c'", tok)
			lex.diagnostics.Add(
				diagnostics.NewError(errMsg).WithPrimaryLabel(source.NewLocation(
					&lex.FilePath,
					&lex.Position,
					&lex.Position,
				), ""),
			)
			// Skip the bad character and continue tokenizing to find more errors
			lex.advance(string(tok))
		}
	}

	lex.push(tokens.NewToken(tokens.EOF_TOKEN, "end of file", lex.Position, lex.Position))

	if debug {
		for _, token := range lex.Tokens {
			token.Debug(lex.FilePath)
		}
	}

	return lex.Tokens
}
