package diagnostics

// Error codes for Ferret compiler
const (

	ErrMax				  = "E0010"

	// Lexer errors (L prefix)
	ErrUnexpectedCharacter = "L0001"
	ErrUnterminatedString  = "L0002"
	ErrInvalidNumber       = "L0003"
	ErrInvalidEscape       = "L0004"
	ErrUnterminatedComment = "L0005"
	ErrUnterminatedBlock   = "L0006"
	ErrUnterminatedExpr    = "L0007"

	// Parser errors (P prefix)
	ErrUnexpectedToken    = "P0001"
	ErrExpectedToken      = "P0002"
	ErrInvalidExpression  = "P0003"
	ErrInvalidStatement   = "P0004"
	ErrInvalidDeclaration = "P0005"
	ErrMissingIdentifier  = "P0006"
	ErrMissingType        = "P0007"
	ErrMissingInitializer = "P0008"
	ErrMissingSemiCol	  = "P0009"

	// Type checker errors (T prefix)
	ErrTypeMismatch            = "T0001"
	ErrUndefinedSymbol         = "T0002"
	ErrRedeclaredSymbol        = "T0003"
	ErrInvalidOperation        = "T0004"
	ErrNotCallable             = "T0005"
	ErrWrongArgumentCount      = "T0006"
	ErrInvalidAssignment       = "T0007"
	ErrNotIndexable            = "T0008"
	ErrFieldNotFound           = "T0009"
	ErrMethodNotFound          = "T0010"
	ErrInterfaceNotImplemented = "T0011"
	ErrCircularDependency      = "T0012"
	ErrInvalidCast             = "T0013"
	ErrNonExhaustiveMatch      = "T0014"
	ErrInvalidReturn           = "T0015"
	ErrMissingReturn           = "T0016"
	ErrConstantReassignment    = "T0017"
	ErrInvalidBreak            = "T0018"
	ErrInvalidContinue         = "T0019"
	ErrInvalidType 			   = "T0020"
	ErrInvalidMethodReceiver   = "T0021"

	// Module/Import errors (M prefix)
	ErrModuleNotFound    = "M0001"
	ErrCyclicImport      = "M0002"
	ErrInvalidImportPath = "M0003"
	ErrSymbolNotExported = "M0004"
	ErrAmbiguousImport   = "M0005"

	// Style/Info codes (S prefix)
	InfoTrailingComma        = "S0001"
	InfoUnnecessarySemicolon = "S0002"

	// Warnings (W prefix)
	WarnUnreachableCode = "W0001"
)
