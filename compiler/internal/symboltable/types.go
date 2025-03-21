package symboltable

type SymbolType interface {
	SymbolType()
}

type UserDefined struct {
	SymbolKind SYMBOL_KIND
	TypeName   string     // alias for the type name
	BaseType   SymbolType // underlying type
}

func (u *UserDefined) SymbolType() {} // implements SymbolType

type Int struct {
	BitSize    uint8
	IsUnsigned bool
}

func (i *Int) SymbolType() {} // implements SymbolType

type Float struct {
	BitSize uint8
}

func (f *Float) SymbolType() {} // implements SymbolType

type Bool struct {
	SymbolKind SYMBOL_KIND
}

func (b *Bool) SymbolType() {} // implements SymbolType

type String struct {
	SymbolKind SYMBOL_KIND
}

func (s *String) SymbolType() {} // implements SymbolType

type Array struct {
	SymbolKind  SYMBOL_KIND
	ElementType SymbolType
	NumElements int
}

func (a *Array) SymbolType() {} // implements SymbolType

type Function struct {
	SymbolKind SYMBOL_KIND
	Parameters []SymbolType
	ReturnType SymbolType
}
