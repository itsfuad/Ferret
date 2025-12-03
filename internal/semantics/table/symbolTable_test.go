package table

import (
	"compiler/internal/semantics/symbols"
	"testing"
)

func newTestSymbol(name string) *symbols.Symbol {
	return &symbols.Symbol{Name: name}
}

func TestNewSymbolTable(t *testing.T) {
	st := NewSymbolTable(nil)
	if st == nil {
		t.Fatal("NewSymbolTable returned nil")
	}
	if st.parent != nil {
		t.Error("Expected parent to be nil")
	}
	if len(st.symbols) != 0 {
		t.Error("Expected symbols map to be empty")
	}
}

func TestDeclareAndGetSymbol(t *testing.T) {
	st := NewSymbolTable(nil)
	sym := newTestSymbol("foo")
	err := st.Declare("foo", sym)
	if err != nil {
		t.Fatalf("Declare failed: %v", err)
	}

	got, ok := st.GetSymbol("foo")
	if !ok {
		t.Error("GetSymbol did not find declared symbol")
	}
	if got != sym {
		t.Error("GetSymbol returned wrong symbol")
	}
}

func TestDeclareDuplicate(t *testing.T) {
	st := NewSymbolTable(nil)
	sym := newTestSymbol("bar")
	_ = st.Declare("bar", sym)
	err := st.Declare("bar", sym)
	if err == nil {
		t.Error("Expected error on duplicate declaration")
	}
}

func TestLookupCurrentScope(t *testing.T) {
	st := NewSymbolTable(nil)
	sym := newTestSymbol("baz")
	_ = st.Declare("baz", sym)

	got, ok := st.Lookup("baz")
	if !ok {
		t.Error("Lookup did not find symbol in current scope")
	}
	if got != sym {
		t.Error("Lookup returned wrong symbol")
	}
}

func TestLookupParentScope(t *testing.T) {
	parent := NewSymbolTable(nil)
	child := NewSymbolTable(parent)
	sym := newTestSymbol("qux")
	_ = parent.Declare("qux", sym)

	got, ok := child.Lookup("qux")
	if !ok {
		t.Error("Lookup did not find symbol in parent scope")
	}
	if got != sym {
		t.Error("Lookup returned wrong symbol from parent scope")
	}
}

func TestLookupNotFound(t *testing.T) {
	st := NewSymbolTable(nil)
	_, ok := st.Lookup("notfound")
	if ok {
		t.Error("Lookup should not find undeclared symbol")
	}
}

func TestGetSymbolOnlyCurrentScope(t *testing.T) {
	parent := NewSymbolTable(nil)
	child := NewSymbolTable(parent)
	sym := newTestSymbol("alpha")
	_ = parent.Declare("alpha", sym)

	_, ok := child.GetSymbol("alpha")
	if ok {
		t.Error("GetSymbol should not find symbol in parent scope")
	}
}
