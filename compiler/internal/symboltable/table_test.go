package symboltable

import (
	"ferret/compiler/internal/types"
	"testing"
)

func TestSymbolTableBasics(t *testing.T) {
	t.Run("create new symbol table", func(t *testing.T) {
		st := NewSymbolTable(nil, GLOBAL_SCOPE)
		if st == nil {
			t.Error("NewSymbolTable returned nil")
		}
		if st.ScopeKind() != GLOBAL_SCOPE {
			t.Errorf("Expected GLOBAL_SCOPE, got %v", st.ScopeKind())
		}
	})

	t.Run("define and resolve symbol", func(t *testing.T) {
		st := NewSymbolTable(nil, GLOBAL_SCOPE)
		sym := &Symbol{
			Name:      "x",
			Kind:      VARIABLE_SYMBOL,
			Type:      types.INT32,
			Scope:     GLOBAL_SCOPE,
			IsMutable: true,
			Location:  SymbolLocation{File: "test.fer", Line: 1, Column: 1},
		}

		if !st.Define(sym) {
			t.Error("Failed to define symbol")
		}

		resolved, found := st.Resolve("x")
		if !found {
			t.Error("Failed to resolve symbol")
		}
		if resolved.Name != "x" || resolved.Type != types.INT32 {
			t.Errorf("Resolved symbol mismatch: got %v", resolved)
		}
	})

	t.Run("prevent duplicate symbols in same scope", func(t *testing.T) {
		st := NewSymbolTable(nil, GLOBAL_SCOPE)
		sym := &Symbol{Name: "x", Kind: VARIABLE_SYMBOL, Type: types.INT32}
		
		if !st.Define(sym) {
			t.Error("Failed to define first symbol")
		}
		if st.Define(sym) {
			t.Error("Should not allow duplicate symbol definition")
		}
	})
}

func TestScopeHierarchy(t *testing.T) {
	t.Run("nested scope resolution", func(t *testing.T) {
		global := NewSymbolTable(nil, GLOBAL_SCOPE)
		function := global.EnterScope(FUNCTION_SCOPE)
		block := function.EnterScope(BLOCK_SCOPE)

		// Define in different scopes
		global.Define(&Symbol{Name: "g", Kind: VARIABLE_SYMBOL, Type: types.INT32})
		function.Define(&Symbol{Name: "f", Kind: VARIABLE_SYMBOL, Type: types.INT32})
		block.Define(&Symbol{Name: "b", Kind: VARIABLE_SYMBOL, Type: types.INT32})

		// Test resolution from innermost scope
		tests := []struct {
			name     string
			wantFind bool
		}{
			{"g", true},  // Should find in global
			{"f", true},  // Should find in function
			{"b", true},  // Should find in block
			{"x", false}, // Should not find
		}

		for _, tt := range tests {
			t.Run(tt.name, func(t *testing.T) {
				_, found := block.Resolve(tt.name)
				if found != tt.wantFind {
					t.Errorf("Resolve(%q) = %v, want %v", tt.name, found, tt.wantFind)
				}
			})
		}
	})

	t.Run("scope navigation", func(t *testing.T) {
		global := NewSymbolTable(nil, GLOBAL_SCOPE)
		function := global.EnterScope(FUNCTION_SCOPE)
		block := function.EnterScope(BLOCK_SCOPE)

		if block.ExitScope() != function {
			t.Error("ExitScope from block should return function scope")
		}
		if function.ExitScope() != global {
			t.Error("ExitScope from function should return global scope")
		}
		if global.ExitScope() != global {
			t.Error("ExitScope from global should return global scope")
		}
	})
}

func TestSymbolKinds(t *testing.T) {
	st := NewSymbolTable(nil, GLOBAL_SCOPE)

	tests := []struct {
		name string
		sym  *Symbol
	}{
		{
			"variable",
			&Symbol{
				Name:      "x",
				Kind:      VARIABLE_SYMBOL,
				Type:      types.INT32,
				IsMutable: true,
			},
		},
		{
			"constant",
			&Symbol{
				Name:      "MAX",
				Kind:      CONST_SYMBOL,
				Type:      types.INT32,
				IsMutable: false,
				Value:     100,
			},
		},
		{
			"function",
			&Symbol{
				Name: "add",
				Kind: FUNCTION_SYMBOL,
				Type: types.FUNCTION,
			},
		},
		{
			"type",
			&Symbol{
				Name: "Point",
				Kind: TYPE_SYMBOL,
				Type: types.STRUCT,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if !st.Define(tt.sym) {
				t.Errorf("Failed to define %s symbol", tt.name)
			}
			resolved, found := st.Resolve(tt.sym.Name)
			if !found {
				t.Errorf("Failed to resolve %s symbol", tt.name)
			}
			if resolved.Kind != tt.sym.Kind {
				t.Errorf("Wrong kind for %s: got %v, want %v", tt.name, resolved.Kind, tt.sym.Kind)
			}
		})
	}
}

func TestStructScope(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE)
	structScope := global.EnterScope(STRUCT_SCOPE)
	structScope.SetStructName("Point")

	// Define struct fields
	fields := []struct {
		name string
		typ  types.TYPE_NAME
	}{
		{"x", types.INT32},
		{"y", types.INT32},
	}

	for _, f := range fields {
		sym := &Symbol{
			Name:  f.name,
			Kind:  STRUCT_FIELD_SYMBOL,
			Type:  f.typ,
			Scope: STRUCT_SCOPE,
		}
		if !structScope.Define(sym) {
			t.Errorf("Failed to define field %s", f.name)
		}
	}

	// Test field resolution
	for _, f := range fields {
		resolved, found := structScope.ResolveLocal(f.name)
		if !found {
			t.Errorf("Failed to resolve field %s", f.name)
		}
		if resolved.Kind != STRUCT_FIELD_SYMBOL {
			t.Errorf("Wrong kind for field %s: got %v, want %v", f.name, resolved.Kind, STRUCT_FIELD_SYMBOL)
		}
		if resolved.Type != f.typ {
			t.Errorf("Wrong type for field %s: got %v, want %v", f.name, resolved.Type, f.typ)
		}
	}

	if structScope.structName != "Point" {
		t.Errorf("Wrong struct name: got %v, want %v", structScope.structName, "Point")
	}
}

func TestFunctionScope(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE)
	funcScope := global.EnterScope(FUNCTION_SCOPE)
	funcScope.SetFunctionName("add")

	// Define parameters
	params := []struct {
		name string
		typ  types.TYPE_NAME
	}{
		{"a", types.INT32},
		{"b", types.INT32},
	}

	for _, p := range params {
		sym := &Symbol{
			Name:  p.name,
			Kind:  PARAMETER_SYMBOL,
			Type:  p.typ,
			Scope: FUNCTION_SCOPE,
		}
		if !funcScope.Define(sym) {
			t.Errorf("Failed to define parameter %s", p.name)
		}
	}

	// Test parameter resolution
	for _, p := range params {
		resolved, found := funcScope.ResolveLocal(p.name)
		if !found {
			t.Errorf("Failed to resolve parameter %s", p.name)
		}
		if resolved.Kind != PARAMETER_SYMBOL {
			t.Errorf("Wrong kind for parameter %s: got %v, want %v", p.name, resolved.Kind, PARAMETER_SYMBOL)
		}
		if resolved.Type != p.typ {
			t.Errorf("Wrong type for parameter %s: got %v, want %v", p.name, resolved.Type, p.typ)
		}
	}

	if funcScope.functionName != "add" {
		t.Errorf("Wrong function name: got %v, want %v", funcScope.functionName, "add")
	}
}

func TestScopeClear(t *testing.T) {
	st := NewSymbolTable(nil, GLOBAL_SCOPE)
	sym := &Symbol{Name: "x", Kind: VARIABLE_SYMBOL, Type: types.INT32}
	
	if !st.Define(sym) {
		t.Error("Failed to define symbol")
	}
	
	st.Clear()
	
	if _, found := st.ResolveLocal("x"); found {
		t.Error("Symbol should not exist after Clear()")
	}
}

func TestChildScopes(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE)
	
	// Create multiple child scopes
	function1 := global.EnterScope(FUNCTION_SCOPE)
	function2 := global.EnterScope(FUNCTION_SCOPE)
	
	children := global.Children()
	if len(children) != 2 {
		t.Errorf("Expected 2 child scopes, got %d", len(children))
	}
	
	if children[0] != function1 || children[1] != function2 {
		t.Error("Child scopes not properly tracked")
	}
}

func TestShadowing(t *testing.T) {
	t.Run("variable shadowing", func(t *testing.T) {
		global := NewSymbolTable(nil, GLOBAL_SCOPE)
		block := global.EnterScope(BLOCK_SCOPE)

		// Define in global scope
		global.Define(&Symbol{
			Name:      "x",
			Kind:      VARIABLE_SYMBOL,
			Type:      types.INT32,
			Value:     1,
			IsMutable: true,
		})

		// Shadow in block scope
		block.Define(&Symbol{
			Name:      "x",
			Kind:      VARIABLE_SYMBOL,
			Type:      types.INT64,
			Value:     2,
			IsMutable: true,
		})

		// Check block scope gets shadowed variable
		blockSym, found := block.ResolveLocal("x")
		if !found {
			t.Error("Failed to resolve shadowed symbol in block scope")
		}
		if blockSym.Type != types.INT64 {
			t.Errorf("Wrong type for shadowed symbol: got %v, want %v", blockSym.Type, types.INT64)
		}

		// Check global scope still has original
		globalSym, found := global.ResolveLocal("x")
		if !found {
			t.Error("Failed to resolve original symbol in global scope")
		}
		if globalSym.Type != types.INT32 {
			t.Errorf("Wrong type for original symbol: got %v, want %v", globalSym.Type, types.INT32)
		}
	})
}

func TestModuleScope(t *testing.T) {
	t.Run("module symbols", func(t *testing.T) {
		module := NewSymbolTable(nil, MODULE_SCOPE)

		// Test exported and non-exported symbols
		tests := []struct {
			name       string
			isExported bool
		}{
			{"privateVar", false},
			{"PublicVar", true},
			{"_hidden", false},
			{"ExportedFn", true},
		}

		for _, tt := range tests {
			sym := &Symbol{
				Name:       tt.name,
				Kind:      VARIABLE_SYMBOL,
				Type:      types.INT32,
				IsExported: tt.isExported,
			}
			if !module.Define(sym) {
				t.Errorf("Failed to define symbol %s", tt.name)
			}

			resolved, found := module.ResolveLocal(tt.name)
			if !found {
				t.Errorf("Failed to resolve symbol %s", tt.name)
			}
			if resolved.IsExported != tt.isExported {
				t.Errorf("Wrong export status for %s: got %v, want %v", tt.name, resolved.IsExported, tt.isExported)
			}
		}
	})
}

func TestSymbolMutability(t *testing.T) {
	st := NewSymbolTable(nil, GLOBAL_SCOPE)

	tests := []struct {
		name      string
		kind      SymbolKind
		isMutable bool
	}{
		{"var", VARIABLE_SYMBOL, true},
		{"CONST", CONST_SYMBOL, false},
		{"fn", FUNCTION_SYMBOL, false},
		{"Type", TYPE_SYMBOL, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sym := &Symbol{
				Name:      tt.name,
				Kind:      tt.kind,
				Type:      types.INT32,
				IsMutable: tt.isMutable,
			}
			if !st.Define(sym) {
				t.Errorf("Failed to define %s symbol", tt.name)
			}

			resolved, found := st.Resolve(tt.name)
			if !found {
				t.Errorf("Failed to resolve %s symbol", tt.name)
			}
			if resolved.IsMutable != tt.isMutable {
				t.Errorf("Wrong mutability for %s: got %v, want %v", tt.name, resolved.IsMutable, tt.isMutable)
			}
		})
	}
}

func TestNestedFunctionScopes(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE)
	outer := global.EnterScope(FUNCTION_SCOPE)
	outer.SetFunctionName("outer")
	inner := outer.EnterScope(FUNCTION_SCOPE)
	inner.SetFunctionName("inner")

	// Define in outer scope
	outer.Define(&Symbol{
		Name:  "x",
		Kind:  VARIABLE_SYMBOL,
		Type:  types.INT32,
		Scope: FUNCTION_SCOPE,
	})

	// Define in inner scope
	inner.Define(&Symbol{
		Name:  "y",
		Kind:  VARIABLE_SYMBOL,
		Type:  types.INT32,
		Scope: FUNCTION_SCOPE,
	})

	// Inner scope should see both variables
	if _, found := inner.Resolve("x"); !found {
		t.Error("Inner scope cannot see outer variable")
	}
	if _, found := inner.Resolve("y"); !found {
		t.Error("Inner scope cannot see own variable")
	}

	// Outer scope should only see its own variable
	if _, found := outer.Resolve("y"); found {
		t.Error("Outer scope should not see inner variable")
	}
}

func TestSymbolLocation(t *testing.T) {
	st := NewSymbolTable(nil, GLOBAL_SCOPE)
	
	loc := SymbolLocation{
		File:   "test.fer",
		Line:   10,
		Column: 5,
	}
	
	sym := &Symbol{
		Name:     "test",
		Kind:     VARIABLE_SYMBOL,
		Type:     types.INT32,
		Location: loc,
	}
	
	if !st.Define(sym) {
		t.Error("Failed to define symbol with location")
	}
	
	resolved, found := st.Resolve("test")
	if !found {
		t.Error("Failed to resolve symbol")
	}
	
	if resolved.Location.File != loc.File ||
		resolved.Location.Line != loc.Line ||
		resolved.Location.Column != loc.Column {
		t.Errorf("Location mismatch: got %+v, want %+v", resolved.Location, loc)
	}
}

func TestScopeNesting(t *testing.T) {
	global := NewSymbolTable(nil, GLOBAL_SCOPE)
	
	// Create a deeply nested scope structure
	current := global
	scopes := []ScopeKind{
		MODULE_SCOPE,
		FUNCTION_SCOPE,
		BLOCK_SCOPE,
		STRUCT_SCOPE,
		BLOCK_SCOPE,
	}
	
	for _, kind := range scopes {
		current = current.EnterScope(kind)
		if current.ScopeKind() != kind {
			t.Errorf("Wrong scope kind: got %v, want %v", current.ScopeKind(), kind)
		}
	}
	
	// Test walking back up the scope chain
	for i := len(scopes) - 1; i >= 0; i-- {
		current = current.ExitScope()
		expectedKind := GLOBAL_SCOPE
		if i > 0 {
			expectedKind = scopes[i-1]
		}
		if current.ScopeKind() != expectedKind {
			t.Errorf("Wrong scope kind after exit: got %v, want %v", current.ScopeKind(), expectedKind)
		}
	}
} 