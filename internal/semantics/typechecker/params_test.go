package typechecker

import (
	"strings"
	"testing"
)

// Test variadic parameter must be last
func TestVariadicParameterMustBeLast(t *testing.T) {
	src := `fn test(args: ...i32, x: i32) -> i32 {
		return x;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	ValidateFunctionParams(ctx, mod, funcDecl)

	// Should report variadic parameter not at end
	diags := ctx.Diagnostics.Diagnostics()
	hasVariadicError := false
	for _, diag := range diags {
		if strings.Contains(diag.Message, "variadic") && strings.Contains(diag.Message, "last") {
			hasVariadicError = true
			break
		}
	}

	if !hasVariadicError {
		t.Error("Expected 'variadic parameter must be last' error")
	}
}

// Test multiple variadic parameters
func TestMultipleVariadicParameters(t *testing.T) {
	src := `fn test(args1: ...i32, args2: ...i32) -> i32 {
		return 0;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	ValidateFunctionParams(ctx, mod, funcDecl)

	// Should report multiple variadic parameters
	diags := ctx.Diagnostics.Diagnostics()
	hasMultipleVariadic := false
	for _, diag := range diags {
		if strings.Contains(diag.Message, "multiple") && strings.Contains(diag.Message, "variadic") {
			hasMultipleVariadic = true
			break
		}
	}

	if !hasMultipleVariadic {
		t.Error("Expected 'multiple variadic parameters' error")
	}
}

// Test valid variadic parameter
func TestValidVariadicParameter(t *testing.T) {
	src := `fn test(x: i32, y: i32, args: ...i32) -> i32 {
		return x + y;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	ValidateFunctionParams(ctx, mod, funcDecl)

	// Should not report any errors
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "variadic") {
			t.Errorf("Valid variadic parameter, unexpected error: %s", diag.Message)
		}
	}
}

// Test duplicate parameter names
func TestDuplicateParameterNames(t *testing.T) {
	src := `fn test(x: i32, x: i32) -> i32 {
		return x;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	ValidateFunctionParams(ctx, mod, funcDecl)

	// Should report duplicate parameter name
	diags := ctx.Diagnostics.Diagnostics()
	hasDuplicate := false
	for _, diag := range diags {
		if strings.Contains(diag.Message, "duplicate") && strings.Contains(diag.Message, "parameter") {
			hasDuplicate = true
			break
		}
	}

	if !hasDuplicate {
		t.Error("Expected 'duplicate parameter name' error")
	}
}

// Test no duplicate with different names
func TestNoDuplicateParameterNames(t *testing.T) {
	src := `fn test(x: i32, y: i32, z: i32) -> i32 {
		return x + y + z;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	ValidateFunctionParams(ctx, mod, funcDecl)

	// Should not report any errors
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "duplicate") {
			t.Errorf("No duplicate parameters, unexpected error: %s", diag.Message)
		}
	}
}

// Test parameter without type annotation
func TestParameterMissingType(t *testing.T) {
	t.Skip("Parser may not support parameters without types yet")
	// Note: This test assumes the parser allows parameters without types
	// If the parser enforces type annotations, this test may need adjustment
	src := `fn test(x, y: i32) -> i32 {
		return y;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	ValidateFunctionParams(ctx, mod, funcDecl)

	// May report missing type annotation depending on parser
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "type") {
			// Expected if parser allows it
			return
		}
	}
}

// Test function with no parameters
func TestNoParameters(t *testing.T) {
	src := `fn test() -> i32 {
		return 42;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	ValidateFunctionParams(ctx, mod, funcDecl)

	// Should not report any errors
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if strings.Contains(diag.Message, "parameter") {
			t.Errorf("Function with no parameters, unexpected error: %s", diag.Message)
		}
	}
}

// Test anonymous function with parameters
func TestAnonymousFunctionParameters(t *testing.T) {
	t.Skip("Anonymous functions at top level not yet supported by parser")
	src := `fn (x: i32, y: i32) -> i32 {
		return x + y;
	}`

	funcDecl, mod := parseFunction(t, src)
	ctx := createTestContext()

	ValidateFunctionParams(ctx, mod, funcDecl)

	// Should not report any errors for valid anonymous function
	diags := ctx.Diagnostics.Diagnostics()
	for _, diag := range diags {
		if diag.Severity.String() == "error" {
			t.Errorf("Valid anonymous function, unexpected error: %s", diag.Message)
		}
	}
}
