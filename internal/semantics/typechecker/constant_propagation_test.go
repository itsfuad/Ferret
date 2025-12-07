package typechecker

import (
	"compiler/internal/diagnostics"
	"strings"
	"testing"
)

func TestConstantPropagationArrayBounds(t *testing.T) {
	tests := []struct {
		name          string
		src           string
		expectError   bool
		errorContains string
	}{
		{
			name: "Constant variable out of bounds",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let i := 10;
				let x := arr[i];
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "Constant variable within bounds",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let i := 3;
				let x := arr[i];
			}`,
			expectError: false,
		},
		{
			name: "Arithmetic expression with constant",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let i := 2;
				let x := arr[i + 1];
			}`,
			expectError: false,
		},
		{
			name: "Arithmetic expression out of bounds",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let i := 3;
				let x := arr[i + 5];
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "Negative constant variable",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let i := -1;
				let x := arr[i];
			}`,
			expectError: false,
		},
		{
			name: "Negative constant variable out of bounds",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let i := -10;
				let x := arr[i];
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "Non-constant variable - no error",
			src: `fn test(n: i32) {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				let i := n;
				let x := arr[i];
			}`,
			expectError: false,
		},
		{
			name: "Constant multiplication",
			src: `fn test() {
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let i := 2;
				let x := arr[i * 3];
			}`,
			expectError: false,
		},
		{
			name: "Const declaration out of bounds",
			src: `fn test() {
				let arr: [5]i32 = [1, 2, 3, 4, 5];
				const INDEX := 100;
				let x := arr[INDEX];
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, ctx := parseCollectAndCheck(t, tt.src)

			diags := ctx.Diagnostics.Diagnostics()
			hasError := false
			foundExpectedError := false

			for _, d := range diags {
				if d.Severity == diagnostics.Error {
					hasError = true
					if tt.errorContains != "" && strings.Contains(d.Message, tt.errorContains) {
						foundExpectedError = true
					}
					t.Logf("Error: %s", d.Message)
				}
			}

			if tt.expectError && !hasError {
				t.Errorf("Expected error but got none")
			}

			if !tt.expectError && hasError {
				t.Errorf("Expected no error but got errors")
			}

			if tt.expectError && tt.errorContains != "" && !foundExpectedError {
				t.Errorf("Expected error containing '%s' but didn't find it", tt.errorContains)
			}
		})
	}
}

func TestConstantPropagationComparison(t *testing.T) {
	tests := []struct {
		name          string
		src           string
		expectError   bool
		errorContains string
	}{
		{
			name: "Constant comparison in condition",
			src: `fn test() -> i32 {
				let x := 10;
				let y := 5;
				if x > y {
					return 1;
				}
				return 0;
			}`,
			expectError: false,
		},
		{
			name: "Complex constant expression",
			src: `fn test() {
				let a := 5;
				let b := 3;
				let c := a + b;
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let x := arr[c];
			}`,
			expectError: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, ctx := parseCollectAndCheck(t, tt.src)

			diags := ctx.Diagnostics.Diagnostics()
			hasError := false

			for _, d := range diags {
				if d.Severity == diagnostics.Error {
					hasError = true
					t.Logf("Error: %s", d.Message)
				}
			}

			if tt.expectError && !hasError {
				t.Errorf("Expected error but got none")
			}

			if !tt.expectError && hasError {
				t.Errorf("Expected no error but got errors")
			}
		})
	}
}
