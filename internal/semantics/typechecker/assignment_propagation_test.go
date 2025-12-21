package typechecker

import (
	"compiler/internal/diagnostics"
	"strings"
	"testing"
)

func TestConstantPropagationThroughAssignments(t *testing.T) {
	tests := []struct {
		name          string
		code          string
		expectError   bool
		errorContains string
	}{
		{
			name: "simple assignment propagation",
			code: `fn test() {
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let i := 5;
				i = 8;
				let x := arr[i];  // i is 8, within bounds
			}`,
			expectError: false,
		},
		{
			name: "assignment with arithmetic",
			code: `fn test() {
				let arr: [20]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20];
				let i := 5;
				i = i + 10;  // i is now 15
				let x := arr[i];  // i is 15, within bounds
			}`,
			expectError: false,
		},
		{
			name: "assignment causes out of bounds",
			code: `fn test() {
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let i := 5;
				i = i + 10;  // i is now 15
				let x := arr[i];  // i is 15, out of bounds
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "multiple assignments",
			code: `fn test() {
				let arr: [30]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30];
				let i := 5;
				i = i + 5;   // i is 10
				i = i * 2;   // i is 20
				let x := arr[i];  // i is 20, within bounds
			}`,
			expectError: false,
		},
		{
			name: "multiple assignments out of bounds",
			code: `fn test() {
				let arr: [20]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20];
				let i := 5;
				i = i + 5;   // i is 10
				i = i * 2;   // i is 20
				let x := arr[i];  // i is 20, out of bounds
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
		{
			name: "assignment clears constant after non-constant operation",
			code: `fn test(n: i32) {
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let i: i32 = 5;  // Explicitly typed to match parameter n
				i = n;  // i is now non-constant
				let x := arr[i];  // Error - index is not compile-time constant
			}`,
			expectError:   true,
			errorContains: "compile-time constant",
		},
		{
			name: "constant propagation with subtraction",
			code: `fn test() {
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let i := 15;
				i = i - 10;  // i is 5
				let x := arr[i];  // i is 5, within bounds
			}`,
			expectError: false,
		},
		{
			name: "reassignment to different constant",
			code: `fn test() {
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let i := 15;
				i = 8;  // i is now 8
				let x := arr[i];  // i is 8, within bounds
			}`,
			expectError: false,
		},
		{
			name: "assignment with negative result",
			code: `fn test() {
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let i := 5;
				i = i - 10;  // i is -5
				let x := arr[i];  // i is -5, valid negative index
			}`,
			expectError: false,
		},
		{
			name: "assignment with negative out of bounds",
			code: `fn test() {
				let arr: [10]i32 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				let i := 5;
				i = i - 20;  // i is -15
				let x := arr[i];  // i is -15, out of bounds
			}`,
			expectError:   true,
			errorContains: "array index out of bounds",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, ctx := parseCollectAndCheck(t, tt.code)
			diags := ctx.Diagnostics.Diagnostics()

			hasError := false
			var errorMsg string
			for _, d := range diags {
				if d.Severity == diagnostics.Error {
					hasError = true
					errorMsg = d.Message
					break
				}
			}

			if tt.expectError && !hasError {
				t.Errorf("Expected error containing %q but got none", tt.errorContains)
			}

			if !tt.expectError && hasError {
				t.Errorf("Expected no error but got: %s", errorMsg)
			}

			if tt.expectError && hasError && !strings.Contains(errorMsg, tt.errorContains) {
				t.Errorf("Expected error containing %q but got: %s", tt.errorContains, errorMsg)
			}
		})
	}
}
