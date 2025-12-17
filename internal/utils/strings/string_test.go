package strings

import (
	"testing"
)

func TestIsCapitalized(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"Hello", true},
		{"hello", false},
		{"", false},
		{"A", true},
		{"a", false},
		{"123", false},
		{"Z", true},
	}

	for _, test := range tests {
		result := IsCapitalized(test.input)
		if result != test.expected {
			t.Errorf("IsCapitalized(%q) = %v, expected %v", test.input, result, test.expected)
		}
	}
}

func TestPlural(t *testing.T) {
	tests := []struct {
		singular string
		plural   string
		count    int
		expected string
	}{
		{"item", "items", 1, "item"},
		{"item", "items", 0, "items"},
		{"item", "items", 2, "items"},
		{"person", "people", 1, "person"},
		{"person", "people", 5, "people"},
		{"mouse", "mice", 1, "mouse"},
		{"mouse", "mice", 2, "mice"},
	}

	for _, test := range tests {
		result := Pluralize(test.singular, test.plural, test.count)
		if result != test.expected {
			t.Errorf("Plural(%q, %q, %d) = %q, expected %q",
				test.singular, test.plural, test.count, result, test.expected)
		}
	}
}

func TestToIdentifier(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"my/mod-name", "my_mod_name"},
		{"my.mod/name", "my_mod_name"},
		{"my\\mod\\name", "my_mod_name"},
		{"name::with::colons", "name_with_colons"},
	}

	for _, tt := range tests {
		if got := ToIdentifier(tt.input); got != tt.want {
			t.Errorf("ToIdentifier(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestToHeaderGuard(t *testing.T) {
	got := ToHeaderGuard("my/mod-name")
	if got != "MY_MOD_NAME_H" {
		t.Errorf("ToHeaderGuard(%q) = %q, want %q", "my/mod-name", got, "MY_MOD_NAME_H")
	}
}
