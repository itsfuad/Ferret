package codegen

import "strings"

// Shared C include helpers so generators don't duplicate the same include lists.
var (
	standardIncludes = []string{
		"#include <stdio.h>",
		"#include <stdlib.h>",
		"#include <string.h>",
		"#include <stdint.h>",
		"#include <math.h>",
	}

	// Runtime headers needed by generated code. Keep this in one place so both
	// pipeline and cgen stay in sync as the runtime grows.
	runtimeIncludes = []string{
		"#include \"io.h\"",
		"#include \"interface.h\"",
		"#include \"bigint.h\"",
		"#include \"optional.h\"",
	}
)

// StandardIncludes returns the formatted standard C includes with a trailing blank line.
func StandardIncludes() string {
	var b strings.Builder
	WriteStandardIncludes(&b)
	return b.String()
}

// RuntimeIncludes returns the formatted runtime includes (no trailing blank line).
func RuntimeIncludes() string {
	var b strings.Builder
	WriteRuntimeIncludes(&b)
	return b.String()
}

// WriteStandardIncludes writes the standard C library includes to the builder.
func WriteStandardIncludes(builder *strings.Builder) {
	for _, inc := range standardIncludes {
		builder.WriteString(inc)
		builder.WriteString("\n")
	}
	builder.WriteString("\n")
}

// WriteRuntimeIncludes writes the runtime header includes to the builder.
func WriteRuntimeIncludes(builder *strings.Builder) {
	for _, inc := range runtimeIncludes {
		builder.WriteString(inc)
		builder.WriteString("\n")
	}
}
