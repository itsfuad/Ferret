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

	headerIncludes = []string{
		"#include <stdint.h>",
		"#include <stdbool.h>",
	}

	// Runtime headers needed by generated code. Keep this in one place so both
	// pipeline and cgen stay in sync as the runtime grows.
	runtimeIncludes = []string{
		"#include \"alloc.h\"",
		"#include \"cast.h\"",
		"#include \"io.h\"",
		"#include \"interface.h\"",
		"#include \"map.h\"",
		"#include \"bigint.h\"",
		"#include \"optional.h\"",
		"#include \"ferret_time.h\"",
		"#include \"string_runtime.h\"",
		"#include \"random.h\"",
	}
)

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

// WriteHeaderIncludes writes minimal standard includes for generated headers.
func WriteHeaderIncludes(builder *strings.Builder) {
	for _, inc := range headerIncludes {
		builder.WriteString(inc)
		builder.WriteString("\n")
	}
	builder.WriteString("\n")
}
