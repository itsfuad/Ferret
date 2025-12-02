package collector

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"fmt"
	"strings"
)

// buildImportAliasMap creates a mapping from alias/module-name to import path
// This enables module::symbol resolution in the resolver phase
func buildImportAliasMap(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	// Track where each alias was first defined for better error reporting
	aliasLocations := make(map[string]*context_v2.Import)

	for _, imp := range mod.Imports {
		var aliasName string

		// If there's an alias, use it
		if imp.Alias != "" {
			aliasName = imp.Alias
		} else {
			// Extract the last component of the import path as the default name
			// e.g., "test_project/utils" -> "utils"
			//       "std/math" -> "math"
			parts := splitImportPath(imp.Path)
			if len(parts) > 0 {
				aliasName = parts[len(parts)-1]
			} else {
				// Skip invalid import paths
				continue
			}
		}

		// Check for duplicate alias
		if existingImport, exists := aliasLocations[aliasName]; exists {
			// Report error for duplicate alias
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("duplicate import alias '%s'", aliasName)).
					WithCode("C-DUP-ALIAS").
					WithPrimaryLabel(mod.FilePath, imp.Location, fmt.Sprintf("'%s' already used for another import", aliasName)).
					WithSecondaryLabel(mod.FilePath, existingImport.Location, "previous import here").
					WithHelp(fmt.Sprintf("use a different alias: import \"%s\" as %s_alt", imp.Path, aliasName)),
			)
			continue
		}

		// Add to map
		mod.ImportAliasMap[aliasName] = imp.Path
		aliasLocations[aliasName] = imp
	}
}

// splitImportPath splits an import path by '/' to get components
func splitImportPath(path string) []string {
	// Remove quotes if present
	path = strings.Trim(path, "\"")

	// Split by forward slash
	parts := []string{}
	for _, part := range strings.Split(path, "/") {
		if part != "" {
			parts = append(parts, part)
		}
	}
	return parts
}
