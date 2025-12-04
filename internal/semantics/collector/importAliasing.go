package collector

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/source"
	"compiler/internal/utils/fs"
	"fmt"
)

// buildImportAliasMap creates a mapping from alias/module-name to import path
// This enables module::symbol resolution in the resolver phase
func buildImportAliasMap(ctx *context_v2.CompilerContext, mod *context_v2.Module) {
	// Track where each alias was first defined for better error reporting
	// Map: aliasName -> {importPath, location}
	aliasInfo := make(map[string]struct {
		importPath string
		location   *source.Location
	})

	for _, imp := range mod.Imports {

		var aliasName string

		// If there's an explicit alias, use it
		if imp.Alias != "" {
			aliasName = imp.Alias
		} else {
			// Extract the last component of the import path as the default name
			// e.g., "test_project/utils" -> "utils"
			//       "std/math" -> "math"
			aliasName = fs.LastPart(imp.Path)
			if aliasName == "" {
				// Skip invalid import paths
				continue
			}
		}

		// Check for duplicate alias
		if existing, exists := aliasInfo[aliasName]; exists {
			// Report error for duplicate alias
			ctx.Diagnostics.Add(
				diagnostics.NewError(fmt.Sprintf("duplicate import alias '%s'", aliasName)).
					WithCode(diagnostics.ErrRedeclaredSymbol).
					WithPrimaryLabel(imp.Location, fmt.Sprintf("'%s' already used for another import", aliasName)).
					WithSecondaryLabel(existing.location, "previous import here").
					WithHelp(fmt.Sprintf("use a different alias: import \"%s\" as %s_alt", imp.Path, aliasName)),
			)
			continue
		}

		// Add to map
		mod.ImportAliasMap[aliasName] = imp.Path
		aliasInfo[aliasName] = struct {
			importPath string
			location   *source.Location
		}{
			importPath: imp.Path,
			location:   imp.Location,
		}
	}
}
