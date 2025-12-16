package codegen

import (
	"compiler/internal/context_v2"
	"compiler/internal/types"
)

const optionalTypesKey = "codegen.optionalTypes"

// OptionalTypesFromModule returns any optional type map stored on the module.
// The returned map should be treated as read-only.
func OptionalTypesFromModule(mod *context_v2.Module) map[string]types.SemType {
	if mod == nil || mod.Artifacts == nil {
		return nil
	}

	if val, ok := mod.Artifacts[optionalTypesKey]; ok {
		if typed, ok := val.(map[string]types.SemType); ok {
			return typed
		}
	}
	return nil
}

// StoreOptionalTypes saves a copy of the optional type map on the module.
func StoreOptionalTypes(mod *context_v2.Module, optTypes map[string]types.SemType) {
	if mod == nil || optTypes == nil {
		return
	}

	if mod.Artifacts == nil {
		mod.Artifacts = make(map[string]interface{})
	}

	copied := make(map[string]types.SemType, len(optTypes))
	for k, v := range optTypes {
		copied[k] = v
	}
	mod.Artifacts[optionalTypesKey] = copied
}
