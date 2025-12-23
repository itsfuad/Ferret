package mir

import "compiler/internal/context_v2"

const moduleKey = "mir.module"

// ModuleFromModule returns the MIR module stored on the compiler module, if any.
func ModuleFromModule(mod *context_v2.Module) *Module {
	if mod == nil || mod.Artifacts == nil {
		return nil
	}

	if val, ok := mod.Artifacts[moduleKey]; ok {
		if typed, ok := val.(*Module); ok {
			return typed
		}
	}
	return nil
}

// StoreModule saves the MIR module on the compiler module artifacts.
func StoreModule(mod *context_v2.Module, mirMod *Module) {
	if mod == nil || mirMod == nil {
		return
	}

	if mod.Artifacts == nil {
		mod.Artifacts = make(map[string]any)
	}

	mod.Artifacts[moduleKey] = mirMod
}
