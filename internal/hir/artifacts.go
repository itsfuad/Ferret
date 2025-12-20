package hir

import "compiler/internal/context_v2"

const (
	moduleKey        = "hir.module"
	loweredModuleKey = "hir.loweredModule"
)

// ModuleFromModule returns the source-shaped HIR module stored on the compiler module, if any.
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

// StoreModule saves the source-shaped HIR module on the compiler module artifacts.
func StoreModule(mod *context_v2.Module, hirMod *Module) {
	if mod == nil || hirMod == nil {
		return
	}

	if mod.Artifacts == nil {
		mod.Artifacts = make(map[string]any)
	}

	mod.Artifacts[moduleKey] = hirMod
}

// LoweredModuleFromModule returns the lowered HIR module stored on the compiler module, if any.
func LoweredModuleFromModule(mod *context_v2.Module) *Module {
	if mod == nil || mod.Artifacts == nil {
		return nil
	}

	if val, ok := mod.Artifacts[loweredModuleKey]; ok {
		if typed, ok := val.(*Module); ok {
			return typed
		}
	}
	return nil
}

// StoreLoweredModule saves the lowered HIR module on the compiler module artifacts.
func StoreLoweredModule(mod *context_v2.Module, hirMod *Module) {
	if mod == nil || hirMod == nil {
		return
	}

	if mod.Artifacts == nil {
		mod.Artifacts = make(map[string]any)
	}

	mod.Artifacts[loweredModuleKey] = hirMod
}
