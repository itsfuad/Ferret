package gen

import "compiler/internal/types"

func largePrimitiveName(typ types.SemType) (string, bool) {
	if typ == nil {
		return "", false
	}
	typ = types.UnwrapType(typ)
	if _, ok := typ.(*types.ReferenceType); ok {
		return "", false
	}
	prim, ok := typ.(*types.PrimitiveType)
	if !ok {
		return "", false
	}
	switch prim.GetName() {
	case types.TYPE_I128, types.TYPE_U128, types.TYPE_I256, types.TYPE_U256,
		types.TYPE_F128, types.TYPE_F256:
		return string(prim.GetName()), true
	default:
		return "", false
	}
}

func isLargePrimitiveType(typ types.SemType) bool {
	_, ok := largePrimitiveName(typ)
	return ok
}

func isLargeIntName(name string) bool {
	switch name {
	case string(types.TYPE_I128), string(types.TYPE_U128),
		string(types.TYPE_I256), string(types.TYPE_U256):
		return true
	default:
		return false
	}
}

func isLargeFloatName(name string) bool {
	switch name {
	case string(types.TYPE_F128), string(types.TYPE_F256):
		return true
	default:
		return false
	}
}
