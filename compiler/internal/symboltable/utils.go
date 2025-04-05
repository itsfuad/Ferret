package symboltable

func UnwrapUserDefType(value *AnalyzerNode) AnalyzerNode {
	switch t := (*value).(type) {
	case *UserDefType:
		return UnwrapUserDefType(&t.UnderlyingType)
	default:
		return t
	}
}
