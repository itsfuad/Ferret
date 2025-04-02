package analyzer

func unwrapType(value AnalyzerNode) AnalyzerNode {
	switch t := value.(type) {
	case *UserDefType:
		return unwrapType(t.UnderlyingType)
	default:
		return t
	}
}
