package analyzer

func IsType(node AnalyzerNode) bool {
	return node.ANodeType() == TYPE_NODE
}

func IsValue(node AnalyzerNode) bool {
	return node.ANodeType() == VALUE_NODE
}
