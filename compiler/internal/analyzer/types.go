package analyzer

type AnalyzerNode interface {
	ANode()
}

type Symbol interface {
	AnalyzerNode
	ASymbol()
}

type Symboltype interface {
	AnalyzerNode
	AType()
}