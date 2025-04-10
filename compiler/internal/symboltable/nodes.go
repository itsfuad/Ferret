package symboltable

import (
	"ferret/compiler/types"
	"fmt"
)

type AnalyzerNode interface {
	ANode() types.TYPE_NAME
	ToString() string
}

type Module struct {
	Name         string
	Path         string
	SymbolTable  *SymbolTable
	Dependencies map[string]*Module
}

func (m *Module) ANode() types.TYPE_NAME { return types.MODULE }
func (m *Module) ToString() string       { return fmt.Sprintf("package '%s'", m.Name) }
