package main

import (
	"errors"
	"ferret/compiler/internal/parser"
	"ferret/compiler/internal/typecheck"
	"ferret/compiler/report"
	"ferret/compiler/wio"
	"fmt"
	"path/filepath"
)

const HALTED = "compilation halted" // this is a constant that is used to halt the compilation

func compile(filePath string, showTokenDebug, saveToJson bool) (reports report.Reports, e error) {

	defer func() {
		if r := recover(); r != nil {
			e = fmt.Errorf("%v", r)
			reports = report.GetReports()
		}
		report.ClearReports()
	}()

	//must have .fer file
	if len(filePath) < 5 || filePath[len(filePath)-4:] != ".fer" {
		e = errors.New("error: file must have .fer extension")
		return nil, e
	}

	//get the folder and file name
	folder, fileName := filepath.Split(filePath)

	tree, table := parser.New(filePath, showTokenDebug).Parse()

	if saveToJson {
		//write the tree to a file named 'expressions.json' in 'code/ast' folder
		e = wio.Serialize(&tree, folder, fileName)
		if reports != nil {
			return report.GetReports(), e
		}
	}

	for _, node := range tree {
		typecheck.ASTNodeToAnalyzerNode(node, table)
	}

	reports = report.GetReports()

	return reports, nil
}
