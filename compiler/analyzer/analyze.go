package analyzer

import (
	"errors"
	"ferret/compiler/internal/parser"
	"ferret/compiler/report"
	"ferret/compiler/wio"
	"fmt"
	"path/filepath"
)

const HALTED = "compilation halted"

func Analyze(filePath string, displayErrors, debug, save2Json bool) (reports report.Reports, e error) {

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

	tree := parser.New(filePath, debug).Parse()

	if save2Json {
		//write the tree to a file named 'expressions.json' in 'code/ast' folder
		e = wio.Serialize(&tree, folder, fileName)
		if reports != nil {
			return report.GetReports(), e
		}
	}

	reports = report.GetReports()

	return reports, nil
}
