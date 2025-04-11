package main

import (
	"ferret/compiler/colors"
	"ferret/compiler/internal/module"
	"ferret/compiler/report"
	"fmt"
)

func compile(filePath string, showTokenDebug, saveToJson bool) (reports report.Reports, e error) {

	defer func() {
		if r := recover(); r != nil {
			e = fmt.Errorf("%v", r)
			reports = report.GetReports()
		}
		report.ClearReports()
		//panic(":(")
	}()

	return report.GetReports(), module.CompileModule(filePath, showTokenDebug, saveToJson)
}

func main() {

	filename := "./../code/anz/0.fer"

	r, err := compile(filename, true, true)

	if len(r) > 0 {
		r.DisplayAll()
		return
	}

	if err != nil {
		colors.RED.Println("Error analyzing file: ", err)
	}

	colors.GREEN.Println("File parsed successfully")
}
