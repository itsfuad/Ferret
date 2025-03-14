package main

import (
	"ferret/compiler/analyzer"
	"ferret/compiler/colors"
)

func main() {

	filename := "./../code/ifelse.fer"

	r, err := analyzer.Analyze(filename, true, true, true)

	if len(r) > 0 {
		r.DisplayAll()
		return
	}

	if err != nil {
		colors.RED.Println("Error analyzing file: ", err)
	}
}
