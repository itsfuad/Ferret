package main

import (
	"ferret/compiler/colors"
)

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
