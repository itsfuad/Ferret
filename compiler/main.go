package main

import (
	"ferret/compiler/internal/lexer"
)

func main() {

	filename := "./../code/start.fer"

	lexer.Tokenize(filename, true)
}
