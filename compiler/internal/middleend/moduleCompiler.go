package middleend

import (
	"errors"
	"ferret/compiler/colors"
	"ferret/compiler/internal/frontend/ast"
	"ferret/compiler/internal/frontend/parser"
	"ferret/compiler/wio"
	"path/filepath"
)

func CompileModule(filePath string, showTokenDebug, saveToJson bool) (*ast.Module, error) {
	//must have .fer file
	if len(filePath) < 5 || filePath[len(filePath)-4:] != ".fer" {
		return nil, errors.New("error: file must have .fer extension")
	}

	//get the folder and file name
	folder, fileName := filepath.Split(filePath)

	colors.BLUE.Printf("Parsing file: %s\n", filePath)

	tree := parser.New(filePath, showTokenDebug).Parse()

	if saveToJson {
		//write the tree to a file named 'expressions.json' in 'code/ast' folder
		err := wio.Serialize(tree, folder, fileName)
		if err != nil {
			return tree, err
		}
	}

	return tree, nil
}
