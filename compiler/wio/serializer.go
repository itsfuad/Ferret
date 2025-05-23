package wio

import (
	"encoding/json"
	"fmt"
	"os"

	"ferret/compiler/internal/frontend/ast"
)

func Serialize(contents *ast.Module, folder, filename string) error {

	//create the folder if it does not exist
	if _, err := os.Stat(folder + "/ast"); os.IsNotExist(err) {
		os.Mkdir(folder+"/ast", os.ModePerm)
	}

	file, err := os.Create(fmt.Sprintf("%s/ast/%s.json", folder, filename))
	if err != nil {
		fmt.Printf("Error creating file: %s", err)
	}
	defer file.Close()

	//write the tree to a file named 'expressions.json' in 'code/ast' folder
	encoder := json.NewEncoder(file)
	encoder.SetIndent("", "    ")
	err = encoder.Encode(contents)
	if err != nil {
		fmt.Printf("Error encoding JSON: %s", err)
		return err
	}
	return nil
}
