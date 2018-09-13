package nodejs_test

import (
	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/files"
)

type MockNPM struct {
	JSONFilename string
}

func (n MockNPM) List(_ string) (npm.Output, error) {
	var output npm.Output
	err := files.ReadJSON(&output, n.JSONFilename)
	if err != nil {
		panic(err)
	}
	return output, nil
}

func (n MockNPM) Clean(dir string) error {
	return nil
}

func (n MockNPM) Install(dir string) error {
	return nil
}
