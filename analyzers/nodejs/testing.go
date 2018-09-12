package nodejs

import (
	"os"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/files"
)

type MockNPM struct{}

func (n MockNPM) List(_ string) (npm.Output, error) {
	var output npm.Output
	dir, _ := os.Getwd()

	err := files.ReadJSON(&output, dir, "fixtures/npmLsOutput.json")

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
