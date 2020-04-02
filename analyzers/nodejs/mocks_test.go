package nodejs_test

import (
	"errors"

	"github.com/fossas/fossa-cli/buildtools/npm"
	"github.com/fossas/fossa-cli/files"
)

type MockNPM struct {
	JSONFilename string
}

func (n MockNPM) List(_ string, _ bool) (npm.Output, error) {
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

// Keeping this true means that yarn will not be favored
func (n MockNPM) Exists() bool {
	return true
}

type MockNPMFailure struct{}

func (n MockNPMFailure) List(_ string, _ bool) (npm.Output, error) {
	return npm.Output{}, errors.New("expected failure for npm list")
}

func (n MockNPMFailure) Clean(dir string) error {
	return errors.New("expected failure for npm clean")
}

func (n MockNPMFailure) Install(dir string) error {
	return errors.New("expected failure for npm i")
}

func (n MockNPMFailure) Exists() bool {
	return true
}
