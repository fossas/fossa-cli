package config

import (
	"errors"

	"github.com/fossas/fossa-cli/files"
)

var ErrFileNotFound = errors.New("no files existed")

func TryStrings(candidates ...string) string {
	for _, c := range candidates {
		if c != "" {
			return c
		}
	}
	return ""
}

func TryFiles(candidates ...string) (string, error) {
	for _, c := range candidates {
		ok, err := files.Exists(c)
		if err != nil {
			return "", err
		}
		if ok {
			return c, nil
		}
	}
	return "", ErrFileNotFound
}
