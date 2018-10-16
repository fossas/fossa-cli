package fossa

import (
	"encoding/json"
	"fmt"

	"github.com/apex/log"
	"github.com/pkg/errors"
)

// lists out all compatible versions, where versions listed earlier in the list should be preferred to be used over later appearing versions
var compatibleVersionsByPreference = []string{"v1", "v0"}

func GetLatestSupportedAPIVersion() (string, error) {
	supportedApiVersions, err := getSupportedAPIVersions()
	if err != nil {
		return "", err
	}

	for _, compatibleVersion := range compatibleVersionsByPreference {
		for _, supportedVersion := range supportedApiVersions {
			if supportedVersion == compatibleVersion {
				return supportedVersion, nil
			}
		}
	}

	return "", nil
}

func getSupportedAPIVersions() ([]string, error) {
	res, statusCode, err := Get("/api/cli/supported_versions")
	if err != nil {
		return []string{}, errors.Wrap(err, "Could not get supported API versions.")
	}
	// If we can't find the endpoint, assume that v0 is supported (case of releasing CLI before core support is ready)
	if statusCode == 404 {
		log.Warn("Could not find the supported versions endpoint, defaulting to v0 upload path.")
		return []string{"v0"}, nil
	}

	if statusCode != 200 {
		return []string{}, fmt.Errorf("Could not get currently supported API versions, fossa returned a status code of %d", statusCode)
	}

	var unmarshalledVersions []string
	err = json.Unmarshal([]byte(res), &unmarshalledVersions)
	if err != nil {
		return []string{}, err
	}

	return unmarshalledVersions, nil
}
