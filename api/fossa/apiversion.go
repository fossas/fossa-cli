package fossa

import (
	"encoding/json"
	"fmt"
	"net/url"

	"github.com/pkg/errors"
)

func GetSupportedApiVersions() ([]string, error) {
	endpoint, err := url.Parse("/api/cli/supported_versions")
	if err != nil {
		return []string{}, err
	}

	res, statusCode, err := Get(endpoint.String())
	if err != nil {
		return []string{}, errors.Wrap(err, "Could not get supported API versions")
	}
	// If we can't find the endpoint, assume that v0 is supported (case of releasing CLI before core support is ready)
	if statusCode == 404 {
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
	if len(unmarshalledVersions) == 0 {
		unmarshalledVersions = append(unmarshalledVersions, "v0")
	}

	return unmarshalledVersions, nil
}
