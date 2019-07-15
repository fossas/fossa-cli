// Package fossa provides a high-level interface to the FOSSA API (by default,
// located at https://app.fossa.com).
package fossa

import (
	"net/url"

	"github.com/fossas/fossa-cli/api"
	"github.com/fossas/fossa-cli/errors"
)

var (
	serverURL          *url.URL
	apiKey             string
	missingAPIKeyError = &errors.Error{
		Cause:           errors.New("missing API Key"),
		Type:            "user",
		Troubleshooting: "follow the link for instructions to add an API KEY",
		Link:            "https://github.com/fossas/fossa-cli/blob/master/docs/user-guide.md/#3-analyzing-a-project",
	}
)

func SetEndpoint(endpoint string) error {
	u, err := url.Parse(endpoint)
	if err != nil {
		return errors.UnknownError(err)
	}
	serverURL = u
	return nil
}

func SetAPIKey(key string) error {
	if key == "" {
		return errors.UnknownError(errors.New("Deepest error"))
		// return missingAPIKeyError
	}
	apiKey = key
	return nil
}

// Get makes an authenticated GET request to a FOSSA API endpoint.
func Get(endpoint string) (res string, statusCode int, err error) {
	u, err := serverURL.Parse(endpoint)
	if err != nil {
		return "", 0, errors.UnknownError(err)
	}
	return api.Get(u, apiKey, nil)
}

// GetJSON makes an authenticated JSON GET request to a FOSSA API endpoint.
func GetJSON(endpoint string, v interface{}) (statusCode int, err error) {
	u, err := serverURL.Parse(endpoint)
	if err != nil {
		return 0, err
	}
	return api.GetJSON(u, apiKey, nil, v)
}

// Post makes an authenticated POST request to a FOSSA API endpoint.
// TODO: maybe `body` should be an `io.Reader` instead.
func Post(endpoint string, body []byte) (res string, statusCode int, err error) {
	u, err := serverURL.Parse(endpoint)
	if err != nil {
		return "", 0, err
	}
	return api.Post(u, apiKey, body)
}
