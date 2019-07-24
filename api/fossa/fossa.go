// Package fossa provides a high-level interface to the FOSSA API (by default,
// located at https://app.fossa.com).
package fossa

import (
	"net/url"

	"github.com/fossas/fossa-cli/api"
	"github.com/fossas/fossa-cli/errors"
)

var (
	serverURL *url.URL
	apiKey    string
)

func SetEndpoint(endpoint string) error {
	u, err := url.Parse(endpoint)
	if err != nil {
		return err
	}
	serverURL = u
	return nil
}

func SetAPIKey(key string) *errors.Error {
	if key == "" {
		return &errors.Error{
			Type:            errors.User,
			Troubleshooting: "A FOSSA API key is needed to run this command.",
			Message:         errors.NoAPIKeyMessage,
		}
	}
	apiKey = key
	return nil
}

// Get makes an authenticated GET request to a FOSSA API endpoint.
func Get(endpoint string) (res string, statusCode int, err error) {
	u, err := serverURL.Parse(endpoint)
	if err != nil {
		return "", 0, err
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
