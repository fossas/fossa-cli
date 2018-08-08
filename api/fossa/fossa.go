// Package fossa provides a high-level interface to the FOSSA API (by default,
// located at https://app.fossa.io).
package fossa

import (
	"net/url"

	"github.com/fossas/fossa-cli/api"
)

var (
	serverURL *url.URL
	apiKey    string
)

// Init sets up the API package.
func Init(server, APIKey string) error {
	var err error
	serverURL, err = url.Parse(server)
	if err != nil {
		return err
	}
	apiKey = APIKey
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

// PutJSON makes an authenticated JSON PUT request to a FOSSA API endpoint.
func PutJSON(endpoint string, body []byte, v interface{}) (statusCode int, err error) {
	u, err := serverURL.Parse(endpoint)
	if err != nil {
		return 0, err
	}
	return api.PutJSON(u, apiKey, body, v)
}
