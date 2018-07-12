// Package fossa provides a high-level interface to the FOSSA API (by default,
// located at https://app.fossa.io).
package fossa

import (
	"net/url"

	"github.com/fossas/fossa-cli/api"
	"github.com/fossas/fossa-cli/log"
)

var (
	serverURL *url.URL
	apiKey    string
)

// Init sets up an API instance.
func Init(server, APIKey string) error {
	var err error
	serverURL, err = url.Parse(server)
	if err != nil {
		return err
	}
	apiKey = APIKey
	return nil
}

// MustInit crashes and logs a fatal exception if `Init` fails.
func MustInit(server, APIKey string) {
	err := Init(server, APIKey)
	if err != nil {
		log.Logger.Fatal(log.Entry{
			Message: "could not initialize API",
			Error:   err,
			Fields: log.Fields{
				"server":  server,
				"API key": APIKey,
			},
		}.String())
	}
}

func mustParse(endpoint string) *url.URL {
	u, err := serverURL.Parse(endpoint)
	if err != nil {
		log.Logger.Fatal(log.Entry{
			Message: "invalid API endpoint",
			Error:   err,
			Fields:  log.Fields{"endpoint": endpoint},
		}.String())
		return nil
	}
	return u
}

// Get makes a GET request to a FOSSA API endpoint.
func Get(endpoint string) (res string, statusCode int, err error) {
	u := mustParse(endpoint)
	return api.Get(u, apiKey, nil)
}

// GetJSON makes a JSON GET request to a FOSSA API endpoint.
func GetJSON(endpoint string, v interface{}) (statusCode int, err error) {
	u := mustParse(endpoint)
	return api.GetJSON(u, apiKey, nil, v)
}

// Post makes a POST request to a FOSSA API endpoint.
func Post(endpoint string, body []byte) (res string, statusCode int, err error) {
	u := mustParse(endpoint)
	return api.Post(u, apiKey, body)
}
