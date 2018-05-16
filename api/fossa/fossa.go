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

// Initialize sets up an API instance.
func Initialize(server, APIKey string) error {
	var err error
	serverURL, err = url.Parse(server)
	if err != nil {
		return err
	}
	apiKey = APIKey
	return nil
}

// MustInitialize crashes and logs a fatal exception if `Initialize` fails.
func MustInitialize(server, APIKey string) {
	err := Initialize(server, APIKey)
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

func Post(endpoint string, body []byte) (res string, statusCode int, err error) {
	u := mustParse(endpoint)
	return api.Post(u, apiKey, body)
}
