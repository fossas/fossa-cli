// Package api provides low-level primitives for implementing interfaces to
// various HTTP APIs.
package api

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"net/url"
	"time"

	"github.com/fossas/fossa-cli/log"
)

var c = http.Client{
	Timeout: 60 * time.Second,
	Transport: &http.Transport{
		DisableKeepAlives: true,
	},
}

// Get is a convenience method for MakeAPIRequest.
func Get(endpoint *url.URL, apiKey string, body []byte) (res string, statusCode int, err error) {
	return stringAPIRequest(http.MethodGet, endpoint, apiKey, body)
}

// Post is a convenience method for MakeAPIRequest.
func Post(endpoint *url.URL, apiKey string, body []byte) (res string, statusCode int, err error) {
	return stringAPIRequest(http.MethodPost, endpoint, apiKey, body)
}

// GetJSON is a convenience method for MakeAPIRequest.
func GetJSON(endpoint *url.URL, apiKey string, body []byte, v interface{}) (statusCode int, err error) {
	return jsonAPIRequest(http.MethodGet, endpoint, apiKey, body, v)
}

// PostJSON is a convenience method for MakeAPIRequest.
func PostJSON(endpoint *url.URL, apiKey string, body []byte, v interface{}) (statusCode int, err error) {
	return jsonAPIRequest(http.MethodPost, endpoint, apiKey, body, v)
}

func stringAPIRequest(method string, endpoint *url.URL, APIKey string, body []byte) (string, int, error) {
	res, code, err := MakeAPIRequest(method, endpoint, APIKey, body)
	if err != nil {
		return "", code, err
	}
	return string(res), code, nil
}

func jsonAPIRequest(method string, endpoint *url.URL, APIKey string, body []byte, v interface{}) (int, error) {
	res, code, err := MakeAPIRequest(method, endpoint, APIKey, body)
	if err != nil {
		return code, err
	}
	jsonErr := json.Unmarshal(res, v)
	if jsonErr != nil {
		return code, fmt.Errorf("could not unmarshal JSON API response: %s", jsonErr.Error())
	}
	return code, nil
}

func isTimeout(err error) bool {
	switch e := err.(type) {
	case net.Error:
		return e.Timeout()
	case *url.Error:
		return e.Err == io.EOF
	}
	return false
}

type TimeoutError error

// MakeAPIRequest runs and logs a request backed by an `http.Client`.
func MakeAPIRequest(method string, endpoint *url.URL, APIKey string, body []byte) (res []byte, statusCode int, err error) {
	log.Logger.Debug(log.Entry{
		Message: "making API request",
		Fields: log.Fields{
			"endpoint": *endpoint,
			"method":   method,
			"API key":  APIKey,
			"body":     string(body),
		},
	}.String())

	// Construct request.
	req, err := http.NewRequest(method, endpoint.String(), bytes.NewReader(body))
	if err != nil {
		return nil, 0, fmt.Errorf("could not construct API HTTP request: %s", err.Error())
	}
	req.Close = true
	req.Header.Set("Authorization", "token "+APIKey)
	req.Header.Set("Content-Type", "application/json")

	// Send request.
	response, err := c.Do(req)
	if err != nil {
		if isTimeout(err) {
			return nil, 0, TimeoutError(fmt.Errorf("API request timed out: %s", err.Error()))
		}
		return nil, 0, fmt.Errorf("could not send API HTTP request: %s", err.Error())
	}
	defer response.Body.Close()

	// Read request.
	res, err = ioutil.ReadAll(response.Body)
	if err != nil {
		return nil, 0, fmt.Errorf("could not read API HTTP response: %s", err.Error())
	}

	log.Logger.Debugf("Got API response: %#v", string(res))
	return res, response.StatusCode, nil
}
