// Package api provides low-level primitives for HTTP APIs.
package api

import (
	"bytes"
	"encoding/json"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"net/url"
	"time"

	"github.com/apex/log"
	"github.com/pkg/errors"
)

var defaultClient = http.Client{
	Timeout: 60 * time.Second,
	Transport: &http.Transport{
		DisableKeepAlives: true,
		Proxy:             http.ProxyFromEnvironment,
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
	err = json.Unmarshal(res, v)
	if err != nil {
		return code, errors.Wrap(err, "could not unmarshal JSON API response")
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

// TimeoutError is an error caused by an HTTP request timeout.
type TimeoutError error

// MakeAPIRequest runs and logs a request backed by a default `http.Client`.
func MakeAPIRequest(method string, endpoint *url.URL, APIKey string, body []byte) (res []byte, statusCode int, err error) {
	log.WithFields(log.Fields{
		"endpoint": *endpoint,
		"method":   method,
		"API key":  APIKey,
		"body":     string(body),
	}).Debug("making API request")

	// Construct request.
	req, err := http.NewRequest(method, endpoint.String(), bytes.NewReader(body))
	if err != nil {
		return nil, 0, errors.Wrap(err, "could not construct API HTTP request")
	}
	req.Close = true
	req.Header.Set("Authorization", "token "+APIKey)
	req.Header.Set("Content-Type", "application/json")

	// Send request.
	response, err := defaultClient.Do(req)
	if err != nil {
		if isTimeout(err) {
			return nil, 0, TimeoutError(errors.Wrap(err, "API request timed out"))
		}
		return nil, 0, errors.Wrap(err, "could not send API HTTP request")
	}
	defer response.Body.Close()

	// Read request.
	res, err = ioutil.ReadAll(response.Body)
	if err != nil {
		return nil, 0, errors.Wrap(err, "could not read API HTTP response")
	}

	log.WithField("response", string(res)).Debug("got API response")
	return res, response.StatusCode, nil
}
