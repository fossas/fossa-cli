package main

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"

	logging "github.com/op/go-logging"
)

var commonLogger = logging.MustGetLogger("common")

// Common utilities among commands
func makeAPIRequest(method, endpoint, apiKey string, payload []byte) ([]byte, error) {
	commonLogger.Debugf("Making API request %#v %#v %#v %#v", method, endpoint, apiKey, string(payload))
	req, err := http.NewRequest(method, endpoint, bytes.NewReader(payload))
	if err != nil {
		return nil, fmt.Errorf("could not construct API HTTP request: %s", err.Error())
	}
	req.Close = true
	req.Header.Set("Authorization", "token "+apiKey)
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		if err.(*url.Error).Err == io.EOF {
			commonLogger.Debugf("API request timed out")
			return nil, err.(*url.Error).Err
		}
		return nil, fmt.Errorf("failed to send API HTTP request: %s", err.Error())
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusForbidden {
		return nil, fmt.Errorf("invalid API key %#v (try setting $FOSSA_API_KEY)", apiKey)
	} else if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("bad server response: %d", resp.StatusCode)
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("could not read API HTTP response: %s", err.Error())
	}
	commonLogger.Debugf("Got API response: %#v", string(body))

	return body, nil
}
