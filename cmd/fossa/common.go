package main

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"net/url"
	"time"

	logging "github.com/op/go-logging"
)

var commonLogger = logging.MustGetLogger("common")
var client = http.Client{
	Timeout: 10 * time.Second,
}

func isTimeout(err error) bool {
	if netErr, ok := err.(net.Error); ok && netErr.Timeout() {
		return true
	}
	if urlErr, ok := err.(*url.Error); ok && urlErr.Err == io.EOF {
		return true
	}
	return false
}

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

	resp, err := client.Do(req)
	if err != nil {
		if isTimeout(err) {
			commonLogger.Debugf("API request timed out")
			return nil, err
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
