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

	"github.com/fossas/fossa-cli/log"
)

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
	log.Logger.Debugf("Making API request %#v %#v %#v %#v", method, endpoint, apiKey, string(payload))
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
			log.Logger.Debugf("API request timed out")
			return nil, err
		}
		return nil, fmt.Errorf("failed to send API HTTP request: %s", err.Error())
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("could not read API HTTP response: %s", err.Error())
	}

	responseStr := string(body)

	if resp.StatusCode == http.StatusForbidden {
		log.Logger.Debugf("Response body: %s", responseStr)
		return nil, fmt.Errorf("invalid API key %#v (try setting $FOSSA_API_KEY); get one at https://fossa.io", apiKey)
	} else if resp.StatusCode != http.StatusOK {
		log.Logger.Debugf("Response body: %s", responseStr)
		maxResLength := 250
		if len(responseStr) < maxResLength {
			maxResLength = len(responseStr)
		}
		return nil, fmt.Errorf("bad server response (%d): %s", resp.StatusCode, responseStr[0:maxResLength])
	}

	log.Logger.Debugf("Got API response: %#v", string(body))
	return body, nil
}
