package main

import (
	"bytes"
	"errors"
	"io/ioutil"
	"net/http"
)

// Common utilities among commands
func makeAPIRequest(requestType string, url string, payload []byte, apiKey string) ([]byte, error) {
	req, _ := http.NewRequest(requestType, url, bytes.NewReader(payload))
	req.Header.Set("Authorization", "token "+apiKey)
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, err
	}

	defer resp.Body.Close()

	if resp.StatusCode == http.StatusForbidden {
		return nil, errors.New("invalid API key, check the $FOSSA_API_KEY environment variable")
	} else if resp.StatusCode != http.StatusOK {
		return nil, errors.New("bad server response (" + string(resp.StatusCode) + ")")
	}
	responseBytes, _ := ioutil.ReadAll(resp.Body)
	return responseBytes, nil
}
