package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"os"
	"time"

	"github.com/briandowns/spinner"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

var testLogger = logging.MustGetLogger("test")

const pollRequestDelay = time.Duration(8) * time.Second
const buildsEndpoint = "/api/revisions/%s/build"
const revisionsEndpoint = "/api/revisions/%s"

type buildResponse struct {
	ID    int
	Error string
	Task  struct {
		Status string
	}
}

func getBuild(endpoint, apiKey, project, revision string) (buildResponse, error) {
	server, err := url.Parse(endpoint)
	if err != nil {
		return buildResponse{}, fmt.Errorf("invalid FOSSA endpoint: %s", err.Error())
	}

	buildsURL, err := url.Parse(fmt.Sprintf(buildsEndpoint, url.PathEscape(makeLocator(project, revision))))
	if err != nil {
		return buildResponse{}, fmt.Errorf("invalid FOSSA builds endpoint: %s", err.Error())
	}
	url := server.ResolveReference(buildsURL).String()

	testLogger.Debugf("Making Builds API request to: %#v", url)
	res, err := makeAPIRequest(http.MethodPut, url, apiKey, nil)
	if err != nil {
		return buildResponse{}, fmt.Errorf("could not make FOSSA build request: %s", err)
	}

	var build buildResponse
	err = json.Unmarshal(res, &build)
	if err != nil {
		return buildResponse{}, fmt.Errorf("could not parse FOSSA build: %#v %#v", err.Error(), res)
	}

	return build, nil
}

type revisionResponse struct {
	Meta []struct {
		LastScan string `json:"last_scan"`
	}
	Issues []issueResponse
}

type issueResponse struct {
	Resolved bool
}

func getRevision(endpoint, apiKey, project, revision string) (revisionResponse, error) {
	server, err := url.Parse(endpoint)
	if err != nil {
		return revisionResponse{}, fmt.Errorf("invalid FOSSA endpoint: %s", err.Error())
	}

	revisionsURL, err := url.Parse(fmt.Sprintf(revisionsEndpoint, url.PathEscape(makeLocator(project, revision))))
	if err != nil {
		return revisionResponse{}, fmt.Errorf("invalid FOSSA issues endpoint: %s", err.Error())
	}
	url := server.ResolveReference(revisionsURL).String()

	testLogger.Debugf("Making Revisions API request to: %#v", url)
	res, err := makeAPIRequest(http.MethodGet, url, apiKey, nil)
	if err != nil {
		return revisionResponse{}, err
	}

	var revisionJSON revisionResponse
	err = json.Unmarshal(res, &revisionJSON)
	if err != nil {
		return revisionResponse{}, fmt.Errorf("could not parse FOSSA revision: %#v %#v", err.Error(), res)
	}
	return revisionJSON, nil
}

func doTest(race chan testResult, endpoint, apiKey, project, revision string) {
	s := spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr
	s.Suffix = " Waiting for analysis to complete..."
	s.Start()

buildLoop:
	for {
		build, err := getBuild(endpoint, apiKey, project, revision)
		if err != nil {
			s.Stop()
			race <- testResult{err: err, issues: nil}
			return
		}
		testLogger.Debugf("Got build: %#v", build)
		switch build.Task.Status {
		case "SUCCEEDED":
			s.Stop()
			break buildLoop
		case "FAILED":
			s.Stop()
			race <- testResult{err: fmt.Errorf("failed to analyze build #%d: %s (visit FOSSA or contact support@fossa.io)", build.ID, build.Error), issues: nil}
			return
		default:
		}
		time.Sleep(pollRequestDelay)
	}

	s.Suffix = " Waiting for FOSSA scan results..."
	s.Restart()

	for {
		revision, err := getRevision(endpoint, apiKey, project, revision)
		if err != nil {
			s.Stop()
			race <- testResult{err: err, issues: nil}
			return
		}
		testLogger.Debugf("Got revision: %#v", revision)
		if len(revision.Meta) > 0 && revision.Meta[0].LastScan != "" {
			s.Stop()
			race <- testResult{err: nil, issues: revision.Issues}
			return
		}
		time.Sleep(pollRequestDelay)
	}
}

type testResult struct {
	err    error
	issues []issueResponse
}

func testCmd(c *cli.Context) {
	config, err := initialize(c)
	if err != nil {
		testLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	race := make(chan testResult, 1)
	go doTest(race, config.endpoint, config.apiKey, config.project, config.revision)
	select {
	case result := <-race:
		if result.err != nil {
			testLogger.Fatalf("Could not execute test: %s", result.err.Error())
		}
		testLogger.Debugf("Test succeeded: %#v", result.issues)
	case <-time.After(config.timeout):
		testLogger.Fatalf("API requests timed out")
	}
}
