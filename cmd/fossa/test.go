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

	"github.com/fossas/fossa-cli/config"
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

func getBuild(endpoint, apiKey, fetcher, project, revision string) (buildResponse, error) {
	server, err := url.Parse(endpoint)
	if err != nil {
		return buildResponse{}, fmt.Errorf("invalid FOSSA endpoint: %s", err.Error())
	}

	buildsURL, err := url.Parse(fmt.Sprintf(buildsEndpoint, url.PathEscape(config.MakeLocator(fetcher, project, revision))))
	if err != nil {
		return buildResponse{}, fmt.Errorf("invalid FOSSA builds endpoint: %s", err.Error())
	}
	url := server.ResolveReference(buildsURL).String()

	testLogger.Debugf("Making Builds API request to: %#v", url)
	res, err := makeAPIRequest(http.MethodPut, url, apiKey, nil)
	if isTimeout(err) {
		return buildResponse{}, err
	}
	if err != nil {
		return buildResponse{}, fmt.Errorf("could not make FOSSA build request: %s", err)
	}

	var build buildResponse
	err = json.Unmarshal(res, &build)
	if err != nil {
		return buildResponse{}, fmt.Errorf("could not parse FOSSA build: %#v %#v", err.Error(), string(res))
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
	Type     string
	// RevisionID string // TODO: We need to get this information from /api/issues?fromRevision=
}

func getRevision(endpoint, apiKey, fetcher, project, revision string) (revisionResponse, error) {
	server, err := url.Parse(endpoint)
	if err != nil {
		return revisionResponse{}, fmt.Errorf("invalid FOSSA endpoint: %s", err.Error())
	}

	revisionsURL, err := url.Parse(fmt.Sprintf(revisionsEndpoint, url.PathEscape(config.MakeLocator(fetcher, project, revision))))
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

func doTest(s *spinner.Spinner, race chan testResult, endpoint, apiKey, fetcher, project, revision string) {
	s.Suffix = " Waiting for analysis to complete..."
	s.Start()

buildLoop:
	for {
		build, err := getBuild(endpoint, apiKey, fetcher, project, revision)
		if isTimeout(err) {
			time.Sleep(pollRequestDelay)
			continue
		}
		if err != nil {
			race <- testResult{err: err, issues: nil}
			return
		}
		testLogger.Debugf("Got build: %#v", build)
		switch build.Task.Status {
		case "SUCCEEDED":
			break buildLoop
		case "FAILED":
			race <- testResult{err: fmt.Errorf("failed to analyze build #%d: %s (visit FOSSA or contact support@fossa.io)", build.ID, build.Error), issues: nil}
			return
		default:
		}
		time.Sleep(pollRequestDelay)
	}

	s.Suffix = " Waiting for FOSSA scan results..."
	s.Restart()

	for {
		revision, err := getRevision(endpoint, apiKey, fetcher, project, revision)
		if err != nil {
			race <- testResult{err: err, issues: nil}
			return
		}
		testLogger.Debugf("Got revision: %#v", revision)
		if len(revision.Meta) > 0 && revision.Meta[0].LastScan != "" {
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
	conf, err := config.New(c)
	if err != nil {
		testLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	s := spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr
	race := make(chan testResult, 1)
	go doTest(s, race, conf.Endpoint, conf.APIKey, conf.Fetcher, conf.Project, conf.Revision)
	select {
	case result := <-race:
		s.Stop()
		if result.err != nil {
			testLogger.Fatalf("Could not execute test: %s", result.err.Error())
		}
		testLogger.Debugf("Test succeeded: %#v", result.issues)
		if len(result.issues) == 0 {
			fmt.Fprintln(os.Stderr, "Test passed! 0 issues found")
		} else {
			var unresolvedIssues []issueResponse
			for _, issue := range result.issues {
				if !issue.Resolved {
					unresolvedIssues = append(unresolvedIssues, issue)
				}
			}

			pluralizedIssues := "issues"
			if len(unresolvedIssues) == 1 {
				pluralizedIssues = "issue"
			}
			fmt.Fprintf(os.Stderr, "Test failed! %d %s found\n", len(unresolvedIssues), pluralizedIssues)

			issues, err := json.Marshal(unresolvedIssues)
			if err != nil {
				testLogger.Fatalf("Could not marshal unresolved issues: %s", err)
			}
			fmt.Println(string(issues))

			os.Exit(1)
		}
	case <-time.After(conf.TestCmd.Timeout):
		s.Stop()
		testLogger.Fatalf("Timed out while waiting for issue report")
	}
}
