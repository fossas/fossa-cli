package main

import (
	"fmt"
	"net/url"
	"os"
	"strconv"
	"time"

	"github.com/briandowns/spinner"
	logging "github.com/op/go-logging"
	"github.com/tidwall/gjson"
	"github.com/urfave/cli"
	emoji "gopkg.in/kyokomi/emoji.v1"
)

var testLogger = logging.MustGetLogger("test")

const PollRequestDelay = 8

func confirmBuild(config cliConfig, timing int) error {
	fossaBaseURL, err := url.Parse(config.endpoint)
	if err != nil {
		return fmt.Errorf("invalid FOSSA endpoint")
	}

	reqRef, _ := url.Parse("/api/revisions/" + url.PathEscape(config.getVCSLocator()) + "/build")
	reqURL := fossaBaseURL.ResolveReference(reqRef).String()

	testLogger.Debugf("Querying <%#v>", reqURL)
	resp, err := makeAPIRequest("PUT", reqURL, nil, config.apiKey)
	if err != nil {
		return err
	}

	buildData := string(resp)
	buildStatus := gjson.Get(buildData, "task.status").String()

	testLogger.Debugf("Build status returned: %s", buildStatus)

	switch buildStatus {
	case "":
		return fmt.Errorf("unable to parse build results")
	case "FAILED":
		return fmt.Errorf("failed to analyze build #" + gjson.Get(buildData, "id").String() + " <" + gjson.Get(buildData, "error").String() + ">; visit FOSSA or contact support@fossa.io")
	case "SUCCEEDED":
		return nil
	default:
	}

	if (time.Duration(timing) * time.Second).Nanoseconds() >= (time.Duration(config.timeout) * time.Second).Nanoseconds() {
		return fmt.Errorf("request series timed out")
	}

	time.Sleep(time.Duration(PollRequestDelay) * time.Second)

	return confirmBuild(config, timing+PollRequestDelay)
}

func confirmScan(config cliConfig, timing int) error {
	fossaBaseURL, err := url.Parse(config.endpoint)
	if err != nil {
		return fmt.Errorf("invalid FOSSA endpoint")
	}

	reqRef, _ := url.Parse("/api/revisions/" + url.PathEscape(config.getVCSLocator()))
	reqURL := fossaBaseURL.ResolveReference(reqRef).String()

	testLogger.Debugf("Querying <%#v>", reqURL)
	resp, err := makeAPIRequest("GET", reqURL, nil, config.apiKey)
	if err != nil {
		return err
	}

	revisionData := string(resp)
	if gjson.Get(revisionData, "meta.0.last_scan").String() == "" {
		// not scanned yet
		if (time.Duration(timing) * time.Second).Nanoseconds() >= (time.Duration(config.timeout) * time.Second).Nanoseconds() {
			return fmt.Errorf("request series timed out")
		}
		time.Sleep(time.Duration(PollRequestDelay) * time.Second)

		return confirmScan(config, timing+PollRequestDelay)
	}

	issueCount := 0
	gjson.Get(revisionData, "issues.#.resolved").ForEach(func(key, value gjson.Result) bool {
		if value.Bool() == false {
			issueCount++
		}
		return true // keep iterating
	})
	if issueCount > 0 {
		return fmt.Errorf(strconv.Itoa(issueCount) + " issues found")
	}

	return nil
}

func testCmd(c *cli.Context) {
	config, err := initialize(c)
	if err != nil {
		testLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	s := spinner.New(spinner.CharSets[11], 100*time.Millisecond)
	s.Writer = os.Stderr
	s.Suffix = " Waiting for analysis job to succeed..."
	s.Start()

	if err := confirmBuild(config, 0); err != nil {
		s.Stop()
		testLogger.Fatalf("Error executing test: %s", err)
	}

	s.Suffix = " Waiting for FOSSA scan results..."
	s.Restart()

	if err := confirmScan(config, 0); err != nil {
		s.Stop()
		testLogger.Fatalf(err.Error())
	}
	// TODO: pipe issue data into a report function
	s.Stop()
	emoji.Println("Success; No issues found! :tada:")
}
