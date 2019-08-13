package test_test

import (
	"encoding/json"
	"flag"
	"net/http"
	"net/http/httptest"
	"strconv"
	"testing"
	"time"

	"github.com/apex/log"
	"github.com/stretchr/testify/assert"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/test"
)

// taskStatus is a struct that imitates the anonymous struct within fossa.Build
type taskStatus struct {
	Status string
}

type mockServer struct {
	Responses []interface{}
}

// TestSuccessfullTest tests the `fossa test` test command and ensures that it properly
// handles all responses in the path to a success.
func TestSuccessfullTest(t *testing.T) {
	if testing.Short() {
		return
	}

	testLocator := fossa.Locator{Fetcher: "custom", Project: "tesRun", Revision: "123"}
	serverTest := mockServer{
		Responses: []interface{}{
			fossa.Organization{OrganizationID: 1},
			fossa.Build{Task: taskStatus{Status: "CREATED"}},
			fossa.Build{Task: taskStatus{Status: "ASSIGNED"}},
			fossa.Build{Task: taskStatus{Status: "RUNNING"}},
			fossa.Build{Task: taskStatus{Status: "SUCCEEDED"}},
			fossa.Issues{Status: "WAITING"},
			fossa.Issues{Status: "SCANNED"},
		},
	}
	ts := testCustomTestServer(t, testLocator, &serverTest)
	defer ts.Close()

	context := testSetup(testLocator, ts.URL, "1")
	err := test.Run(context)
	assert.NoError(t, err)
}

// testCustomTestServer dequeues the first item in server.Responses, marshals it, and responds with it.
func testCustomTestServer(t *testing.T, locator fossa.Locator, server *mockServer) *httptest.Server {
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp := server.Responses[0]
		server.Responses = server.Responses[1:]
		response, err := json.Marshal(resp)
		if err != nil {
			t.Fatalf("Failed to unmarshal JSON: %s", err)
		}
		_, err = w.Write(response)
		if err != nil {
			t.Fatalf("Failed to write response: %s", err)
		}
	}))
}

func testSetup(locator fossa.Locator, endpoint, apiKey string) *cli.Context {
	test.PollRequestDelay = 2 * time.Second
	apiErr := fossa.SetAPIKey(apiKey)
	if apiErr != nil {
		log.Fatalf("Could not set api key: %s", apiErr)
	}

	flagSet := &flag.FlagSet{}
	flagSet.Int("timeout", 100, "")
	rev, err := strconv.Atoi(locator.Revision)
	if err != nil {
		log.Warn(err.Error())
	}
	flagSet.Int("revision", rev, "")
	flagSet.String("fetcher", locator.Fetcher, "")
	flagSet.String("project", locator.Project, "")
	flagSet.String("endpoint", endpoint, "")
	return cli.NewContext(&cli.App{}, flagSet, &cli.Context{})
}
