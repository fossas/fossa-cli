package test_test

import (
	"encoding/json"
	"flag"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/apex/log"
	"github.com/stretchr/testify/assert"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/cmd/test"
	"github.com/fossas/fossa-cli/errors"
)

// taskStatus is a struct that imitates the anonymous struct within fossa.Build
type taskStatus struct {
	Status string
}

type mockServer struct {
	Responses []interface{}
}

func (server *mockServer) nextResponse() (interface{}, error) {
	if len(server.Responses) == 0 {
		return nil, errors.New("no more responses left")
	}
	resp := server.Responses[0]
	server.Responses = server.Responses[1:]
	return resp, nil
}

// TestSuccessfullTest tests the `fossa test` test command and ensures that it properly
// handles all responses in the path to a success.
func TestSuccessfullTest(t *testing.T) {
	if testing.Short() {
		return
	}

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
	ts := testCustomTestServer(t, &serverTest)
	defer ts.Close()

	context := testSetup(ts.URL, "1")
	test.PollRequestDelay = 2 * time.Second
	err := test.Run(context)
	assert.NoError(t, err)
}

// testCustomTestServer dequeues the first item in server.Responses, marshals it, and responds with it.
func testCustomTestServer(t *testing.T, server *mockServer) *httptest.Server {
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		resp, err := server.nextResponse()
		assert.Nil(t, err)
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

func testSetup(endpoint, apiKey string) *cli.Context {
	apiErr := fossa.SetAPIKey(apiKey)
	if apiErr != nil {
		log.Fatalf("Could not set api key: %s", apiErr)
	}

	flagSet := &flag.FlagSet{}
	flagSet.String("endpoint", endpoint, "")
	flagSet.Int("timeout", 20, "")
	return cli.NewContext(&cli.App{}, flagSet, &cli.Context{})
}
