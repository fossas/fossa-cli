package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/builders"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/module"
)

var uploadLogger = logging.MustGetLogger("upload")

type normalizedModule struct {
	Name     string
	Type     string
	Manifest string
	Build    normalizedBuild
}

type normalizedBuild struct {
	Artifact string
	Context  interface{}

	Succeeded bool
	Error     error `json:",omitempty"`

	Dependencies []normalizedDependency
}

type normalizedDependency struct {
	// Location
	Locator string `json:"locator"`

	// Metadata
	Data *json.RawMessage `json:"data,omitempty"`

	// Context
	Depth              int      `json:"depth,omitempty"`
	Parent             string   `json:"parent,omitempty"`
	UnresolvedLocators []string `json:"unresolved_locators,omitempty"`
}

func normalize(builder builders.Builder, m module.Module, deps []module.Dependency) (normalizedModule, error) {
	var normalDeps []normalizedDependency
	for i := 0; i < len(deps); i++ {
		data, err := json.Marshal(deps[i])
		if err != nil {
			return normalizedModule{}, err
		}

		normalDeps = append(normalDeps, normalizedDependency{
			Locator: string(module.MakeLocator(deps[i])),
			Data:    (*json.RawMessage)(&data),
		})
	}

	normalizedType := string(m.Type)
	if normalizedType == "nodejs" {
		normalizedType = "commonjspackage"
	}

	return normalizedModule{
		Name:     m.Name,
		Type:     string(m.Type),
		Manifest: m.Target,
		Build: normalizedBuild{
			Artifact:     "default",
			Context:      builder,
			Succeeded:    true,
			Dependencies: normalDeps,
		},
	}, nil
}

func normalizeAnalysis(results analysis) ([]normalizedModule, error) {
	var normalized []normalizedModule
	for key, deps := range results {
		n, err := normalize(key.builder, key.module, deps)
		if err != nil {
			return nil, err
		}
		normalized = append(normalized, n)
	}
	return normalized, nil
}

func uploadCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		uploadLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	var data []normalizedModule
	err = json.Unmarshal([]byte(conf.UploadCmd.Data), &data)
	if err != nil {
		uploadLogger.Fatalf("Could not parse user-provided build data: %s", err.Error())
	}

	msg, err := doUpload(conf, data)
	if err != nil {
		analysisLogger.Fatalf("Upload failed: %s", err.Error())
	}
	fmt.Print(msg)
}

func doUpload(conf config.CLIConfig, results []normalizedModule) (string, error) {
	fossaBaseURL, err := url.Parse(conf.Endpoint)
	if err != nil {
		return "", fmt.Errorf("invalid FOSSA endpoint")
	}

	if conf.Project == "" {
		analysisLogger.Fatal("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}

	if conf.Fetcher != "custom" && conf.Revision == "" {
		analysisLogger.Fatal("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}

	// Re-marshal into build data
	buildData, err := json.Marshal(results)
	if err != nil {
		return "", err
	}

	analysisLogger.Debugf("Uploading build data from (%#v) modules: %#v", len(results), string(buildData))

	fossaEndpoint := "/api/builds/custom?locator=" + url.QueryEscape(config.MakeLocator(conf.Fetcher, conf.Project, conf.Revision)) + "&v=" + version
	if conf.Fetcher == "custom" {
		fossaEndpoint += "&managedBuild=true"
	}

	postRef, _ := url.Parse(fossaEndpoint)
	postURL := fossaBaseURL.ResolveReference(postRef).String()

	analysisLogger.Debugf("Sending build data to <%#v>", postURL)

	req, _ := http.NewRequest("POST", postURL, bytes.NewReader(buildData))
	req.Close = true
	req.Header.Set("Authorization", "token "+conf.APIKey)
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", fmt.Errorf("could not begin upload: %s", err.Error())
	}
	defer resp.Body.Close()
	responseBytes, _ := ioutil.ReadAll(resp.Body)
	responseStr := string(responseBytes)

	if resp.StatusCode == http.StatusForbidden {
		return "", fmt.Errorf("invalid API key (check the $FOSSA_API_KEY environment variable)")
	} else if resp.StatusCode == http.StatusPreconditionRequired {
		// TODO: handle "Managed Project" workflow
		return "", fmt.Errorf("invalid project or revision; make sure this version is published and FOSSA has access to your repo. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	} else if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("bad server response (%#v)", responseStr)
	}

	analysisLogger.Debugf("Upload succeeded")
	analysisLogger.Debugf("Response: %#v", responseStr)

	var jsonResponse map[string]interface{}
	if err := json.Unmarshal(responseBytes, &jsonResponse); err != nil {
		return "", fmt.Errorf("invalid response, but build was uploaded")
	}
	locParts := strings.Split(jsonResponse["locator"].(string), "$")
	getRef, _ := url.Parse("/projects/" + url.QueryEscape(locParts[0]) + "/refs/branch/master/" + url.QueryEscape(locParts[1]))
	return fmt.Sprint(`
============================================================

    View FOSSA Report:
    ` + fossaBaseURL.ResolveReference(getRef).String() + `

============================================================
`), nil
}
