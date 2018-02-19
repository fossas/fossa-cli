package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strings"

	"github.com/fossas/fossa-cli/module"
)

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

func normalize(builder module.Builder, m module.Module, deps []module.Dependency) (normalizedModule, error) {
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

func doUpload(config cliConfig, results analysis) error {
	fossaBaseURL, err := url.Parse(config.endpoint)
	if err != nil {
		return fmt.Errorf("invalid FOSSA endpoint")
	}

	if config.revision == "" {
		analysisLogger.Fatal("no revision found in working directory; try running in a git repo or passing a locator")
	}

	if config.project == "" {
		analysisLogger.Fatal("could not infer project name from either `.fossa.yaml` or `git` remote named `origin`")
	}

	// Re-marshal into build data
	normalModules, err := normalizeAnalysis(results)
	if err != nil {
		return fmt.Errorf("could not normalize build data")
	}
	buildData, err := json.Marshal(normalModules)
	if err != nil {
		return err
	}

	analysisLogger.Debugf("Uploading build data from (%#v) modules: %#v", len(normalModules), string(buildData))

	postRef, _ := url.Parse("/api/builds/custom?locator=" + url.QueryEscape(makeLocator(config.project, config.revision)) + "&v=" + version)
	postURL := fossaBaseURL.ResolveReference(postRef).String()

	analysisLogger.Debugf("Sending build data to <%#v>", postURL)

	req, _ := http.NewRequest("POST", postURL, bytes.NewReader(buildData))
	req.Header.Set("Authorization", "token "+config.apiKey)
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("could not begin upload: %#v", err)
	}
	defer resp.Body.Close()
	responseBytes, _ := ioutil.ReadAll(resp.Body)
	responseStr := string(responseBytes)

	if resp.StatusCode == http.StatusForbidden {
		return fmt.Errorf("invalid API key (check the $FOSSA_API_KEY environment variable)")
	} else if resp.StatusCode == http.StatusPreconditionRequired {
		// TODO: handle "Managed Project" workflow
		return fmt.Errorf("invalid project or revision; make sure this version is published and FOSSA has access to your repo")
	} else if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("bad server response (%#v)", responseStr)
	}

	analysisLogger.Debugf("Upload succeeded")
	analysisLogger.Debugf("Response: %#v", responseStr)

	var jsonResponse map[string]interface{}
	if err := json.Unmarshal(responseBytes, &jsonResponse); err != nil {
		return fmt.Errorf("invalid response, but build was uploaded")
	}
	locParts := strings.Split(jsonResponse["locator"].(string), "$")
	getRef, _ := url.Parse("/projects/" + url.QueryEscape(locParts[0]) + "/refs/branch/master/" + url.QueryEscape(locParts[1]))
	fmt.Fprintln(os.Stderr, `
============================================================

		View FOSSA Report:
		`+fossaBaseURL.ResolveReference(getRef).String()+`

============================================================
`)
	return nil
}
