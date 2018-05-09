package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"path/filepath"
	"strings"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"

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

func normalize(builder module.Builder, m module.Module, deps []module.Dependency) (normalizedModule, error) {
	var normalDeps []normalizedDependency
	for i := 0; i < len(deps); i++ {
		data, err := json.Marshal(deps[i])
		if err != nil {
			return normalizedModule{}, err
		}

		normalDeps = append(normalDeps, normalizedDependency{
			Locator: deps[i].Locator.String(),
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

func normalizeAnalysis(results []analysis) ([]normalizedModule, error) {
	var normalized []normalizedModule
	for _, res := range results {
		n, err := normalize(res.builder, res.module, res.dependencies)
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
	if conf.UploadCmd.UseLocators {
		var deps []normalizedDependency
		lines := strings.Split(conf.UploadCmd.Data, "\n")
		for _, line := range lines {
			deps = append(deps, normalizedDependency{
				Locator: line,
			})
		}
		data = append(data, normalizedModule{
			Build: normalizedBuild{
				Succeeded:    true,
				Dependencies: deps,
			},
		})
	} else {
		err = json.Unmarshal([]byte(conf.UploadCmd.Data), &data)
		if err != nil {
			uploadLogger.Fatalf("Could not parse user-provided build data: %s", err.Error())
		}
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

	if len(results) == 0 {
		analysisLogger.Fatal("No results to upload")
	}

	// Re-marshal into build data
	buildData, err := json.Marshal(results)
	if err != nil {
		return "", err
	}

	analysisLogger.Debugf("Uploading build data from (%#v) modules: %#v", len(results), string(buildData))

	fossaEndpoint := "/api/builds/custom?locator=" + url.QueryEscape(module.Locator{Fetcher: conf.Fetcher, Project: conf.Project, Revision: conf.Revision}.String()) + "&v=" + version
	if conf.Fetcher == "custom" {
		defaultProjectTitle := results[0].Name
		cwd, _ := filepath.Abs(".")
		if cwd != "" {
			defaultProjectTitle = filepath.Base(cwd)
		}
		fossaEndpoint += fmt.Sprintf("&managedBuild=true&title=%s", url.PathEscape(defaultProjectTitle))
	}

	postRef, _ := url.Parse(fossaEndpoint)
	postURL := fossaBaseURL.ResolveReference(postRef).String()

	analysisLogger.Debugf("Sending build data to <%#v>", postURL)

	res, err := makeAPIRequest(http.MethodPost, postURL, conf.APIKey, buildData)
	if err != nil {
		// HACK: really, we should be exporting this error and comparing against it
		if err.Error() == "bad server response: 428" {
			// TODO: handle "Managed Project" workflow
			return "", fmt.Errorf("invalid project or revision; make sure this version is published and FOSSA has access to your repo (to submit a custom project, set Fetcher to `custom` in `.fossa.yml`)")
		}
		return "", fmt.Errorf("could not upload: %s", err.Error())
	}

	analysisLogger.Debugf("Upload succeeded")
	analysisLogger.Debugf("Response: %#v", string(res))

	var jsonResponse map[string]interface{}
	if err := json.Unmarshal(res, &jsonResponse); err != nil {
		return "", fmt.Errorf("invalid response, but build was uploaded")
	}
	locParts := strings.Split(jsonResponse["locator"].(string), "$")
	getRef, _ := url.Parse("/projects/" + url.QueryEscape(locParts[0]) + "/refs/branch/master/" + url.QueryEscape(locParts[1]) + "/browse/dependencies")
	return fmt.Sprint(`
============================================================

    View FOSSA Report:
    ` + fossaBaseURL.ResolveReference(getRef).String() + `

============================================================
`), nil
}
