package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strings"

	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/version"
	"github.com/fossas/fossa-cli/config"
	"github.com/fossas/fossa-cli/log"
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
		log.Logger.Fatalf("Could not load configuration: %s", err.Error())
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
			log.Logger.Fatalf("Could not parse user-provided build data: %s", err.Error())
		}
	}

	msg, err := doUpload(conf, data)
	if err != nil {
		log.Logger.Fatalf("Upload failed: %s", err.Error())
	}
	fmt.Print(msg)
}

func doUpload(conf config.CLIConfig, results []normalizedModule) (string, error) {
	fossaBaseURL, err := url.Parse(conf.Endpoint)
	if err != nil {
		return "", fmt.Errorf("invalid FOSSA endpoint")
	}

	if conf.Project == "" {
		log.Logger.Fatal("Could not infer project name from either `.fossa.yml` or `git` remote named `origin`")
	}

	if conf.Fetcher != "custom" && conf.Revision == "" {
		log.Logger.Fatal("Could not infer revision name from `git` remote named `origin`. To submit a custom project, set Fetcher to `custom` in `.fossa.yml`")
	}

	if len(results) == 0 {
		log.Logger.Fatal("No results to upload")
	}

	// Re-marshal into build data
	buildData, err := json.Marshal(results)
	if err != nil {
		return "", err
	}

	log.Logger.Debugf("Uploading build data from (%#v) modules: %#v", len(results), string(buildData))

	fossaEndpoint := "/api/builds/custom?locator=" + url.QueryEscape(module.Locator{Fetcher: conf.Fetcher, Project: conf.Project, Revision: conf.Revision}.String()) + "&v=" + version.ShortString()
	// TODO: warn when using not-custom fetcher + specified branch
	if conf.Fetcher == "custom" {
		// TODO: make title configurable
		title := results[0].Name
		if title == "" {
			title = conf.Project
		}
		fossaEndpoint += fmt.Sprintf("&managedBuild=true&title=%s&branch=%s", url.PathEscape(title), url.PathEscape(conf.Branch))
	}

	postRef, _ := url.Parse(fossaEndpoint)
	postURL := fossaBaseURL.ResolveReference(postRef).String()

	log.Logger.Debugf("Sending build data to <%#v>", postURL)

	res, err := makeAPIRequest(http.MethodPost, postURL, conf.APIKey, buildData)
	if err != nil {
		// HACK: really, we should be exporting this error and comparing against it
		if err.Error() == "bad server response: 428" {
			// TODO: handle "Managed Project" workflow
			return "", fmt.Errorf("invalid project or revision; make sure this version is published and FOSSA has access to your repo (to submit a custom project, set Fetcher to `custom` in `.fossa.yml`)")
		}
		return "", fmt.Errorf("could not upload: %s", err.Error())
	}

	log.Logger.Debugf("Upload succeeded")
	log.Logger.Debugf("Response: %#v", string(res))

	var jsonResponse map[string]interface{}
	if err := json.Unmarshal(res, &jsonResponse); err != nil {
		return "", fmt.Errorf("invalid response, but build was uploaded")
	}
	locParts := strings.Split(jsonResponse["locator"].(string), "$")
	reportBranch := conf.Branch
	if reportBranch == "" {
		reportBranch = "master"
	}
	getRef, _ := url.Parse("/projects/" + url.QueryEscape(locParts[0]) + "/refs/branch/" + reportBranch + "/" + url.QueryEscape(locParts[1]) + "/browse/dependencies")
	return fmt.Sprint(`
============================================================

    View FOSSA Report:
    ` + fossaBaseURL.ResolveReference(getRef).String() + `

============================================================
`), nil
}
