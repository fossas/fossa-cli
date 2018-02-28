package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/config"
)

var reportLogger = logging.MustGetLogger("report")

type dependencyResponse struct {
	Loc struct {
		Package  string
		Revision string
	}
	Licenses []licenseResponse
	Project  struct {
		Title   string
		URL     string
		Authors []string
	}
}

type licenseResponse struct {
	ID       string
	Title    string
	FullText string
}

func reportCmd(c *cli.Context) {
	conf, err := config.New(c)
	if err != nil {
		reportLogger.Fatalf("Could not load configuration: %s", err.Error())
	}

	server, err := url.Parse(conf.Endpoint)
	if err != nil {
		reportLogger.Fatalf("Invalid FOSSA endpoint: %s", err.Error())
	}
	api, err := server.Parse(fmt.Sprintf("/api/revisions/%s/dependencies", url.PathEscape(config.MakeLocator(conf.Project, conf.Revision))))
	if err != nil {
		reportLogger.Fatalf("Invalid API endpoint: %s", err.Error())
	}
	params := url.Values{}
	params.Add("mediated", "true")
	api.RawQuery = params.Encode()

	res, err := makeAPIRequest(http.MethodGet, api.String(), conf.APIKey, nil)
	if err != nil {
		reportLogger.Fatalf("Could not get licenses from FOSSA API: %s", err.Error())
	}

	var deps []dependencyResponse
	err = json.Unmarshal(res, &deps)
	if err != nil {
		reportLogger.Fatalf("Could not parse API response: %s", err.Error())
	}

	depsByLicense := make(map[licenseResponse][]dependencyResponse)
	for _, dep := range deps {
		for _, license := range dep.Licenses {
			depsByLicense[license] = append(depsByLicense[license], dep)
		}
	}

	fmt.Printf("This software includes the following software and licenses:\n\n")
	for license, deps := range depsByLicense {
		fmt.Printf(`
========================================================================
%s
========================================================================

The following software have components provided under the terms of this license:

`, license.Title)
		for _, dep := range deps {
			fmt.Printf("- %s (from %s)\n", dep.Project.Title, dep.Project.URL)
		}
	}
	fmt.Println()
}
