package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"

	"github.com/fossas/fossa-cli/build"
	"github.com/fossas/fossa-cli/log"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"
)

// main.version is picked up by goreleaser
var version = "0.0.0"
var context = make(map[string]interface{})

func main() {
	app := cli.NewApp()
	app.Name = "fossa-cli"
	app.Usage = "get dependencies from your code"
	app.Version = version
	app.Action = MakeCmd
	app.Flags = []cli.Flag{
		cli.StringFlag{Name: "loglevel, l"},
	}

	app.Commands = []cli.Command{
		{
			Name:    "build",
			Aliases: []string{},
			Usage:   "discover dependencies for an inline module",
			Action:  BuildCmd,
			Flags: []cli.Flag{
				cli.StringFlag{Name: "type, t"},
				cli.StringFlag{Name: "entry-point, e"},
				cli.BoolFlag{Name: "install, i"},
				cli.BoolFlag{Name: "upload, u"},
				cli.BoolFlag{Name: "no-cache"},
			},
		},
		{
			Name:    "upload",
			Aliases: []string{},
			Usage:   "upload build data to a FOSSA endpoint",
			Action:  UploadCmd,
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:  "endpoint",
					Value: "https://app.fossa.io/",
				},
				cli.StringFlag{
					Name: "locator",
				},
				cli.StringFlag{
					Name: "api-key, k",
				},
			},
		},
	}

	app.Before = BootstrapCmd

	app.Run(os.Args)
}

// BootstrapCmd initializes and loads config for the CLI
func BootstrapCmd(c *cli.Context) error {
	devNullBackend := logging.NewLogBackend(ioutil.Discard, "", 0)

	// log errors to stderr
	stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), log.Format))
	stderrBackend.SetLevel(logging.ERROR, "")

	if c.String("loglevel") == "debug" {
		stderrBackend.SetLevel(logging.DEBUG, "")
	}

	logging.SetBackend(devNullBackend, stderrBackend)

	return nil
}

// MakeCmd runs the scan and build commands
func MakeCmd(c *cli.Context) error {
	// run scan and set context
	// run build and set context
	return nil
}

// BuildCmd takes in a Module and builds it / populates dependency data
// A successful build will set Module.Resolved to true
// An unsuccessful build will set Module.Error to a value
func BuildCmd(c *cli.Context) error {
	mod := build.Module{
		Type: c.String("type"),
	}

	buildOpts := make(map[string]interface{})
	buildOpts["install"] = c.Bool("install")
	buildOpts["no-cache"] = c.Bool("no-cache")
	buildOpts["entry-point"] = c.String("entry-point")
	buildOpts["upload"] = c.Bool("upload")

	if buildOpts["entry-point"].(string) != "" {
		// override module manifest
		mod.Manifest = buildOpts["entry-point"].(string)
	}

	if err := mod.Analyze(buildOpts); err != nil {
		log.Logger.Fatalf("analysis failed (%v);\ntry pre-building and then running `fossa`", err)
	}

	log.Logger.Debugf("found (%s) deduped dependencies", len(mod.Build.RawDependencies))

	buildOutput, _ := json.Marshal(mod)

	if buildOpts["upload"] == true {
		log.Logger.Debugf("uploading build results...")
		context["buildData"] = buildOutput
		UploadCmd(c)
	} else {
		fmt.Print(string(buildOutput))
	}
	return nil
}

// UploadCmd sends data to the fossa-core server about a speicifc revision
func UploadCmd(c *cli.Context) error {
	fossaBaseUrl, err := url.Parse(c.String("endpoint"))
	if err != nil {
		log.Logger.Fatal("invalid FOSSA endpoint")
	}

	var buildInput []byte
	if context["buildData"] == nil {
		buildInput = []byte(c.Args().First())
	} else {
		buildInput = context["buildData"].([]byte)
	}

	log.Logger.Debugf("parsing build data: %s", buildInput)

	var js interface{}
	if err := json.Unmarshal(buildInput, &js); err != nil {
		log.Logger.Fatal("invalid build data")
	}

	locator := c.String("locator")
	if locator == "" {
		// TODO: get locator from environment
		log.Logger.Fatalf("no locator specified")
	}

	postRef, _ := url.Parse("/api/builds/custom?locator=" + url.QueryEscape(locator) + "&v=" + version)
	postUrl := fossaBaseUrl.ResolveReference(postRef).String()

	log.Logger.Debugf("sending build data to <%s>", postUrl)

	req, _ := http.NewRequest("POST", postUrl, bytes.NewReader(buildInput))
	req.Header.Set("Authorization", "token "+c.String("api-key"))
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Logger.Fatalf("upload failed: %s", err)
	}

	defer resp.Body.Close()

	if resp.StatusCode == http.StatusForbidden {
		log.Logger.Fatal("upload failed: invalid API key")
	} else if resp.StatusCode != http.StatusOK {
		responseBytes, _ := ioutil.ReadAll(resp.Body)
		responseStr := string(responseBytes)
		log.Logger.Fatalf("upload failed: invalid response from FOSSA (%s)", responseStr)
	}

	log.Logger.Info("upload succeeded")
	return nil
}
