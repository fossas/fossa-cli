package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"strings"

	logging "github.com/op/go-logging"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/build"
	"github.com/fossas/fossa-cli/log"
)

type cliContext struct {
	config Config
}

// main.version is picked up by goreleaser
var version = "0.0.0"
var context = cliContext{}

func main() {
	app := cli.NewApp()
	app.Name = "fossa-cli"
	app.Usage = "get dependencies from your code"
	app.Version = version
	app.Action = DefaultCmd
	app.Flags = []cli.Flag{
		cli.StringFlag{Name: "log_level, l"},
	}

	app.Commands = []cli.Command{
		{
			Name:    "build",
			Aliases: []string{},
			Usage:   "discover dependencies for an inline module",
			Action:  BuildCmd,
			Flags: []cli.Flag{
				// Format: `type:path` e.g. `gopackage:github.com/fossas/fossa-cli/cmd/fossa`
				cli.StringFlag{Name: "entry_point, e"},
				cli.BoolFlag{Name: "install, i"},
				cli.BoolFlag{Name: "upload, u"},
				cli.BoolFlag{Name: "no_cache"},
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
					Name:   "api_key, k",
					EnvVar: "FOSSA_CLI_API_KEY",
				},
			},
		},
	}

	app.Before = BootstrapCmd

	app.Run(os.Args)
}

// BootstrapCmd initializes and loads config for the CLI.
func BootstrapCmd(c *cli.Context) error {
	// Configure logging.
	devNullBackend := logging.NewLogBackend(ioutil.Discard, "", 0)

	stderrBackend := logging.AddModuleLevel(logging.NewBackendFormatter(logging.NewLogBackend(os.Stderr, "", 0), log.Format))
	stderrBackend.SetLevel(logging.ERROR, "")

	if c.String("log_level") == "debug" {
		stderrBackend.SetLevel(logging.DEBUG, "")
	}

	logging.SetBackend(devNullBackend, stderrBackend)

	// Read configuration file.
	config, err := ReadConfig()
	log.Logger.Debugf("Configuration: %+v\n", config)
	if err != nil {
		return err
	}

	// Set configuration via flags.
	// // Global configuration flags.
	logLevelFlag := c.String("log_level")
	if logLevelFlag != "" {
		config.CLI.LogLevel = logLevelFlag
	}

	// // Analysis flags.
	entryPointFlag := c.String("entry_point")
	if entryPointFlag != "" {
		entryPointSections := strings.Split(entryPointFlag, "")
		config.Analyze.Modules = []ModuleConfig{
			ModuleConfig{
				Name: "",
				Path: entryPointSections[1],
				Type: entryPointSections[0],
			},
		}
	}

	// // CLI flags.
	installFlag := c.Bool("install")
	if installFlag {
		config.CLI.Install = true
	}

	uploadFlag := c.Bool("upload")
	if uploadFlag {
		config.CLI.Upload = true
	}

	noCacheFlag := c.Bool("no_cache")
	if noCacheFlag {
		config.CLI.NoCache = true
	}

	// // Upload flags.
	endpointFlag := c.String("endpoint")
	if endpointFlag != "" {
		config.CLI.Server = endpointFlag
	}

	locatorFlag := c.String("locator")
	if locatorFlag != "" {
		config.CLI.Locator = locatorFlag
	}

	apiKeyFlag := c.String("api_key")
	if apiKeyFlag != "" {
		config.CLI.APIKey = apiKeyFlag
	}

	context.config = config
	return nil
}

// DefaultCmd resolves dependencies and uploads the results.
func DefaultCmd(_ *cli.Context) {
	config := context.config
	config.CLI.Upload = true

	build, err := doBuild(config)
	if err != nil {
		log.Logger.Fatalf("Build failed: %s\n", err.Error())
	}

	err = doUpload(config, build)
	if err != nil {
		log.Logger.Fatalf("Upload failed: %s\n", err.Error())
	}

	fmt.Println("OK")
}

// BuildCmd runs a build given a configuration.
func BuildCmd(_ *cli.Context) {
	config := context.config
	build, err := doBuild(config)

	if err != nil {
		log.Logger.Fatalf("Analysis failed: (%v).\nTry pre-building and then running `fossa`", err)
	}

	if config.CLI.Upload {
		log.Logger.Debug("Uploading build results...")
		err = doUpload(config, build)
		if err != nil {
			log.Logger.Fatalf("Could not upload build results: %s\n", err.Error())
		}
	} else {
		fmt.Print(string(build))
	}

	fmt.Println("BUILD OK")
}

func doBuild(config Config) ([]byte, error) {
	// TODO: make this work for multiple entry points.
	// var modules []build.Module
	// for _, module := range config.Analyze.Modules {
	module := build.Module{
		Name:     config.Analyze.Modules[0].Name,
		Manifest: config.Analyze.Modules[0].Path,
		Type:     config.Analyze.Modules[0].Type,
	}

	// TODO(leo): refactor this into a struct.
	buildOpts := make(map[string]interface{})
	buildOpts["install"] = config.CLI.Install
	buildOpts["no_cache"] = config.CLI.NoCache

	log.Logger.Debugf("Building module: %+v\n", module)
	if err := module.Analyze(buildOpts); err != nil {
		return nil, err
	}

	log.Logger.Debugf("Found %d deduped dependencies", len(module.Build.RawDependencies))

	buildOutput, err := json.Marshal(module)
	if err != nil {
		return nil, err
	}

	return buildOutput, nil
}

// UploadCmd sends data to the fossa-core server about a specific revision.
func UploadCmd(c *cli.Context) {
	err := doUpload(context.config, []byte(c.Args().First()))
	if err != nil {
		log.Logger.Fatalf("Upload failed: %s\n", err.Error())
	}

	log.Logger.Info("Upload succeeded")
}

func doUpload(config Config, data []byte) error {
	fossaBaseURL, err := url.Parse(config.CLI.Server)
	if err != nil {
		return errors.New("invalid FOSSA endpoint")
	}

	log.Logger.Debugf("Uploading build data: %s\n", data)

	var js interface{}
	if err := json.Unmarshal(data, &js); err != nil {
		return errors.New("invalid build data")
	}

	postRef, _ := url.Parse("/api/builds/custom?locator=" + url.QueryEscape(config.CLI.Locator) + "&v=" + version)
	postURL := fossaBaseURL.ResolveReference(postRef).String()

	log.Logger.Debugf("Sending build data to <%s>", postURL)

	req, _ := http.NewRequest("POST", postURL, bytes.NewReader(data))
	req.Header.Set("Authorization", "token "+config.CLI.APIKey)
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("could not begin upload: %s", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusForbidden {
		return errors.New("invalid API key")
	} else if resp.StatusCode != http.StatusOK {
		responseBytes, _ := ioutil.ReadAll(resp.Body)
		responseStr := string(responseBytes)
		return fmt.Errorf("bad server response (%s)", responseStr)
	}

	return nil
}
