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
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/briandowns/spinner"
	logging "github.com/op/go-logging"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/build"
	"github.com/fossas/fossa-cli/log"
)

type cliContext struct {
	config Config
}

// main.version is picked up by goreleaser
var version = "0.2.0"
var context = cliContext{}

func main() {
	app := cli.NewApp()
	app.Name = "fossa-cli"
	app.Usage = "get dependencies from your code"
	app.Version = version
	app.Action = DefaultCmd
	app.Flags = []cli.Flag{
		cli.StringFlag{Name: "log_level, l"},
		cli.BoolFlag{Name: "install, i", Usage: "run a default build in module directories if they have not been pre-built"},
		cli.BoolFlag{Name: "output, o", Usage: "output build data to JSON and exit; do not upload results to FOSSA"},
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
				cli.BoolFlag{Name: "install, i", Usage: "run a default build in module directories if they have not been pre-built"},
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
func DefaultCmd(c *cli.Context) {
	config := context.config

	// Run the spinner only when we don't rely on stdout
	s := spinner.New(spinner.CharSets[11], 100*time.Millisecond)

	if !c.Bool("output") {
		s.Suffix = " Initializing..."
		s.Start()
	}

	builds := []*build.Module{}
	numModules := strconv.Itoa(len(config.Analyze.Modules))

	for i, mod := range config.Analyze.Modules {
		if !c.Bool("output") {
			s.Suffix = " Running build analysis (" +
				strconv.Itoa(i+1) + "/" +
				numModules + "): " +
				mod.Name
			s.Restart()
		}

		build, err := doBuild(config, i)
		if err != nil {
			s.Stop()
			log.Logger.Fatalf("Build failed (%s): %s\n", mod.Name, err.Error())
		}

		builds = append(builds, build)
	}

	if !c.Bool("output") {
		s.Suffix = " Uploading build results (" + numModules + "/" + numModules + ")..."
		s.Restart()
		err := doUpload(config, builds)
		if err != nil {
			s.Stop()
			log.Logger.Fatalf("Upload failed: %s\n", err.Error())
		}
		s.Stop()
	} else {
		output, _ := createBuildData(builds)
		fmt.Print(string(output))
	}
}

// BuildCmd runs the first build of a given configuration.
func BuildCmd(_ *cli.Context) {
	config := context.config
	mod, err := doBuild(config, 0)

	if err != nil {
		log.Logger.Fatalf("Analysis failed: (%v).\nTry pre-building and then running `fossa`", err)
	}

	if config.CLI.Upload {
		log.Logger.Debug("Uploading build results...")
		err = doUpload(config, []*build.Module{mod})
		if err != nil {
			log.Logger.Fatalf("Could not upload build results: %s\n", err.Error())
		}
		fmt.Print("build & upload succeeded")
	} else {
		data, _ := json.Marshal(mod)
		fmt.Print(string(data))
	}

	log.Logger.Debug("BUILD OK")
}

func doBuild(config Config, mIndex int) (*build.Module, error) {
	if len(config.Analyze.Modules) == 0 {
		return nil, errors.New("no entry points specified")
	}

	module := build.Module{
		Name:     config.Analyze.Modules[mIndex].Name,
		Manifest: config.Analyze.Modules[mIndex].Path,
		Dir:      filepath.Dir(config.Analyze.Modules[mIndex].Path),
		Type:     config.Analyze.Modules[mIndex].Type,
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

	return &module, nil
}

// UploadCmd sends data to the fossa-core server about a specific revision.
func UploadCmd(c *cli.Context) {
	builds := []*build.Module{}
	argIndex := 0
	for c.Args().Get(argIndex) != "" {
		var data build.Module
		if err := json.Unmarshal([]byte(c.Args().Get(argIndex)), &data); err != nil {
			log.Logger.Fatalf("Invalid build data for module #" + strconv.Itoa(argIndex))
		}
		builds = append(builds, &data)
		argIndex++
	}

	err := doUpload(context.config, builds)
	if err != nil {
		log.Logger.Fatalf("Upload failed: %s\n", err.Error())
	}

	log.Logger.Info("Upload succeeded")
}

func createBuildData(modules []*build.Module) ([]byte, error) {
	// TODO: make this return a build data struct over []byte later
	return json.Marshal(modules)
}

func doUpload(config Config, modules []*build.Module) error {
	fossaBaseURL, err := url.Parse(config.CLI.Server)
	if err != nil {
		return errors.New("invalid FOSSA endpoint")
	}

	log.Logger.Debugf("Uploading build data from (%s) modules:", len(modules))

	// re-marshall into build data
	buildData, err := createBuildData(modules)
	if err != nil {
		return errors.New("invalid build data")
	}

	log.Logger.Debugf(string(buildData))

	postRef, _ := url.Parse("/api/builds/custom?locator=" + url.QueryEscape(config.CLI.Locator) + "&v=" + version)
	postURL := fossaBaseURL.ResolveReference(postRef).String()

	log.Logger.Debugf("Sending build data to <%s>", postURL)

	req, _ := http.NewRequest("POST", postURL, bytes.NewReader(buildData))
	req.Header.Set("Authorization", "token "+config.CLI.APIKey)
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("could not begin upload: %s", err)
	}
	defer resp.Body.Close()
	responseBytes, _ := ioutil.ReadAll(resp.Body)
	responseStr := string(responseBytes)

	if resp.StatusCode == http.StatusForbidden {
		return errors.New("invalid API key")
	} else if resp.StatusCode == http.StatusPreconditionRequired {
		// TODO: handle "Managed Project" workflow
		return fmt.Errorf("invalid project or revision; make sure this version is published and FOSSA has access to your repo.")
	} else if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("bad server response (%s)", responseStr)
	} else {
		log.Logger.Info("upload succeeded")
		var jsonResponse map[string]interface{}
		if err := json.Unmarshal(responseBytes, &jsonResponse); err != nil {
			return errors.New("invalid response, but build was uploaded")
		}
		locParts := strings.Split(jsonResponse["locator"].(string), "$")
		getRef, _ := url.Parse("/projects/" + url.QueryEscape(locParts[0]) + "/refs/branch/master/" + url.QueryEscape(locParts[1]))
		fmt.Println("\n============================================================\n")
		fmt.Println("   View FOSSA Report:")
		fmt.Println("   " + fossaBaseURL.ResolveReference(getRef).String())
		fmt.Println("\n============================================================")
	}

	return nil
}
