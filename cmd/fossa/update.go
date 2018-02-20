package main

import (
	"fmt"

	"github.com/blang/semver"
	logging "github.com/op/go-logging"
	"github.com/rhysd/go-github-selfupdate/selfupdate"
	"github.com/urfave/cli"
)

var updateLogger = logging.MustGetLogger("update")
var updateEndpoint = "fossas/fossa-cli"

func getSemver() string {
	if version[0] != 'v' {
		return ""
	}
	return version[1:]
}

func checkUpdate() error {
	latest, found, err := selfupdate.DetectLatest(updateEndpoint)
	if err != nil {
		return err // unable to update due to GH error
	}

	parsedVersion := getSemver()
	updateLogger.Debugf("checking version for updates (%s -> %s)", version, parsedVersion)
	v, err := semver.Parse(parsedVersion)
	if err != nil {
		return fmt.Errorf("invalid version; you may be using a development binary")
	}
	if !found || latest.Version.Equals(v) {
		return fmt.Errorf("current binary is latest version")
	}
	return nil
}

func doSelfUpdate() error {
	parsedVersion := getSemver()
	v, err := semver.Parse(parsedVersion)
	if err != nil {
		return fmt.Errorf("invalid version; you may be using a development binary")
	}
	latest, err := selfupdate.UpdateSelf(v, updateEndpoint)
	if err != nil {
		return err
	}
	if latest.Version.Equals(v) {
		// latest version is the same as current version. It means current binary is up to date.
		return fmt.Errorf("no update required; currently on latest")
	}
	return nil
}

func updateCmd(c *cli.Context) {
	if err := checkUpdate(); err != nil {
		updateLogger.Fatalf("unable to update (%s)", err)
	}

	if err := doSelfUpdate(); err != nil {
		updateLogger.Fatalf("unable failed (%s)", err)
	}

	updateLogger.Info("fossa has been updated; run `fossa -v` to view the current version")
}
