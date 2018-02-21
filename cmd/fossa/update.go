package main

import (
	"errors"
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

func checkUpdate() (bool, error) {
	latest, found, err := selfupdate.DetectLatest(updateEndpoint)
	if err != nil {
		return false, err // unable to update due to GH error
	}

	parsedVersion := getSemver()
	updateLogger.Debugf("checking version for updates (%s -> %s)", version, parsedVersion)
	v, err := semver.Parse(parsedVersion)
	if err != nil {
		return false, fmt.Errorf("invalid version; you may be using a development binary")
	}
	if !found || latest.Version.Equals(v) {
		return false, nil
	}
	return true, nil
}

func doSelfUpdate() error {
	parsedVersion := getSemver()
	v, err := semver.Parse(parsedVersion)
	if err != nil {
		return errors.New("invalid version; you may be using a development binary")
	}
	latest, err := selfupdate.UpdateSelf(v, updateEndpoint)
	if err != nil {
		return err
	}
	if latest.Version.Equals(v) {
		// latest version is the same as current version. It means current binary is up to date.
		return errors.New("no update required; currently on latest")
	}
	updateLogger.Debugf("updating binary versions (%s -> %s)", version, latest.Version)
	return nil
}

func updateCmd(c *cli.Context) {
	ok, err := checkUpdate()
	if err != nil {
		updateLogger.Fatalf("Unable to update: %s", err.Error())
	}
	if !ok {
		updateLogger.Fatalf("No updates available")
	}

	if err := doSelfUpdate(); err != nil {
		updateLogger.Fatalf("Update failed: %s", err.Error())
	}

	updateLogger.Info("fossa has been updated; run `fossa -v` to view the current version")
}
