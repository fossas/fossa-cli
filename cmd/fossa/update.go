package main

import (
	"errors"
	"fmt"

	"github.com/blang/semver"
	logging "github.com/op/go-logging"
	"github.com/rhysd/go-github-selfupdate/selfupdate"
	"github.com/urfave/cli"
)

const updateEndpoint = "fossas/fossa-cli"

var updateLogger = logging.MustGetLogger("update")

func getSemver(v string) string {
	if v == "" {
		return ""
	}
	if v[0] != 'v' {
		return ""
	}
	return v[1:]
}

func checkUpdate() (bool, error) {
	latest, found, err := selfupdate.DetectLatest(updateEndpoint)
	if err != nil {
		return false, fmt.Errorf("could not check for updates: %s", err.Error())
	}

	parsedVersion := getSemver(version)
	updateLogger.Debugf("checking version for updates (%s -> %s)", version, parsedVersion)
	v, err := semver.Parse(parsedVersion)
	if err != nil {
		return false, errors.New("invalid version (are you using a development binary?)")
	}
	if !found || latest.Version.Equals(v) {
		return false, nil
	}
	return true, nil
}

func doSelfUpdate() error {
	parsedVersion := getSemver(version)
	v, err := semver.Parse(parsedVersion)
	if err != nil {
		return errors.New("invalid version (are you using a development binary?)")
	}
	latest, err := selfupdate.UpdateSelf(v, updateEndpoint)
	if err != nil {
		return fmt.Errorf("could not update: %s", err.Error())
	}
	if latest.Version.Equals(v) {
		return errors.New("no update required")
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
