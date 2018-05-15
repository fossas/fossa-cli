package main

import (
	"errors"
	"fmt"

	"github.com/blang/semver"
	"github.com/fossas/fossa-cli/log"
	"github.com/rhysd/go-github-selfupdate/selfupdate"
	"github.com/urfave/cli"
)

const updateEndpoint = "fossas/fossa-cli"

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
	log.Logger.Debugf("checking version for updates (%s -> %s)", version, latest)
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
	log.Logger.Debugf("updating binary versions (%s -> %s)", version, latest.Version)
	return nil
}

func updateCmd(c *cli.Context) {
	ok, err := checkUpdate()
	if err != nil {
		log.Logger.Fatalf("Unable to update: %s", err.Error())
	}
	if !ok {
		log.Logger.Fatalf("No updates available")
	}

	if err := doSelfUpdate(); err != nil {
		log.Logger.Fatalf("Update failed: %s", err.Error())
	}

	log.Logger.Notice("fossa has been updated; run `fossa -v` to view the current version")
}
