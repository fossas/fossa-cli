package update

import (
	"fmt"

	"github.com/blang/semver"
	"github.com/pkg/errors"
	"github.com/rhysd/go-github-selfupdate/selfupdate"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/version"
	"github.com/apex/log"
)

const updateEndpoint = "fossas/fossa-cli"

var (
	ErrDevelopmentBuild = errors.New("development builds cannot be automatically updated")
	ErrInvalidVersion   = errors.New("invalid version (are you using a development binary?)")
)

var Cmd = cli.Command{
	Name:    "update",
	Aliases: []string{"upgrade"},
	Usage:   "Updates `fossa` to the latest version",
	Action:  Do,
	Flags:   flags.Global,
}

func Do(c *cli.Context) {
	log.ShowSpinner("Checking for updates...")

	ok, err := AvailableUpdate()
	if err != nil {
		log.Logger.Fatalf("Unable to update: %s", err.Error())
	}
	if !ok {
		log.Logger.Fatalf("No updates available")
	}

	version, err := Update()
	if err != nil {
		log.Logger.Fatalf("Update failed: %s", err.Error())
	}

	log.Logger.Notice("fossa has been updated to " + version.String())
}

func AvailableUpdate() (bool, error) {
	if version.IsDevelopment() {
		return false, ErrDevelopmentBuild
	}

	current, err := version.Semver()
	if err != nil {
		return false, ErrInvalidVersion
	}

	latest, found, err := selfupdate.DetectLatest(updateEndpoint)
	if err != nil {
		return false, errors.Wrap(err, "could not check for updates")
	}

	if !found || latest.Version.Equals(current) {
		return false, nil
	}
	return true, nil
}

func Update() (semver.Version, error) {
	if version.IsDevelopment() {
		return semver.Version{}, ErrDevelopmentBuild
	}

	current, err := version.Semver()
	if err != nil {
		return semver.Version{}, ErrInvalidVersion
	}

	latest, err := selfupdate.UpdateSelf(current, updateEndpoint)
	if err != nil {
		return semver.Version{}, fmt.Errorf("could not update: %s", err.Error())
	}

	if latest.Version.Equals(current) {
		return semver.Version{}, errors.New("no update required")
	}

	return latest.Version, nil
}
