package archive

import (
	"fmt"

	"github.com/apex/log"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
	"github.com/fossas/fossa-cli/config"
)

var Cmd = cli.Command{
	Name:      "archive",
	Usage:     "Archive Upload",
	Action:    Run,
	ArgsUsage: "MODULE",
	Flags:     flags.WithGlobalFlags(flags.WithAPIFlags([]cli.Flag{})),
}

var _ cli.ActionFunc = Run

func Run(ctx *cli.Context) error {
	err := setup.SetContext(ctx, true)
	if err != nil {
		log.Fatalf("Could not initialize %s", err)
	}

	dirs := ctx.Args()
	if len(dirs) == 0 {
		log.Fatal("no directories specified")
	}

	defer display.ClearProgress()

	dir := dirs.Get(0)
	display.InProgress(fmt.Sprintf("Archive uploading directory: %s", dir))

	locator, err := fossa.UploadTarballProject(config.Project(), config.Revision(), dir, true, fossa.UploadOptions{
		Branch:         config.Branch(),
		JIRAProjectKey: config.JIRAProjectKey(),
		Team:           config.Team(),
		Policy:         config.Policy(),
	})

	if err != nil {
		log.Warnf("Could not archive upload `%s` with error: %s", dir, err.Error())
		return err
	}

	fmt.Println(locator.ReportURL())
	return nil
}
