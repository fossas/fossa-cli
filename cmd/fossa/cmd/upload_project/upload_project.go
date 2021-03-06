package upload_project

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
	Name:      "upload-project",
	Usage:     "Uploads source code to FOSSA that will be treated as a single project",
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
	display.InProgress(fmt.Sprintf("Uploading directory: %s", dir))

	locator, err := fossa.UploadTarball(fossa.UploadTarballOptions{
		Name:            config.Project(),
		Revision:        config.Revision(),
		Directory:       dir,
		Title:           config.Title(),
		LicenseScanOnly: false,
		IsDependency:    false,
		Upload:          true,
		UploadOptions: fossa.UploadOptions{
			Branch:              config.Branch(),
			JIRAProjectKey:      config.JIRAProjectKey(),
			Team:                config.Team(),
			Policy:              config.Policy(),
			ProjectURL:          config.ProjectURL(),
			ReleaseGroup:        config.ReleaseGroup(),
			ReleaseGroupVersion: config.ReleaseGroupVersion(),
		},
	})

	if err != nil {
		log.Warnf("Could not upload project `%s` with error: %s", dir, err.Error())
		return err
	}

	fmt.Println(locator.ReportURL())
	return nil
}
