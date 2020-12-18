package archive

import (
	"fmt"

	"github.com/apex/log"
	"github.com/urfave/cli"

	"github.com/fossas/fossa-cli/api/fossa"
	"github.com/fossas/fossa-cli/cmd/fossa/display"
	"github.com/fossas/fossa-cli/cmd/fossa/flags"
	"github.com/fossas/fossa-cli/cmd/fossa/setup"
)

var Cmd = cli.Command{
	Name:      "archive",
	Usage:     "Archive Upload",
	Action:    Run,
	ArgsUsage: "MODULE",
	Flags: flags.WithGlobalFlags(flags.WithAPIFlags(flags.WithOptions([]cli.Flag{
		flags.TemplateF,
	}))),
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
	archiveLocators := []fossa.Locator{}

	for i := 0; i < len(dirs); i++ {
		dir := dirs.Get(i)
		display.InProgress(fmt.Sprintf("Archive uploading directory (%d/%d): %s", i+1, len(dirs), dir))

		locator, err := fossa.UploadTarball(dir, dir, false, true, true)
		if err != nil {
			log.Warnf("Could not archive upload `%s` with error: %s", dir, err.Error())
		} else {
			archiveLocators = append(archiveLocators, locator)
		}
	}

	fmt.Println(archiveReports(archiveLocators))
	return err
}

func archiveReports(locators []fossa.Locator) string {
	formattedURLs := `
============================================================

    FOSSA Reports:
`
	for _, loc := range locators {
		formattedURLs = formattedURLs + fmt.Sprintf("    %s  %s\n", loc.Project, loc.URL())
	}
	formattedURLs = formattedURLs + `
============================================================
`

	return formattedURLs
}
