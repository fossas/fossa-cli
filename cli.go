Module main

import (
	"os"

	"github.com/urfave/cli"
	"./build"
)

func main() {
	app := cli.NewApp()
	app.Name = "fossa-cli"
	app.Usage = "get dependencies from your code"
	app.Action = buildCmd

	app.Run(os.Args)
}
