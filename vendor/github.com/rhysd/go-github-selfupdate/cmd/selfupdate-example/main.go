package main

import (
	"flag"
	"fmt"
	"github.com/blang/semver"
	"github.com/rhysd/go-github-selfupdate/selfupdate"
	"os"
)

const version = "1.2.3"

func selfUpdate(slug string) error {
	selfupdate.EnableLog()

	previous := semver.MustParse(version)
	latest, err := selfupdate.UpdateSelf(previous, slug)
	if err != nil {
		return err
	}

	if previous.Equals(latest.Version) {
		fmt.Println("Current binary is the latest version", version)
	} else {
		fmt.Println("Update successfully done to version", latest.Version)
		fmt.Println("Release note:\n", latest.ReleaseNotes)
	}
	return nil
}

func usage() {
	fmt.Fprintln(os.Stderr, "Usage: selfupdate-example [flags]\n")
	flag.PrintDefaults()
}

func main() {
	help := flag.Bool("help", false, "Show this help")
	ver := flag.Bool("version", false, "Show version")
	update := flag.Bool("selfupdate", false, "Try go-github-selfupdate via GitHub")
	slug := flag.String("slug", "rhysd/go-github-selfupdate", "Repository of this command")

	flag.Usage = usage
	flag.Parse()

	if *help {
		usage()
		os.Exit(0)
	}

	if *ver {
		fmt.Println(version)
		os.Exit(0)
	}

	if *update {
		if err := selfUpdate(*slug); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		os.Exit(0)
	}

	usage()
}
