package main

import (
	"flag"
	"fmt"
	"github.com/rhysd/go-github-selfupdate/selfupdate"
	"os"
	"regexp"
	"strings"
)

func usage() {
	fmt.Fprintln(os.Stderr, "Usage: detect-latest-release [flags] {repo}\n\n  {repo} must be URL to GitHub repository or in 'owner/name' format.\n\nFlags:\n")
	flag.PrintDefaults()
}

func main() {
	asset := flag.Bool("asset", false, "Output URL to asset")
	notes := flag.Bool("release-notes", false, "Output release notes additionally")
	url := flag.Bool("url", false, "Output URL for release page")

	flag.Usage = usage
	flag.Parse()

	if flag.NArg() != 1 {
		usage()
		os.Exit(1)
	}

	repo := flag.Arg(0)
	if strings.HasPrefix(repo, "https://") {
		repo = repo[len("https://"):]
	}
	if strings.HasPrefix(repo, "github.com/") {
		repo = repo[len("github.com/"):]
	}

	matched, err := regexp.MatchString("[^/]+/[^/]+", repo)
	if err != nil {
		panic(err)
	}
	if !matched {
		usage()
		os.Exit(1)
	}

	latest, found, err := selfupdate.DetectLatest(repo)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if !found {
		fmt.Println("No release was found")
	} else {
		if *asset {
			fmt.Println(latest.AssetURL)
		} else if *url {
			fmt.Println(latest.URL)
		} else {
			fmt.Println(latest.Version)
			if *notes {
				fmt.Printf("\nRelease Notes:\n%s\n", latest.ReleaseNotes)
			}
		}
	}
}
