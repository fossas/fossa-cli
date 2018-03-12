package main

import (
	"flag"
	"fmt"
	"github.com/rhysd/go-github-selfupdate/selfupdate"
	"go/build"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
)

func usage() {
	fmt.Fprintln(os.Stderr, `Usage: go-get-release [flags] {package}

  go-get-release is like "go get", but it downloads the latest release from
  GitHub. {package} must start with "github.com/".

Flags:`)
	flag.PrintDefaults()
}

func getCommand(pkg string) string {
	_, cmd := filepath.Split(pkg)
	if cmd == "" {
		// When pkg path is ending with path separator, we need to split it out.
		// i.e. github.com/rhysd/foo/cmd/bar/
		_, cmd = filepath.Split(cmd)
	}
	return cmd
}

func parseSlug(pkg string) (string, bool) {
	pkg = pkg[len("github.com/"):]
	first := false
	for i, r := range pkg {
		if r == '/' {
			if !first {
				first = true
			} else {
				return pkg[:i], true
			}
		}
	}
	if first {
		// When 'github.com/foo/bar' is specified, reaching here.
		return pkg, true
	}
	return "", false
}

func installFrom(url, cmd, path string) error {
	res, err := http.Get(url)
	if err != nil {
		return fmt.Errorf("Failed to download release binary from %s: %s", url, err)
	}
	defer res.Body.Close()
	if res.StatusCode != 200 {
		return fmt.Errorf("Failed to download release binary from %s: Invalid response ", url)
	}
	executable, err := selfupdate.UncompressCommand(res.Body, url, cmd)
	if err != nil {
		return fmt.Errorf("Failed to uncompress downloaded asset from %s: %s", url, err)
	}
	bin, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE, 0755)
	if err != nil {
		return err
	}
	if _, err := io.Copy(bin, executable); err != nil {
		return fmt.Errorf("Failed to write binary to %s: %s", path, err)
	}
	return nil
}

func main() {
	help := flag.Bool("help", false, "Show help")

	flag.Usage = usage
	flag.Parse()

	if *help || flag.NArg() != 1 || !strings.HasPrefix(flag.Arg(0), "github.com/") {
		usage()
		os.Exit(1)
	}

	slug, ok := parseSlug(flag.Arg(0))
	if !ok {
		usage()
		os.Exit(1)
	}

	latest, found, err := selfupdate.DetectLatest(slug)
	if err != nil {
		fmt.Fprintln(os.Stderr, "Error while detecting the latest version:", err)
		os.Exit(1)
	}
	if !found {
		fmt.Fprintln(os.Stderr, "No release was found in", slug)
		os.Exit(1)
	}

	cmd := getCommand(flag.Arg(0))
	cmdPath := filepath.Join(build.Default.GOPATH, "bin", cmd)
	if _, err := os.Stat(cmdPath); err != nil {
		// When executable is not existing yet
		if err := installFrom(latest.AssetURL, cmd, cmdPath); err != nil {
			fmt.Fprintf(os.Stderr, "Error while installing the release binary from %s: %s\n", latest.AssetURL, err)
			os.Exit(1)
		}
	} else {
		if err := selfupdate.UpdateTo(latest.AssetURL, cmdPath); err != nil {
			fmt.Fprintf(os.Stderr, "Error while replacing the binary with %s: %s\n", latest.AssetURL, err)
			os.Exit(1)
		}
	}

	fmt.Printf(`Command was updated to the latest version %s: %s

Release Notes:
%s
`, latest.Version, cmdPath, latest.ReleaseNotes)
}
