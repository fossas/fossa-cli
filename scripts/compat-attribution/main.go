// Using this program
//
// This program is meant to be a quick compatibility layer between the output of
// `fossa report attribution --json` between FOSSAv1 and FOSSAv2.
//
// Usage:
//
// 1. Build this script and place it somewhere.
//    For example, `go build -o /usr/local/bin/compat-attribution`.
//    Alternately, use the built version from the release assets.
// 2. Run `fossa report attribution --json`, piping its output to this script.
//    For example, `fossa report attribution --json | /usr/local/bin/compat-attribution`
// 3. Parse the resulting output as you would have from FOSSAv1.
//
// Support
//
// This compatibility script will be supported for six months, and will reach end-of-life on April 30, 2022.
//
package main

import (
	"encoding/json"
	"errors"
	"io"
	"log"
	"os"

	"github.com/fossas/fossa-cli/api/fossa"
)

func main() {
	stat, err := os.Stdin.Stat()
	if err != nil {
		log.Fatalf("Failed to stat stdin: %v", err)
	}
	if (stat.Mode() & os.ModeCharDevice) != 0 {
		log.Fatalf("Provide FOSSA CLI v2 attribution report output to this program via stdin")
	}

	var attribution fossa.AttributionReport
	if err := json.NewDecoder(os.Stdin).Decode(&attribution); err != nil {
		if errors.Is(err, io.EOF) {
			err = errors.New("no content to read")
		}
		log.Fatalf("Failed to parse attribution report from stdin: %v", err)
	}

	if err := json.NewEncoder(os.Stdout).Encode(attribution); err != nil {
		log.Fatalf("Failed to render attribution report to stdout: %v", err)
	}
}
