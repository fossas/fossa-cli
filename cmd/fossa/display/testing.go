package display

import "github.com/apex/log"

// Entries collects log entries during testing.
var Entries []log.Entry

// Test sets up logging with testing defaults.
func Test() {
	log.SetHandler(log.HandlerFunc(TestHandler))
}

// TestHandler handles log entries while testing.
func TestHandler(entry *log.Entry) error {
	Entries = append(Entries, *entry)
	return nil
}
