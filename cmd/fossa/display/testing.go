package display

import (
	"github.com/apex/log"
	"github.com/apex/log/handlers/memory"
)

// TestHandler collects log entries during testing.
var TestHandler log.Handler

// Test sets up logging with testing defaults.
func Test() {
	TestHandler = memory.New()
	log.SetHandler(TestHandler)
}
