package selfupdate

import (
	"testing"
)

func TestEnableDisableLog(t *testing.T) {
	defer DisableLog()

	EnableLog()
	if !logEnabled {
		t.Fatal("Log should be enabled")
	}
	EnableLog()
	if !logEnabled {
		t.Fatal("Log should be enabled")
	}
	DisableLog()
	if logEnabled {
		t.Fatal("Log should not be enabled")
	}
	DisableLog()
	if logEnabled {
		t.Fatal("Log should not be enabled")
	}
	EnableLog()
	if !logEnabled {
		t.Fatal("Log should be enabled")
	}
}
