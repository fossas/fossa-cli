package display

import (
	"encoding/json"
	"fmt"
)

// JSON is a convenience function for printing JSON to STDOUT.
func JSON(data interface{}) (int, error) {
	msg, err := json.Marshal(data)
	if err != nil {
		return 0, err
	}
	return fmt.Println(string(msg))
}
