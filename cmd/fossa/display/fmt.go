package display

import (
	"encoding/json"
	"fmt"
)

func JSON(data interface{}) (int, error) {
	msg, err := json.Marshal(data)
	if err != nil {
		return 0, err
	}
	return fmt.Println(string(msg))
}
