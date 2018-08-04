// Copyright 2012-2017, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.package ogdl

package ogdl

import (
	"encoding/json"
)

// FromJSON converts a JSON text stream into OGDL
//
// Json types returned by Unmashal:
// bool, for JSON booleans
// float64, for JSON numbers
// string, for JSON strings
// []interface{}, for JSON arrays
// map[string]interface{}, for JSON objects
// nil for JSON null
//
func FromJSON(buf []byte) (*Graph, error) {

	var v interface{}

	err := json.Unmarshal(buf, &v)
	if err != nil {
		return nil, err
	}
	return toGraph(v), nil
}

func toGraph(v interface{}) *Graph {

	g := New()

	switch v.(type) {

	case []interface{}:
		for _, i := range v.([]interface{}) {
			g.Add(toGraph(i))
		}
	case map[string]interface{}:
		for k, i := range v.(map[string]interface{}) {
			g.Add(k).Add(toGraph(i))
		}
	default:
		g.Add(v)
	}
	return g
}
