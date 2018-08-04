// Copyright 2012-2016, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

// NewPath takes a Unicode string representing an OGDL path, parses it and
// returns it as a Graph object.
//
// It also parses extended paths, as those used in templates, which may have
// argument lists.
func NewPath(s string) *Graph {
	parse := newStringParser(s)
	parse.Path()
	return parse.graphTop(TypePath)
}
