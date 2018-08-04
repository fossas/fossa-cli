// Copyright 2012-2014, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

// Check returns true if the Graph given as a parameter conforms to the
// schema represented by the receiver Graph.
func (g *Graph) Check(x *Graph) (bool, string) {

	for i := 0; i < g.Len(); i++ {
		ns := g.GetAt(i)
		nx := x.GetAt(i)

		b := ns.checkNode(nx)
		if !b {
			if ns != nil {
				return false, "want " + ns.ThisString() + ", got " + nx.ThisString()
			}
		}

		ok, mess := ns.Check(nx)
		if !ok {
			return false, mess
		}
	}

	return true, ""
}

func (g *Graph) checkNode(x *Graph) bool {

	if g == nil || x == nil {
		return false
	}

	sx := x.ThisString()
	sc := g.ThisString()

	ty := x.ThisType()
	var ok bool

	// literal | literal* | literal? | literal+ | ** | ***
	if sc[0] != '!' {
		return sx == sc
	}

	// some type
	switch sc {

	case "!float":
		_, ok = _float64f(x.This)
		return ok

	case "!int":
		_, ok = _int64f(x.This)
		return ok

	case "!bool":
		_, ok = _boolf(x.This)
		return ok

	case "!string":
		return ty == "string"

	case "!binary":
		return ty == "[]byte"
	}

	return false
}
