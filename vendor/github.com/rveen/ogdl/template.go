// Copyright 2012-2017, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

import (
	"bytes"
)

// NewTemplate parses a text template given as a string and converts it to a Graph.
// Templates have fixed and variable parts. Variables all begin with '$'.
//
// A template is a text file in any format: plain text, HTML, XML, OGDL or
// whatever. The dolar sign acts as an escape character that switches from the
// text to the variable plane. Parsed templates are converted back to text
// by evaluating the variable parts against a Graph object, by means of the
// Process() method.
//
// Template grammar
//
//     template ::= ( text | variable )*
//
//     variable ::= ('$' path) | ('$' '(' expression ')') | ('$' '{' expression '}')
//     path ::= as defined in path.go
//     expression ::= as defined in expression.go
//
// Some variables act as directives: $if, $else, $end, $for, $break.
//
//    $if(expression)
//    $else
//    $end
//
//    $for(destPath,sourcepath)
//      $break
//    $end
//
func NewTemplate(s string) *Graph {
	p := newStringParser(s)
	p.Template()

	t := p.graphTop(TypeTemplate)
	t.ast()
	t.simplify()
	t.flow()

	return t
}

// NewTemplateBytes has the same function as NewTemplate except that the input stream
// is a byte array.
func NewTemplateFromBytes(b []byte) *Graph {
	p := newBytesParser(b)
	p.Template()

	t := p.graphTop(TypeTemplate)
	t.ast()
	t.simplify()
	t.flow()

	return t
}

// Process processes the parsed template, returning the resulting text in a byte array.
// The variable parts are resolved out of the Graph given.
func (tpl *Graph) Process(ctx *Graph) []byte {

	buffer := &bytes.Buffer{}

	tpl.process(ctx, buffer)

	return buffer.Bytes()
}

func (g *Graph) process(c *Graph, buffer *bytes.Buffer) bool {

	if g == nil || g.Out == nil {
		return false
	}

	falseIf := false

	for _, n := range g.Out {
		s := n.ThisString()

		switch s {
		case TypePath:
			i := c.Eval(n)
			buffer.WriteString(_text(i))

		case TypeExpression:
			// Silent evaluation
			c.Eval(n)
		case TypeIf:
			// evaluate the expression
			b := c.evalBool(n.GetAt(0).GetAt(0))

			if b {
				n.GetAt(1).process(c, buffer)
				falseIf = false
			} else {
				falseIf = true
			}
		case TypeElse:
			// if there was a previous if evaluating to false:
			if falseIf {
				n.process(c, buffer)
				falseIf = false
			}
		case TypeFor:
			// The first subnode (of !g) is a path
			// The second is an expression evaluating to a list of elements
			i := c.Eval(n.GetAt(0).GetAt(1))

			// Check that i is iterable
			gi, ok := i.(*Graph)
			if !ok || gi == nil {
				continue
			}

			// The third is the subtemplate to travel
			// println ("for type: ",reflect.TypeOf(i).String(), "ok",ok)
			// Assing expression value to path
			// XXX if not Graph

			varname := n.GetAt(0).GetAt(0).GetAt(0).String()
			c.Delete(varname)
			it := c.Add(varname)

			for _, ee := range gi.Out {

				it.Out = nil
				it.Add(ee)

				brk := n.GetAt(1).process(c, buffer)
				if brk {
					break
				}
			}
		case TypeBreak:
			return true

		default:
			buffer.WriteString(n.ThisString())
		}
	}
	return false
}

// simplify converts !p TYPE in !TYPE for keywords if, end, else, for and break.
func (g *Graph) simplify() {

	if g == nil {
		return
	}

	for _, node := range g.Out {
		if TypePath == node.ThisString() {
			s := node.GetAt(0).ThisString()

			switch s {
			case "if":
				node.This = TypeIf
				node.DeleteAt(0)
			case "end":
				node.This = TypeEnd
				node.DeleteAt(0)
			case "else":
				node.This = TypeElse
				node.DeleteAt(0)
			case "for":
				node.This = TypeFor
				node.DeleteAt(0)
			case "break":
				node.This = TypeBreak
				node.DeleteAt(0)
			}
		}
	}

}

// flow nests 'if' and 'for' loops.
func (g *Graph) flow() {
	n := 0
	var nod *Graph

	for i := 0; i < g.Len(); i++ {

		node := g.Out[i]
		s := node.ThisString()

		if s == TypeIf || s == TypeFor {
			n++
			if n == 1 {
				nod = node.Add(TypeTemplate)
				continue
			}
		}

		if s == TypeElse {
			if n == 1 {
				nod.flow()
				nod = node
				continue
			}
		}

		if s == TypeEnd {
			n--
			if n == 0 {
				nod.flow()
				g.DeleteAt(i)
				i--
				continue
			}
		}

		if n > 0 {
			nod.Add(node)
			g.DeleteAt(i)
			i--
		}
	}

}
