// Copyright 2012-2017, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

import (
	"strconv"
)

// evalGraph
func (g *Graph) evalGraph(e *Graph) {

	for i, n := range e.Out {
		switch n.ThisString() {
		case TypePath:
			n.This = g.evalPath(n)
			n.Out = nil
		case TypeExpression:
			v := g.evalExpression(n)
			if nn, ok := v.(*Graph); ok {
				e.Out[i] = nn
			} else {
				n.This = v
				n.Out = nil
			}
		}
	}
}

// Eval takes a parsed expression and evaluates it
// in the context of the current graph.
func (g *Graph) Eval(e *Graph) interface{} {

	switch e.ThisString() {
	case TypePath:
		return g.evalPath(e)
	case TypeExpression:
		return g.evalExpression(e)
	}

	if e.Len() != 0 {
		return e
	}

	// Return constant in its normalizad native form
	// either: int64, float64, string, bool or []byte
	return e.ThisScalar()
}

// EvalBool takes a parsed expression and evaluates it in the context of the
// current graph, and converts the result to a boolean.
func (g *Graph) evalBool(e *Graph) bool {
	b, _ := _boolf(g.Eval(e))
	return b
}

// evalPath traverses g following a path p. The path needs to be previously converted
// to a Graph with NewPath().
//
// This function is similar to ogdl.Get, but for complexer paths. Code could
// be shared.
func (g *Graph) evalPath(p *Graph) interface{} {

	if p.Len() == 0 || g == nil {
		return nil
	}

	var node, nodePrev *Graph

	node = g

	iknow := false

	for i := 0; i < len(p.Out); i++ {
		n := p.Out[i]

		// For each path element, look at its type:
		// token, index, selector, arglist, keyword
		s := n.ThisString()

		iknow = false

		switch s {

		case TypeIndex:
			// must evaluate to an integer
			if n.Len() == 0 {
				return "empty []"
			}

			itf := g.evalExpression(n.Out[0])
			ix, ok := _int64(itf)
			if !ok || ix < 0 {
				return "[] does not evaluate to a valid integer"
			}

			iknow = true
			nodePrev = node
			node = node.GetAt(int(ix))

		case TypeSelector:
			if nodePrev == nil || nodePrev.Len() == 0 || i < 1 {
				return nil
			}

			elemPrev := p.Out[i-1].ThisString()
			if len(elemPrev) == 0 {
				return nil
			}

			r := New()

			if n.Len() == 0 {
				// This case is {}, meaning that we must return
				// all ocurrences of the token just before (elemPrev).
				// And that means creating a new Graph object.

				r.addEqualNodes(nodePrev, elemPrev, false)

				if r.Len() == 0 {
					return nil
				}
				node = r
			} else {
				i, err := strconv.Atoi(n.Out[0].ThisString())
				if err != nil || i < 0 {
					return nil
				}

				// {0} must still be handled: add it to r
				i++
				// of all the nodes with name elemPrev, select the ith.
				for _, nn := range nodePrev.Out {
					if nn.ThisString() == elemPrev {
						i--
						if i == 0 {

							r.AddNodes(nn)
							node = r
							break
						}
					}
				}

				if i > 0 {
					return nil
				}
			}

		case "_len":
			return node.Len()

		case "_this":
			return node

		case "_thisString":
			return node.ThisString()

		case "_string":
			return node.String()

		case TypeGroup:
			// We have hit an argument list of a function
			if node.Len() > 0 {
				itf, err := g.function(p, node.GetAt(0).This)
				if err != nil {
					return err.Error()
				}
				return itf
			}
			return nil

		case TypeExpression:
			// The expression is evaluated and used as path element
			itf := g.evalExpression(n.Out[0])
			str := _string(itf)

			if len(str) == 0 {
				return nil // expr does not evaluate to a string
			}
			s = str
			// [!] .().
			fallthrough

		default:
			nn := node.Node(s)

			if nn == nil {
				if node.Len() != 0 {
					itf, err := g.function(p, node.Out[0].This)
					if err != nil {
						return err.Error()
					}
					return itf
				}
				return nil
			}

			iknow = true
			nodePrev = node
			node = nn
		}
	}

	if node == nil {
		return nil
	}

	// iknow is true if the path includes the token that is now at the root.
	// We don't want to return what we already know.

	if node.This != nil && iknow {
		node2 := New()
		node2.Out = node.Out
		node = node2
	} else {
		node2 := New()
		node2.Add(node)
		node = node2
	}

	return node
}

// EvalExpression evaluates expressions (!e)
// g can have native types (other things than strings), but
// p only []byte or string
//
func (g *Graph) evalExpression(p *Graph) interface{} {

	// Return nil and empty strings as is
	if p.This == nil {
		return nil
	}

	s := p.ThisString()

	if len(s) == 0 {
		return ""
	}

	// first check if it is a number because it can have an operatorChar
	// in front: the minus sign
	if isNumber(s) {
		return p.ThisNumber()
	}

	switch s {
	case "!":
		// Unary expression !expr
		return !g.evalBool(p.Out[0])
	case TypeExpression:
		return g.evalExpression(p.GetAt(0))
	case TypePath:
		return g.evalPath(p)
	case TypeGroup:
		// expression list
		r := New(TypeGroup)
		for _, expr := range p.Out {
			r.Add(g.evalExpression(expr))
		}
		return r
	case TypeString:
		if p.Len() == 0 {
			return ""
		}
		return p.GetAt(0).ThisString()

	}

	c := int(s[0])

	// [!] Operator should be identified. Operators written as strings are
	// missinterpreted.
	if isOperatorChar(c) {
		if len(s) <= 2 {
			if len(s) == 1 || isOperatorChar(int(s[1])) {
				return g.evalBinary(p)
			}
		}
	}

	if c == '"' || c == '\'' {
		return s
	}

	if isLetter(c) {
		if s == "false" {
			return false
		}
		if s == "true" {
			return true
		}
		return s
	}

	return p
}

func (g *Graph) evalBinary(p *Graph) interface{} {

	n1 := p.Out[0]
	i2 := g.evalExpression(p.Out[1])

	switch p.ThisString() {

	case "+":
		return calc(g.evalExpression(n1), i2, '+')
	case "-":
		return calc(g.evalExpression(n1), i2, '-')
	case "*":
		return calc(g.evalExpression(n1), i2, '*')
	case "/":
		return calc(g.evalExpression(n1), i2, '/')
	case "%":
		return calc(g.evalExpression(n1), i2, '%')

	case "=":
		return g.assign(n1, i2, '=')
	case "+=":
		return g.assign(n1, i2, '+')
	case "-=":
		return g.assign(n1, i2, '-')
	case "*=":
		return g.assign(n1, i2, '*')
	case "/=":
		return g.assign(n1, i2, '/')
	case "%=":
		return g.assign(n1, i2, '%')

	case "==":
		return compare(g.evalExpression(n1), i2, '=')
	case ">=":
		return compare(g.evalExpression(n1), i2, '+')
	case "<=":
		return compare(g.evalExpression(n1), i2, '-')
	case "!=":
		return compare(g.evalExpression(n1), i2, '!')
	case ">":
		return compare(g.evalExpression(n1), i2, '>')
	case "<":
		return compare(g.evalExpression(n1), i2, '<')

	case "&&":
		return logic(g.evalExpression(n1), i2, '&')
	case "||":
		return logic(g.evalExpression(n1), i2, '|')

	}

	return nil
}

// int* | float* | string
// first element determines type
func compare(v1, v2 interface{}, op int) bool {

	i1, ok := _int64(v1)

	if ok {
		i2, ok := _int64f(v2)
		if !ok {
			return false
		}

		switch op {
		case '=':
			return i1 == i2
		case '+':
			return i1 >= i2
		case '-':
			return i1 <= i2
		case '>':
			return i1 > i2
		case '<':
			return i1 < i2
		case '!':
			return i1 != i2
		}
		return false
	}

	f1, ok := _float64(v1)
	if ok {
		f2, ok := _float64f(v2)
		if !ok {
			return false
		}
		switch op {
		case '=':
			return f1 == f2
		case '+':
			return f1 >= f2
		case '-':
			return f1 <= f2
		case '>':
			return f1 > f2
		case '<':
			return f1 < f2
		case '!':
			return f1 != f2
		}
		return false
	}

	s1 := _string(v1)
	s2 := _string(v2)

	switch op {
	case '=':
		return s1 == s2
	case '!':
		return s1 != s2
	}
	return false
}

func logic(i1, i2 interface{}, op int) bool {

	b1, ok1 := _boolf(i1)
	b2, ok2 := _boolf(i2)

	if !ok1 || !ok2 {
		return false
	}

	switch op {
	case '&':
		return b1 && b2
	case '|':
		return b1 || b2
	}

	return false
}

// assign modifies the context graph
func (g *Graph) assign(p *Graph, v interface{}, op int) interface{} {

	if op == '=' {
		return g.set(p, v)
	}

	// if p doesn't exist, just set it to the value given
	left := g.get(p)
	if left != nil {
		return g.set(p, calc(left.Out[0].This, v, op))
	}

	switch op {
	case '+':
		return g.set(p, v)
	case '-':
		return g.set(p, calc(0, v, '-'))
	case '*':
		return g.set(p, 0)
	case '/':
		return g.set(p, "infinity")
	case '%':
		return g.set(p, "undefined")
	}

	return nil
}

// calc: int64 | float64 | string
func calc(v1, v2 interface{}, op int) interface{} {

	i1, ok := _int64(v1)
	i2, ok2 := _int64(v2)

	var ok3, ok4 bool
	var i3, i4 float64

	if !ok {
		i3, ok3 = _float64(v1)
	}
	if !ok2 {
		i4, ok4 = _float64(v2)
	}

	if ok && ok2 {
		switch op {
		case '+':
			return i1 + i2
		case '-':
			return i1 - i2
		case '*':
			return i1 * i2
		case '/':
			return i1 / i2
		case '%':
			return i1 % i2
		}
	}
	if ok3 && ok4 {
		switch op {
		case '+':
			return i3 + i4
		case '-':
			return i3 - i4
		case '*':
			return i3 * i4
		case '/':
			return i3 / i4
		case '%':
			return int(i3) % int(i4)
		}
	}
	if ok && ok4 {
		i3 = float64(i1)
		switch op {
		case '+':
			return i3 + i4
		case '-':
			return i3 - i4
		case '*':
			return i3 * i4
		case '/':
			return i3 / i4
		case '%':
			return i1 % int64(i4)
		}
	}
	if ok3 && ok2 {
		i4 = float64(i2)
		switch op {
		case '+':
			return i3 + i4
		case '-':
			return i3 - i4
		case '*':
			return i3 * i4
		case '/':
			return i3 / i4
		case '%':
			return int64(i3) % i2
		}
	}

	if op != '+' {
		return nil
	}

	return _string(v1) + _string(v2)
}
