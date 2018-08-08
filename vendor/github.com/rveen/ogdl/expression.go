// Copyright 2012-2014, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

// NewExpression parses an expression in text format (given in the string) to a Graph,
// in the form of a suitable syntax tree.
//
//     expression := expr1 (op2 expr1)*
//     expr1 := path | constant | op1 path | op1 constant | '(' expr ')' | op1 '(' expr ')'
//     constant ::= quoted | number
func NewExpression(s string) *Graph {
	p := newStringParser(s)
	p.Expression()
	g := p.graphTop(TypeExpression)
	g._ast()

	return g
}

// Ast reorganizes the expression graph in the form of an abstract syntax tree.
func (g *Graph) ast() {

	if g == nil {
		return
	}

	for _, node := range g.Out {
		if node.ThisString() == TypeExpression {
			node._ast()
		} else {
			node.ast()
		}
	}
}

func (g *Graph) _ast() {

	if g.Len() < 3 {
		return
	}

	for _, node := range g.Out {
		node.ast()
	}

	var e1, e2 *Graph

	for j := 6; j >= 0; j-- {

		for i := 0; i < len(g.Out); i++ {

			node := g.Out[i]
			if precedence(node.ThisString()) == j {
				e1 = g.Out[i-1]
				e2 = g.Out[i+1]
				g.Out = append(g.Out[:i-1], g.Out[i:]...)
				g.Out = append(g.Out[:i], g.Out[i+1:]...)
				node.Add(e1)
				node.Add(e2)
				i--
			}
		}
	}
}

// Precedence is same as in Go, except for the missing operators (| << >> & ^ &^)
//
// Assignment operators are given the lowest precedence.
func precedence(s string) int {

	switch s {

	case "+":
		return 4
	case "-":
		return 4
	case "*":
		return 5
	case "/":
		return 5
	case "%":
		return 5

	case "=":
		return 0
	case "+=":
		return 0
	case "-=":
		return 0
	case "*=":
		return 0
	case "/=":
		return 0
	case "%=":
		return 0

	case "==":
		return 3
	case "!=":
		return 3
	case ">=":
		return 3
	case "<=":
		return 3
	case ">":
		return 3
	case "<":
		return 3

	case "||":
		return 1
	case "&&":
		return 2
	}

	return -1
}
