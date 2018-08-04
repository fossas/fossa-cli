// Copyright 2012-2014, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

import (
	"bufio"
	"bytes"
	"io"
	"io/ioutil"
	"strings"
)

// Nodes containing these strings are special
const (
	TypeExpression = "!e"
	TypePath       = "!p"
	TypeVariable   = "!v"
	TypeSelector   = "!s"
	TypeIndex      = "!i"
	TypeGroup      = "!g"
	TypeTemplate   = "!t"
	TypeString     = "!string"

	TypeIf    = "!if"
	TypeEnd   = "!end"
	TypeElse  = "!else"
	TypeFor   = "!for"
	TypeBreak = "!break"
)

// Parser is used to parse textual OGDL streams, paths, empressions and
// templates into Graph objects.
//
// Simple productions return a scalar (normally a string), more complex ones
// write to and event handler.
//
// BUG(): Level 2 (graphs) not implemented.
type parser struct {
	// The input (byte) stream
	in io.ByteReader

	// The output (event) stream
	ev *eventHandler

	// ind holds indentation at different levels, that is,
	// the number of spaces at each level.
	ind []int

	// last holds the 2 last characters read.
	// We need 2 characters of look-ahead (for Block()).
	last [2]int

	// unread index
	lastn int

	// the number of characters after a NL was found (used in Quoted)
	lastnl int

	// line keeps track of the line number
	line int

	// saved spaces at end of block
	spaces int
}

// NewStringParser creates an OGDL parser from a string
func newStringParser(s string) *parser {
	return &parser{strings.NewReader(s), newEventHandler(), make([]int, 32), [2]int{0, 0}, 0, 0, 1, 0}
}

// NewParser creates an OGDL parser from a generic io.Reader
func newParser(r io.Reader) *parser {
	return &parser{bufio.NewReader(r), newEventHandler(), make([]int, 32), [2]int{0, 0}, 0, 0, 1, 0}
}

// NewFileParser creates an OGDL parser that reads from a file
func newFileParser(s string) *parser {
	b, err := ioutil.ReadFile(s)
	if err != nil || len(b) == 0 {
		return nil
	}

	buf := bytes.NewBuffer(b)
	return &parser{buf, newEventHandler(), make([]int, 32), [2]int{0, 0}, 0, 0, 1, 0}
}

// NewBytesParser creates an OGDL parser from a []byte source
func newBytesParser(b []byte) *parser {
	buf := bytes.NewBuffer(b)
	return &parser{buf, newEventHandler(), make([]int, 32), [2]int{0, 0}, 0, 0, 1, 0}
}

// FromBytes parses OGDL text contained in a byte array. It returns a *Graph
func FromBytes(b []byte) *Graph {
	p := newBytesParser(b)
	p.Ogdl()
	return p.graph()
}

// FromString parses OGDL text from the given string. It returns a *Graph
func FromString(s string) *Graph {
	p := newBytesParser([]byte(s))
	p.Ogdl()
	return p.graph()
}

// FromReader parses OGDL text coming from a generic io.Reader
func FromReader(r io.Reader) *Graph {
	p := newParser(r)
	p.Ogdl()
	return p.graph()
}

// FromFile parses OGDL text contained in a file. It returns a Graph
func FromFile(s string) *Graph {
	p := newFileParser(s)
	if p == nil {
		return nil
	}
	p.Ogdl()
	return p.graph()
}

// Graph returns the *Graph object associated with this parser (where root
// where the OGDL tree is build on).
func (p *parser) graph() *Graph {
	return p.ev.Graph()
}

// GraphTop returns the *Graph object associated with this parser (where root
// where the OGDL tree is build on). Additionally, the name of the root node
// is set to the given string.
func (p *parser) graphTop(s string) *Graph {
	return p.ev.GraphTop(s)
}

// NextByteIs tests if the next character in the
// stream is the one given as parameter, in which
// case it is consumed.
//
func (p *parser) nextByteIs(c int) bool {
	ch := p.Read()
	if ch == c {
		return true
	}
	p.Unread()
	return false
}

// Read reads the next byte out of the stream.
func (p *parser) Read() int {

	var c int

	if p.lastn > 0 {
		p.lastn--
		c = p.last[p.lastn]
	} else {
		i, _ := p.in.ReadByte()
		c = int(i)
		p.last[1] = p.last[0]
		p.last[0] = c
	}

	if c == 10 {
		p.lastnl = 0
		p.line++
	} else {
		p.lastnl++
	}

	return c
}

// Unread puts the last readed character back into the stream.
// Up to two consecutive Unread()'s can be issued.
//
// BUG: line-- if newline
func (p *parser) Unread() {
	p.lastn++
	p.lastnl--
}

// setLevel sets the nesting level for a given indentation (number of spaces)
// setLevel sets ind[lev] = n, and all ind[>lev] = 0.
func (p *parser) setLevel(lev, n int) {

	// avoid out of index errors (the number of supported indentation levels is
	// limited, and the parser SHOULD return and error above that)
	if lev >= len(p.ind) {
		return
	}

	// Set ind[level] to the number of spaces
	p.ind[lev] = n

	// ( If there were any holes, bad things could happend. )

	for i := lev + 1; i < len(p.ind); i++ {
		p.ind[i] = 0
	}
}

// getLevel returns the nesting level corresponding to the given indentation.
// This function is used by the line() production for parsing OGDL text.
//
// getLevel returns the level for which ind[level] is equal or higher than n.
func (p *parser) getLevel(n int) int {

	l := 0

	for i := 0; i < len(p.ind); i++ {
		if p.ind[i] >= n {
			return i
		}
		if i != 0 && p.ind[i] == 0 {
			l = i - 1
			break
		}
	}
	if l < 0 {
		return 0
	}
	return l
}

// emit sends a string to the event handler
func (p *parser) emit(s string) {
	p.ev.Add(s)
}

// emitBytes sends a byte array to the event handler
func (p *parser) emitBytes(b []byte) {
	p.ev.AddBytes(b)
}
