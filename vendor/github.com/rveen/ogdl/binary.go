// Copyright 2012-2014, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

import (
	"bufio"
	"bytes"
	"io"
	"io/ioutil"
)

// BinParser and its methods implement a parser for binary OGDL, as defined in the
// OGDL Binary specification, v1.0, available at ogdl.org, reproduced below.
//
//     ogdl-binary ::= header ( level node )* 0x00
//
//     header ::= 0x01 'G' 0x00
//     level  ::= varInt
//     node   ::= text-node | binary-node
//
//     text-node ::= text 0x00
//     binary-node ::= 0x01 ( length data )* 0x00
//
//     length ::= multibyte-integer
//     data :: byte[length]
type binParser struct {
	r    *bufio.Reader
	last int
	// n counts the bytes read. Used in log.go.
	n int
}

// NewBytesBinParser creates a parser that can convert a binary OGDL byte stream into an
// ogdl.Graph object. To actually parse the stream, the method Parse() has to be invoked.
func newBytesBinParser(b []byte) *binParser {
	p := &binParser{bufio.NewReader(bytes.NewReader(b)), 0, 0}
	if p.r == nil {
		return nil
	}
	return p
}

// NewFileBinParser creates a parser that can convert a binary OGDL file into an
// ogdl.Graph object. To actually parse the stream, the method Parse() has to be invoked.
func newFileBinParser(file string) *binParser {

	// Read the entire file into memory
	b, err := ioutil.ReadFile(file)
	if err != nil || len(b) == 0 {
		return nil
	}

	return newBytesBinParser(b)
}

//NewBinParser creates a parser that can convert a binary OGDL stream into an
// ogdl.Graph object. To actually parse the stream, the method Parse() has to be invoked.
func newBinParser(r io.Reader) *binParser {
	return &binParser{bufio.NewReader(r), 0, 0}
}

// FromBinary converts an OGDL binary stream of bytes into a Graph.
func FromBinary(b []byte) *Graph {
	p := newBytesBinParser(b)
	return p.Parse()
}

// FromBinaryReader converts an OGDL binary stream of bytes into a Graph.
func FromBinaryReader(r io.Reader) *Graph {
	p := newBinParser(r)
	return p.Parse()
}

// FromBinaryFile converts an OGDL binary stream of bytes into a Graph.
func FromBinaryFile(file string) *Graph {
	p := newFileBinParser(file)
	return p.Parse()
}

// Binary converts a Graph to a binary OGDL byte stream.
func (g *Graph) Binary() []byte {

	if g == nil {
		return nil
	}

	// Header
	buf := make([]byte, 3)
	buf[0] = 1
	buf[1] = 'G'
	buf[2] = 0

	// The 'root' node is bypassed (it's the 'holder')
	for _, node := range g.Out {
		buf = node.bin(1, buf)
	}

	// Ending null
	buf = append(buf, 0)

	return buf
}

func (g *Graph) bin(level int, buf []byte) []byte {

	// Skip empty nodes
	b := _bytes(g.This)
	if len(b) != 0 {
		buf = append(buf, newVarInt(level)...)
		buf = append(buf, b...)
		buf = append(buf, 0)
		level++
	}

	for _, node := range g.Out {
		buf = node.bin(level, buf)
	}

	return buf
}

// Parse parses a binary OGDL stream and returns a Graph.
func (p *binParser) Parse() *Graph {

	if p == nil || !p.header() {
		return nil
	}

	ev := newEventHandler()

	for {
		lev, bin, b := p.line(true)
		if lev == 0 {
			break
		}
		// Store the content in the same format as it was sent (string or []byte)
		if bin {
			ev.AddBytesAt(b, lev)
		} else {
			ev.AddAt(string(b), lev)
		}
	}
	return ev.Graph()
}

// newVarInt produces a variable integer from an int.
// Only positive integers are accepted.
func newVarInt(i int) []byte {

	if i < 0 {
		return nil
	}

	if i < 0x80 {
		b := make([]byte, 1)
		b[0] = byte(i)
		return b
	}

	if i < 0x4000 {
		b := make([]byte, 2)
		b[0] = byte(i>>8 | 0x80)
		b[1] = byte(i & 0xff)
		return b
	}

	if i < 0x200000 {
		b := make([]byte, 3)
		b[0] = byte(i>>16 | 0xc0)
		b[1] = byte(i >> 8 & 0xff)
		b[2] = byte(i & 0xff)
		return b
	}

	if i < 0x10000000 {
		b := make([]byte, 4)
		b[0] = byte(i>>24 | 0xe0)
		b[1] = byte(i>>16 | 0xff)
		b[2] = byte(i >> 8 & 0xff)
		b[3] = byte(i & 0xff)
		return b
	}

	return nil
}

// header is the parser production that reads the header from the stream
//
// header ::= 0x01 'G' 0x00
func (p *binParser) header() bool {

	if p.read() != 1 {
		return false
	}
	if p.read() != 'G' {
		return false
	}
	if p.read() != 0 {
		return false
	}
	return true
}

// varInt is the parser production that reads a variable length integer from the stream.
//
// varInt ::=
//   0x00 - 0x7F:      0xxxxxxx
//   0x00 - 0x3FFF:    10xxxxxx xxxxxxxx
//   0x00 - 0x1FFFFF:  110xxxxx xxxxxxxx xxxxxxxx
//   0x00 - 0xFFFFFFF: 1110xxxx xxxxxxxx xxxxxxxx xxxxxxxx
//    ...
func (p *binParser) varInt() int {

	b0 := p.read()

	if b0 < 0x80 {
		return b0
	}

	if b0 < 0xc0 {
		b1 := p.read()
		return (b0&0x3f)<<8 | b1
	}

	if b0 < 0xe0 {
		b1 := p.read()
		b2 := p.read()
		return ((b0 & 0x1f) << 16) | (b1 << 8) | b2
	}

	if b0 < 0xf0 {
		b1 := p.read()
		b2 := p.read()
		b3 := p.read()
		return ((b0 & 0x0f) << 24) | (b1 << 16) | (b2 << 8) | b3
	}

	return -1
}

// line is the parser production that reads one line out of the binary OGDL stream.
//
//     line  ::= level node 0x00
//     level ::= varInt
//     node  ::= text-node | binary-node
//
// This function returns:
// - the level (1..)
// - the byte stream
// - a boolean that is true for binary nodes, false for text nodes
//
// This function accepts one boolean parameter that can be set to false if the
// actual byte content is not needed and we just want to walk through the
// stream. This functionality is used in log.go.
func (p *binParser) line(write bool) (int, bool, []byte) {

	// Read an integer (the level)
	level := p.varInt()
	if level < 1 {
		return 0, false, nil
	}

	// create a byte buffer to accumulate the bytes read.
	buf := bytes.Buffer{}

	// read first byte of the node content.
	n := p.read()

	// Binary node if n==1
	if n == 1 {
		// Read length, then bytes
		for {
			n = p.varInt()
			if n < 0 {
				break
			}
			for ; n != 0; n-- {
				c := p.read()
				if c < 0 {
					return level, true, buf.Bytes()
				}
				if write {
					buf.WriteByte(byte(c))
				}
			}
		}
		return level, true, buf.Bytes()
	}

	// Text node. Read bytes until 0

	if write {
		buf.WriteByte(byte(n))
	}

	for {
		c := p.read()
		if c == 0 {
			return level, false, buf.Bytes()
		}
		if write {
			buf.WriteByte(byte(c))
		}
	}
}

// read reads one character (byte) from the stream, returning it in the for of an int.
// Returning an int permits signaling an EOS with -1.
func (p *binParser) read() int {

	i, err := p.r.ReadByte()

	c := int(i)
	if err == io.EOF {
		c = -1
	} else {
		p.n++
	}

	p.last = c

	return c
}

// unread puts the last character readed back into the stream.
// (unread is used in rfunction.go, not in this file)
func (p *binParser) unread() {
	if p.last > 0 {
		p.n--
		p.r.UnreadByte()
	}
}
