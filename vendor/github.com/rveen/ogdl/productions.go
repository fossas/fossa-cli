// Copyright 2012-2017, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

import (
	"bytes"
	"errors"
)

// Ogdl is the main function for parsing OGDL text.
//
// An OGDL stream is a sequence of lines (a block
// of text or a quoted string can span multiple lines
// but is still parsed by Line())
//
//     Graph ::= Line* End
func (p *parser) Ogdl() error {

	for {
		more, err := p.Line()
		if err != nil {
			return err
		}
		if !more {
			break
		}
	}
	p.End()

	return nil
}

// Line processes an OGDL line or a multiline scalar.
//
// - A Line is composed of scalars and groups.
// - A Scalar is a Quoted or a String.
// - A Group is a sequence of Scalars enclosed in parenthesis
// - Scalars can be separated by commas or by space
// - The last element of a line can be a Comment, or a Block
//
// The indentation of the line and the Scalar sequences and Groups on it define
// the tree structure characteristic of OGDL level 1.
//
//    Line ::= Space(n) Sequence? ((Comment? Break)|Block)?
//
// Anything other than one Scalar before a Block should be an syntax error.
// Anything after a closing ')' that is not a comment is a syntax error, thus
// only one Group per line is allowed. That is because it would be difficult to
// define the origin of the edges pointing to what comes after a Group.
//
// Indentation rules:
//
//   a           -> level 0
//     b         -> level 1
//     c         -> level 1
//       d       -> level 2
//      e        -> level 2
//    f          -> level 1
//
func (p *parser) Line() (bool, error) {

	sp, n := p.Space()

	// if a line begins with non-uniform space, throw a syntax error.
	if sp && n == 0 {
		return false, errors.New("non-uniform space")
	}

	if p.End() {
		return false, nil
	}

	// We should not have a Comma here, but lets ignore it.
	if p.nextByteIs(',') {
		p.Space() // Eat eventual space characters
	}

	// indentation to level
	l := p.getLevel(n)
	p.ev.SetLevel(l)

	// Now we can expect a sequence of scalars and finally a block or comment.

	for {

		if p.Comment() {
			return true, nil
		}

		s, ok := p.Block()

		if ok {
			p.ev.Add(s)
			p.Break()
			break
		} else {

			if p.nextByteIs('(') || p.nextByteIs(')') {
				// TODO handle what previously was allowed (flow and block mixed)
				// Maybe just treat ( and ) as text characters
			} else {
				b, ok := p.Scalar()

				if ok {
					p.ev.Add(b)
				} else {
					p.Break()
					break
				}
			}
		}

		p.Space()

		if p.nextByteIs(',') {
			p.Space()
			// After a comma, reset the level to that of the start of this Line.
			p.ev.SetLevel(l)
		} else {
			p.ev.Inc()
		}

	}

	// Set the indentation to level rules for subsequent lines
	p.setLevel(l, n)
	p.setLevel(l+1, n+1)

	return true, nil
}

// Path parses an OGDL path, or an extended path as used in templates.
//
//     path ::= element ('.' element)*
//
//     element ::= token | integer | quoted | group | index | selector
//
//     (Dot optional before Group, Index, Selector)
//
//     group := '(' Expression [[,] Expression]* ')'
//     index := '[' Expression ']'
//     selector := '{' Expression '}'
//
// The OGDL parser doesn't need to know about Unicode. The character
// classification relies on values < 127, thus in the ASCII range,
// which is also part of Unicode.
//
// Note: On the other hand it would be stupid not to recognize for example
// Unicode quotation marks if we know that we have UTF-8. But when do we
// know for sure?
func (p *parser) Path() bool {

	var b string
	var begin = true
	var anything = false
	var ok bool
	var err error

	// dot keeps track of just read dots. This is used in Args(), to
	// distinguish between a(b) and a.(b)
	var dot bool

	for {

		// Expect: token | quoted | index | group | selector | dot,
		// or else we abort.

		// A dot is requiered before a token or quoted, except at
		// the beginning

		if !begin {
			c := p.Read()

			if c != '.' {
				dot = false
				p.Unread()

				// If not [, {, (, break
				if c != '[' && c != '(' && c != '{' {
					break
				}
			} else {
				dot = true
			}
		}

		begin = false

		b, ok = p.Quoted()
		if ok {
			p.ev.Add(b)
			anything = true
			continue
		}

		b, ok = p.Number()
		if ok {
			p.ev.Add(b)
			anything = true
			continue
		}

		b, ok = p.Token()
		if ok {
			p.ev.Add(b)
			anything = true
			continue
		}

		if p.Index() {
			anything = true
			continue
		}

		if p.Selector() {
			anything = true
			continue
		}

		ok, err = p.Args(dot)
		if ok {
			anything = true
			continue
		} else {
			if err != nil {
				return false // XXX
			}
		}

		break
	}

	return anything
}

// Sequence ::= (Scalar|Group) (Space? (Comma? Space?) (Scalar|Group))*
//
//   [!] with the requirement that after a group a comma is required if there are more elements.
//
//   Examples:
//     a b c
//     a b,c
//     a(b,c)
//     (a b,c)
//     (b,c),(d,e) <-- This can be handled
//     a (b c) d   <-- This is an error
//
//
//   This method returns two booleans: if there has been a sequence, and if the last element was a Group
//
func (p *parser) Sequence() (bool, bool, error) {

	i := p.ev.Level()

	wasGroup := false
	n := 0

	for {
		gr, err := p.Group()
		if gr {
			wasGroup = true
		} else if err != nil {
			return false, false, err
		} else {
			b, ok := p.Scalar()
			if !ok {
				return n > 0, wasGroup, nil
			}
			wasGroup = false
			p.ev.Add(b)
		}

		n++

		// We first eat spaces

		p.WhiteSpace()

		co := p.nextByteIs(',')

		if co {
			p.WhiteSpace()
			p.ev.SetLevel(i)
		} else {
			p.ev.Inc()
		}
	}
}

// Group ::= '(' Space? Sequence?  Space? ')'
func (p *parser) Group() (bool, error) {

	if !p.nextByteIs('(') {
		return false, nil
	}

	i := p.ev.Level()

	p.WhiteSpace()

	p.Sequence()

	p.WhiteSpace()

	if !p.nextByteIs(')') {
		return false, errors.New("missing )")
	}

	// Level before and after a group is the same
	p.ev.SetLevel(i)
	return true, nil
}

// Scalar ::= quoted | string
func (p *parser) Scalar() (string, bool) {
	b, ok := p.Quoted()
	if ok {
		return b, true
	}
	return p.String()
}

// Comment consumes anything from # up to the end of the line.
//
// TODO: Should expect a space after # !
//
func (p *parser) Comment() bool {
	c := p.Read()

	if c == '#' {
		for {
			c = p.Read()
			if isEndChar(c) || isBreakChar(c) {
				break
			}
		}
		return true
	}
	p.Unread()
	return false
}

// String is a concatenation of characters that are > 0x20
// and are not '(', ')', ',', and do not begin with '#'.
//
// NOTE: '#' is allowed inside a string. For '#' to start
// a comment it must be preceeded by break or space, or come
// after a closing ')'.
//
// TOTHINK: Many productions return a string and not []byte, which could be
// more efficient, but has no type information: []byte can be a raw binary or
// a string.
func (p *parser) String() (string, bool) {

	c := p.Read()

	if !isTextChar(c) || c == '#' {
		p.Unread()
		return "", false
	}

	buf := make([]byte, 1, 16)
	buf[0] = byte(c)

	for {
		c = p.Read()
		if !isTextChar(c) {
			p.Unread()
			break
		}
		buf = append(buf, byte(c))
	}

	return string(buf), true
}

// Quoted string. Can have newlines in it.
func (p *parser) Quoted() (string, bool) {

	cs := p.Read()
	if cs != '"' && cs != '\'' {
		p.Unread()
		return "", false
	}

	buf := make([]byte, 0, 16)

	// p.lastnl is the indentation of this quoted string
	lnl := p.lastnl

	/* Handle \", \', and spaces after NL */
	for {
		c := p.Read()
		if c == cs {
			break
		}

		buf = append(buf, byte(c))

		if c == 10 {
			_, n := p.Space()
			// There are n spaces. Skip lnl spaces and add rest.
			for ; n-lnl > 0; n-- {
				buf = append(buf, ' ')
			}
		} else if c == '\\' {
			c = p.Read()
			if c != '"' && c != '\'' {
				buf = append(buf, '\\')
			}
			buf = append(buf, byte(c))
		}
	}

	// May have zero length
	return string(buf), true
}

// Block ::= '\\' NL LINES_OF_TEXT
func (p *parser) Block() (string, bool) {

	c := p.Read()
	if c != '\\' {
		p.Unread()
		return "", false
	}

	c = p.Read()
	if c != 10 && c != 13 {
		p.Unread()
		p.Unread()
		return "", false
	}

	// read lines until indentation is >= indentation of upper level.
	i := 0
	if p.ev.Level() > 0 {
		i = p.ind[p.ev.Level()-1]
	}

	u, ns := p.Space()

	if u && ns == 0 {
		println("Non uniform space at beginning of block at line", p.line)
		panic("")
	}

	buffer := &bytes.Buffer{}

	j := ns

	for {
		if j <= i {
			p.spaces = j /// XXX: unread spaces!
			break
		}

		// Adjust indentation if less that initial
		if j < ns {
			ns = j
		}

		// Read bytes until end of line
		for {
			c = p.Read()

			buffer.WriteByte(byte(c))
			if c == 13 {
				continue
			}

			if c == 10 || p.End() {
				break
			}
		}

		_, j = p.Space()
	}

	// Remove trailing NL
	if c == 10 {
		if buffer.Len() > 0 {
			buffer.Truncate(buffer.Len() - 1)
		}
	}

	return buffer.String(), true
}

// Break is NL, CR or CR+NL
func (p *parser) Break() bool {
	c := p.Read()
	if c == 13 {
		c = p.Read()
		if c != 10 {
			p.Unread()
		}
		return true
	}
	if c == 10 {
		return true
	}
	p.Unread()
	return false
}

// WhiteSpace is equivalent to Space | Break. It consumes all white space,
// whether spaces, tabs or newlines
func (p *parser) WhiteSpace() bool {

	any := false
	for {
		c := p.Read()
		if c != 13 && c != 10 && c != 9 && c != 32 {
			break
		}
		any = true
	}

	p.Unread()
	return any
}

// Space is (0x20|0x09)+. It returns a boolean indicating
// if space has been found, and an integer indicating
// how many spaces, iff uniform (either all 0x20 or 0x09)
func (p *parser) Space() (bool, int) {

	// The Block() production eats to many spaces trying to
	// detect the end of it. They are saved in p.spaces.
	if p.spaces > 0 {
		i := p.spaces
		p.spaces = 0
		return true, i
	}

	c := p.Read()
	if c != 32 && c != 9 {
		p.Unread()
		return false, 0
	}

	n := 1

	// We keep 'c' to tell us what spaces will count as uniform.

	for {
		cs := p.Read()
		if cs != 32 && cs != 9 {
			p.Unread()
			break
		}
		if n != 0 && cs == c {
			n++
		} else {
			n = 0
		}
	}

	return true, n
}

// End returns true if the end of stream has been reached.
//
// end < stream > bool
func (p *parser) End() bool {
	c := p.Read()
	if c < 32 && c != 9 && c != 10 && c != 13 {
		return true
	}
	p.Unread()
	return false
}

// Newline returns true is a newline is found at the current position.
func (p *parser) Newline() bool {
	c := p.Read()
	if c == '\r' {
		c = p.Read()
	}

	if c == '\n' {
		return true
	}

	p.Unread()
	return false
}

// Token reads from the Parser input stream and returns
// a token or nil. A token is defined as a sequence of
// letters and/or numbers and/or _.
//
// Examples of tokens:
//  _a
//  1
//  143lasd034
//
func (p *parser) Token() (string, bool) {

	c := p.Read()

	if !isTokenChar(c) {
		p.Unread()
		return "", false
	}

	buf := make([]byte, 1, 16)
	buf[0] = byte(c)

	for {
		c = p.Read()
		if !isTokenChar(c) {
			p.Unread()
			break
		}
		buf = append(buf, byte(c))
	}

	return string(buf), true
}

// Number returns true if it finds a number at the current parser position
// It returns also the number found.
func (p *parser) Number() (string, bool) {

	c := p.Read()

	if !isDigit(c) {
		if c != '-' {
			p.Unread()
			return "", false
		}
		d := p.Read()
		if !isDigit(d) {
			p.Unread()
			p.Unread()
			return "", false
		}
		p.Unread()
	}

	buf := make([]byte, 1, 16)
	buf[0] = byte(c)

	for {
		c = p.Read()
		if !isDigit(c) && c != '.' {
			p.Unread()
			break
		}
		buf = append(buf, byte(c))
	}

	return string(buf), true
}

// Operator returns true if it finds an operator at the current parser position
// It returns also the operator found.
func (p *parser) Operator() (string, bool) {

	c := p.Read()

	if !isOperatorChar(c) {
		p.Unread()
		return "", false
	}

	buf := make([]byte, 1, 16)
	buf[0] = byte(c)

	for {
		c = p.Read()
		if !isOperatorChar(c) {
			p.Unread()
			break
		}
		buf = append(buf, byte(c))
	}

	return string(buf), true
}

// Expression := expr1 (op2 expr1)*
//
func (p *parser) Expression() bool {
	if !p.UnaryExpression() {
		return false
	}

	for {
		p.Space()
		b, ok := p.Operator()
		if ok {
			p.ev.Add(b)
		} else {
			return true
		}
		p.Space()
		if !p.UnaryExpression() {
			return false // error
		}
		p.Space()
	}
}

// UnaryExpression := cpath | constant | op1 cpath | op1 constant | '(' expr ')' | op1 '(' expr ')'
//
func (p *parser) UnaryExpression() bool {

	c := p.Read()
	p.Unread()

	if isLetter(c) {
		p.ev.Add(TypePath)
		p.ev.Inc()
		p.Path()
		p.ev.Dec()
		return true
	}

	b, ok := p.Number()
	if ok {
		p.ev.Add(b)
		return true
	}

	b, ok = p.Quoted()
	if ok {
		p.ev.Add(TypeString)
		p.ev.Inc()
		p.ev.Add(b)
		p.ev.Dec()
		return true
	}

	b, ok = p.Operator()
	if ok {
		p.ev.Add(b)
	}

	if p.nextByteIs('(') {

		p.ev.Add(TypeGroup)
		p.ev.Inc()
		p.Space()
		p.Expression()
		p.Space()
		p.ev.Dec()

		return p.nextByteIs(')')
	}

	return p.Path()
}

// Text parses text in a template.
func (p *parser) Text() bool {

	c := p.Read()

	if !isTemplateTextChar(c) {
		p.Unread()
		return false
	}

	buf := make([]byte, 1, 16)
	buf[0] = byte(c)

	for {
		c := p.Read()
		if !isTemplateTextChar(c) {

			p.Unread()
			break
		}
		buf = append(buf, byte(c))
	}

	p.ev.AddBytes(buf)
	return true
}

// Variable parses variables in a template. They begin with $.
func (p *parser) Variable() bool {

	c := p.Read()

	if c != '$' {
		p.Unread()
		return false
	}

	c = p.Read()
	if c == '\\' {
		p.ev.Add("$")
		return true
	}

	p.Unread()

	i := p.ev.Level()

	c = p.Read()
	if c == '(' {
		p.ev.Add(TypeExpression)
		p.ev.Inc()
		p.Expression()
		p.Space()
		p.Read() // Should be ')'
	} else {
		p.ev.Add(TypePath)
		p.ev.Inc()
		if c != '{' {
			p.Unread()
		} else {
			p.Space()
		}
		p.Path()
		if c == '{' {
			p.Space()
			p.Read() // Should be '}'
		}
	}

	// Reset the level
	p.ev.SetLevel(i)

	return true

}

// Index ::= '[' expression ']'
func (p *parser) Index() bool {

	if !p.nextByteIs('[') {
		return false
	}

	i := p.ev.Level()

	p.ev.Add(TypeIndex)
	p.ev.Inc()

	p.Space()
	p.Expression()
	p.Space()

	if !p.nextByteIs(']') {
		return false // error
	}

	/* Level before and after is the same */
	p.ev.SetLevel(i)
	return true
}

// Selector ::= '{' expression? '}'
func (p *parser) Selector() bool {

	if !p.nextByteIs('{') {
		return false
	}

	i := p.ev.Level()

	p.ev.Add(TypeSelector)
	p.ev.Inc()

	p.Space()
	p.Expression()
	p.Space()

	if !p.nextByteIs('}') {
		return false // error
	}

	/* Level before and after is the same */
	p.ev.SetLevel(i)
	return true
}

// Args ::= '(' space? sequence? space? ')'
func (p *parser) Args(dot bool) (bool, error) {

	if !p.nextByteIs('(') {
		return false, nil
	}

	i := p.ev.Level()

	if !dot {
		p.ev.Add(TypeGroup)
	} else {
		p.ev.Add(TypeExpression)

	}
	p.ev.Inc()

	p.Space()
	p.ArgList()
	p.Space()

	if !p.nextByteIs(')') {
		return false, errors.New("missing )")
	}

	/* Level before and after is the same */
	p.ev.SetLevel(i)
	return true, nil
}

// ArgList ::= space? expression space? [, space? expression]* space?
//
// arglist < stream > events
//
// arglist can be empty, then returning false (this fact is not represented
// in the BNF definition).
//
func (p *parser) ArgList() bool {

	something := false

	level := p.ev.Level()
	defer p.ev.SetLevel(level)

	for {
		p.Space()

		p.ev.Add(TypeExpression)
		p.ev.Inc()
		if !p.Expression() {
			p.ev.Dec()
			p.ev.Delete()
			return something
		}
		p.ev.Dec()
		something = true

		p.Space()
		if !p.nextByteIs(',') {
			p.ev.Inc()
		} else {
			p.ev.SetLevel(level)
		}
	}
}

// Template ::= (Text | Variable)*
func (p *parser) Template() {
	for {
		if !p.Text() && !p.Variable() {
			break
		}
	}
}

// TokenList ::= token [, token]*
func (p *parser) TokenList() {

	comma := false

	for {
		p.Space()

		if comma && !p.nextByteIs(',') {
			return
		}

		p.Space()

		s, ok := p.Token()
		if !ok {
			return
		}

		p.emit(s)
		comma = true
	}
}
