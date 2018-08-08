// Copyright 2012-2015, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

import (
	"encoding/binary"
	"errors"
	"log"
	"net"
	"time"
)

// RFunction represents a remote function (also known as a remote procedure
// call).
type RFunction struct {
	Host     string
	Protocol int
	conn     net.Conn
}

func NewRFunction(host string) *RFunction {
	return &RFunction{host, 1, nil}
}

// connect opens a connection to a TCP/IP server.
func (rf *RFunction) connect() error {

	tcpAddr, err := net.ResolveTCPAddr("tcp", rf.Host)
	if err != nil {
		return err
	}

	conn, err := net.DialTCP("tcp", nil, tcpAddr)
	if err != nil {
		return err
	}

	rf.conn = conn
	return nil
}

func TCPRawServer(host string, handler func(c net.Conn, b []byte) []byte, timeout int) error {

	l, err := net.Listen("tcp", host)
	if err != nil {
		return err
	}

	for {
		// Wait for a connection.
		conn, err := l.Accept()
		if err != nil {
			log.Println(err)
			continue
		}

		// Handle the connection in a new goroutine.
		go rawhandler(conn, handler, timeout)
	}
}

func TCPServerV2(host string, handler func(net.Conn, *Graph) *Graph, timeout int) error {

	l, err := net.Listen("tcp", host)
	if err != nil {
		return err
	}

	for {
		// Wait for a connection.
		conn, err := l.Accept()
		if err != nil {
			log.Println(err)
			continue
		}

		// Handle the connection in a new goroutine.
		go handlerV2(conn, handler, timeout)
	}
}

func TCPServerV1(host string, handler func(net.Conn, *Graph) *Graph, timeout int) error {

	l, err := net.Listen("tcp", host)
	if err != nil {
		return err
	}

	for {
		// Wait for a connection.
		conn, err := l.Accept()
		if err != nil {
			log.Println(err)
			continue
		}

		// Handle the connection in a new goroutine.
		go handlerV1(conn, handler, timeout)
	}
}

func handlerV1(c net.Conn, handler func(net.Conn, *Graph) *Graph, timeout int) {

	for {
		// Set a time out (maximum time until next message)
		c.SetReadDeadline(time.Now().Add(time.Second * time.Duration(timeout)))

		// Read the incoming object
		p := newBinParser(c)
		g := p.Parse()

		if g == nil {
			c.Close()
			return
		}

		r := handler(c, g)

		// Write to result back in binary format
		b := r.Binary()
		c.Write(b)
	}
}

func handlerV2(c net.Conn, handler func(net.Conn, *Graph) *Graph, timeout int) {

	b4 := make([]byte, 4)

	log.Println("connection accepted")

	for {

		// Set a time out (maximum time until next message)
		c.SetReadDeadline(time.Now().Add(time.Second * time.Duration(timeout)))

		// Read message: LEN(uint32) BYTES

		// Read 4 bytes (LEN)
		i, err := c.Read(b4)
		// Set a time out (maximum time for receiving the body)
		c.SetReadDeadline(time.Now().Add(time.Millisecond * 1000))

		if err != nil || i != 4 {
			log.Println("connection closed", i, err)
			c.Close()
			return
		}

		l := int(binary.BigEndian.Uint32(b4))

		// read body of message
		buf := make([]byte, l)
		if buf == nil {
			log.Printf("connection closed: cannot allocate % bytes\n", l)
			c.Close()
			return
		}
		i, err = c.Read(buf)

		if err != nil || i != l {
			log.Println("connection pre-closed", err)
			c.Close()
			return
		}

		g := FromBinary(buf)
		r := handler(c, g)

		// Write message back
		buf = r.Binary()
		binary.BigEndian.PutUint32(b4, uint32(len(buf)))

		c.Write(b4)
		c.Write(buf)
	}
}

func rawhandler(c net.Conn, handler func(c net.Conn, body []byte) []byte, timeout int) {

	b4 := make([]byte, 4)

	log.Println("connection accepted")

	for {

		// Set a time out (maximum time until next message)
		c.SetReadDeadline(time.Now().Add(time.Second * time.Duration(timeout)))

		// Read message: LEN(uint32) BYTES

		// Read 4 bytes (LEN)
		i, err := c.Read(b4)
		// Set a time out (maximum time for receiving the body)
		c.SetReadDeadline(time.Now().Add(time.Millisecond * 100))

		if err != nil || i != 4 {
			log.Println("connection closed", i, err)
			c.Close()
			return
		}

		l := int(binary.BigEndian.Uint32(b4))

		// read body of message
		buf := make([]byte, l)
		if buf == nil {
			log.Println("connection closed: not more memory")
			c.Close()
			return
		}
		i, err = c.Read(buf)
		if err != nil || i != l {
			log.Println("connection pre-closed", err)
			c.Close()
			return
		}

		r := handler(c, buf)

		// Write back

		l = len(r)
		binary.BigEndian.PutUint32(b4, uint32(l))
		c.Write(b4)
		c.Write(r)
	}
}

// Call makes a remote call. It sends the given Graph in binary format to the server
// and returns the response Graph.
func (rf *RFunction) Call(g *Graph) (*Graph, error) {

	var r *Graph
	var err error

	if rf.conn == nil {
		err = rf.connect()
		if err != nil {
			return nil, err
		}
		if rf.conn == nil {
			return nil, errors.New("Connection = nil")
		}
	}

	if rf.Protocol == 2 {
		r, err = rf.callV2(g)
	} else {
		r, err = rf.callV1(g)
	}

	if err != nil {
		err = rf.connect()
		if err != nil {
			return nil, err
		}
		if rf.conn == nil {
			return nil, errors.New("Connection = nil")
		}

		if rf.Protocol == 2 {
			r, err = rf.callV2(g)
		} else {
			r, err = rf.callV1(g)
		}
	}

	return r, err
}

func (rf *RFunction) callV2(g *Graph) (*Graph, error) {

	buf := g.Binary()
	buf2 := make([]byte, 4+len(buf))

	for i := 0; i < len(buf); i++ {
		buf2[i+4] = buf[i]
	}

	b4 := make([]byte, 4)
	binary.BigEndian.PutUint32(buf2, uint32(len(buf)))

	// Write request (len + body)
	rf.conn.SetDeadline(time.Now().Add(time.Second * 10))
	rf.conn.Write(buf2)

	// Read header response
	rf.conn.SetReadDeadline(time.Now().Add(time.Second * 10))
	j, _ := rf.conn.Read(b4)
	if j != 4 {
		rf.conn = nil
		log.Println("callv2 error, message header")
		return nil, errors.New("error in message header")
	}
	l := binary.BigEndian.Uint32(b4)

	// Read body response
	buf3 := make([]byte, l)
	j, err := rf.conn.Read(buf3)
	if err != nil {
		rf.conn = nil
		log.Println("callv2", err)
		return nil, err
	}
	if j != int(l) {
		rf.conn = nil
		log.Println("callv2 error, message len")
		return nil, errors.New("error in message len")
	}

	g = FromBinary(buf3)

	return g, nil
}

func (rf *RFunction) callV1(g *Graph) (*Graph, error) {

	b := g.Binary()
	rf.conn.SetDeadline(time.Now().Add(time.Second * 10))
	n, err := rf.conn.Write(b)

	if err != nil {
		rf.conn = nil
		log.Println("callv1", err)
		return nil, err
	}
	if n < len(b) {
		rf.conn = nil
		log.Println("callv1", err)
		return nil, errors.New("could not write all bytes")
	}

	p := newBinParser(rf.conn)
	if p.r == nil {
		rf.conn = nil
		log.Println("callv1, rf.conn buffered reader = nil")
		return nil, errors.New("callv1, rf.conn buffered reader = nil")
	}

	rf.conn.SetReadDeadline(time.Now().Add(time.Second * 10))

	c := p.read()
	if c == -1 {
		rf.conn = nil
		log.Println("callv1", "EOS")
		return nil, errors.New("unexpected EOS")
	}

	p.unread()

	r := p.Parse()

	if r == nil || r.Len() == 0 {
		return nil, errors.New("nil response")
	}

	return r, nil
}

// Close closes the underlying connection, if open.
func (rf *RFunction) Close() {
	if rf.conn != nil {
		rf.conn.Close()
		rf.conn = nil
	}
}
