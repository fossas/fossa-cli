// Copyright 2012-2017, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package ogdl is used to process OGDL, the Ordered Graph Data Language.
//
// OGDL is a textual format to write trees or graphs of text, where
// indentation and spaces define the structure. Here is an example:
//
//    network
//      ip 192.168.1.100
//      gw 192.168.1.9
//
// The languange is simple, either in its textual representation or its
// number of productions (the specification rules), allowing for compact
// implementations.
//
// OGDL character streams are normally formed by Unicode characters, and
// encoded as UTF-8 strings, but any encoding that is ASCII transparent
// is compatible with the specification.
//
// See the full spec at http://ogdl.org.
//
// Installation
//
// To install this package just do:
//
//     go get github.com/rveen/ogdl
//
// An example
//
// If we have a text file 'config.g' containing:
//
//    eth0
//      ip
//        192.168.1.1
//      gateway
//        192.168.1.10
//      mask
//        255.255.255.0
//      timeout
//        20
//
// then,
//
//    g := ogdl.FromFile("config.g")
//    ip := g.Get("eth0.ip").String()
//    to := g.Get("eth0.timeout").Int64(60)
//
//    println("ip:",ip,", timeout:",to)
//
// will print
//
//    ip: 192.168.1.1, timeout: 20
//
// If the timeout parameter was not present, then the default value (60) will be
// assigned to 'to'. The default value is optional, but be aware that Int64() will
// return 0 in case that the parameter doesn't exist.
//
// The configuration file can be written in a conciser way:
//
//    eth0
//      ip      192.168.1.1
//      gateway 192.168.1.10
//      mask    255.255.255.0
//      timeout 20
//
// A template example
//
// The package includes a template processor. It takes an arbitrary input stream
// with some variables in it, and produces an output stream with the variables
// resolved out of a Graph object which acts as context.
//
// For example (given the previous config file):
//
//     g := ogdl.FromFile("config.g")
//     t := ogdl.NewTemplate("The gateway's IP is $eth0.gateway")
//     b := t.Process(g)
//
// string(b) is then:
//
//     The gateway's IP is 192.168.1.10
//
// Function signature conventions
//
// Some rules are followed:
//
//   .<Type>()      Return the first subnode content converted to the specified type.
//
//   .This<Type>()  Return the node content itself converted to the specified type.
//
//   .Get()         Return the specified path as a (possible nil) *Graph object.
//
//   .Get<Type>()   Return the specified path converted to the specified type.
//                  These series of functions return value and error.
//
package ogdl
