// Copyright 2012-2015, Rolf Veen and contributors.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ogdl

import (
	"errors"
	"fmt"
	"log"
	"reflect"
	"runtime"
)

// Function enables calling Go functions from templates.
//
// INPUT FORMAT
//
// g is the Function's context. g.This contains the presumed class name.
// The _type subnode of g, if present, contains the function type (a Go
// interface name or 'rfunction'
//
// p is the input path, where i points to the current position to be processed.
// The arguments of the function are 1 level higher than the function name.
// p[ix] points to the class name.
//
// Example 1
//
// !p
//   T
//   !g
//     'some text'
//
// Example 2
// !p
//   math
//   Sin
//   !g
//    !e
//      1.0
//
// Functions calls are limited to whole paths.
//
// TODO: Catch panic() att Call(). Return named variables so that defer/recover
// return something usefull

func (g *Graph) function(path *Graph, typ interface{}) (interface{}, error) {

	defer func() {
		if err := recover(); err != nil {
			fmt.Println(err)
			return
		}
	}()

	// log.Printf("\n%s\n", path.Show())

	v := reflect.ValueOf(typ)

	// Remote functions have this signature
	var f func(*Graph) (*Graph, error)
	rfType := reflect.ValueOf(f).Type()

	// Build arguments in the form []reflect.Value
	var vargs []reflect.Value

	switch v.Kind() {

	case reflect.Func:

		//log.Println("function.Func", path.Out[1].ThisString(), path.Out[1].Len())
		// log.Println("Func type", v.Type())
		//log.Println(runtime.FuncForPC(v.Pointer()).Name())
		//log.Println(reflect.TypeOf(typ).String())

		// Pre-evaluate
		var args []interface{}

		if v.Type() == rfType {
			// Remote function
			n := New()
			nn := n.Add(path.Out[1].This)
			if len(path.Out) > 2 {
				for _, arg := range path.Out[2].Out {
					// log.Printf("arg:\n%s\n", arg.Show())
					nn.Add(g.evalExpression(arg))
				}
			}

			log.Println(n.Show())
			args = append(args, n)
		} else {
			// Local function
			for _, arg := range path.Out[1].Out {
				args = append(args, g.evalExpression(arg))
				// log.Printf("%v\n", args[len(args)-1])
			}
		}

		for i, arg := range args {
			if arg == nil {
				// No untyped nil support :-(
				vargs = append(vargs, reflect.Zero(v.Type().In(i)))
			} else {
				vargs = append(vargs, reflect.ValueOf(arg))
			}
		}

		/* DEBUG CODE
		for i := 0; i < v.Type().NumIn(); i++ {
			log.Println("> ", v.Type().In(i).String())
		}
		for i := 0; i < len(vargs); i++ {
			log.Println("< ", vargs[i].Type().String())
		} /**/

		if v.Type().NumIn() != len(args) {
			// TODO Check that we print the name of the function
			return nil, fmt.Errorf("Invalid number of arguments in function %s (is %d, soll %d)\n%s", runtime.FuncForPC(v.Pointer()).Name(), len(args), v.Type().NumIn(), path.Show())
		}

		// TODO: return 0..2 values
		vv := v.Call(vargs)
		if len(vv) > 0 {
			return vv[0].Interface(), nil
		}
		return nil, nil

	case reflect.Ptr:

		// log.Println("function.Ptr")

		fn := path.GetAt(1)
		if fn == nil {
			return nil, errors.New("No method")
		}
		fname := fn.ThisString()

		// Check if it is a method
		me := v.MethodByName(fname)

		if !me.IsValid() {
			// Try field
			if v.Kind() == reflect.Struct {
				v = v.FieldByName(fname)
				if v.IsValid() {
					return v.Interface(), nil
				}
			}

			return nil, errors.New("No method or field: " + fname)
		}

		// Pre-evaluate
		var args []interface{}
		if len(path.Out) > 2 {
			for _, arg := range path.Out[2].Out {
				args = append(args, g.evalExpression(arg))
			}
		}

		for i, arg := range args {
			if arg == nil {
				// No untyped nil support :-(
				vargs = append(vargs, reflect.Zero(me.Type().In(i)))
			} else {
				vargs = append(vargs, reflect.ValueOf(arg))
			}
		}

		// TODO: variadic
		/*
			if me.Type().NumIn() != len(args) {
				return nil, errors.New("Invalid number of arguments in method " + fname)
			}

			for i, arg := range args {
				v := reflect.TypeOf(arg)
				if v == nil || me.Type().In(i).String() != v.String() {
					return nil, errors.New("Invalid argument for method " + fname)
				}
			}*/

		// TODO: return 0..2 values
		vv := me.Call(vargs)
		if len(vv) > 0 {
			return vv[0].Interface(), nil
		}
		return nil, nil

	default:
		return nil, nil
	}

}
