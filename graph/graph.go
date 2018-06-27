package graph

import (
	"errors"
	"reflect"

	"github.com/fossas/fossa-cli/log"
	"github.com/fossas/fossa-cli/pkg"
)

type Graph map[interface{}][]interface{}

type Deps map[pkg.ID]pkg.Package

type Rooted struct {
	Imports []interface{}
	Graph   Graph
}

// See https://jimmyfrasche.github.io/go-reflection-codex/
func Unwrap(imports interface{}, graph interface{}, result Rooted) error {
	log.Logger.Debugf("result.Imports: %#v", result.Imports)
	log.Logger.Debugf("result.Graph: %#v", result.Graph)

	err := unwrapImports(imports, result)
	if err != nil {
		return err
	}

	err = unwrapGraph(graph, result)
	if err != nil {
		return err
	}

	log.Logger.Debugf("imports: %#v", imports)
	log.Logger.Debugf("graph: %#v", graph)
	return nil
}

func unwrapImports(imports interface{}, result Rooted) error {
	if len(result.Imports) == 0 {
		return nil
	}

	// Get type of result.
	ri := reflect.ValueOf(result.Imports)

	// Check type of imports.
	iptr := reflect.ValueOf(imports)
	if iptr.IsNil() || iptr.Kind() != reflect.Ptr {
		return errors.New("imports must be a non-nil pointer")
	}
	i := iptr.Elem()
	if i.Kind() != reflect.Slice {
		return errors.New("imports must be a pointer to an array")
	}
	s := i.Type()
	e := s.Elem()
	if e != ri.Index(0).Elem().Type() {
		return errors.New("imports type does not match result.Imports type")
	}

	// Construct correct slice for imports.
	i.Set(reflect.MakeSlice(s, 0, len(result.Imports)))
	for _, dep := range result.Imports {
		i.Set(reflect.Append(i, reflect.ValueOf(dep).Convert(e)))
	}

	return nil
}

func unwrapGraph(graph interface{}, result Rooted) error {
	if len(result.Graph) == 0 {
		return nil
	}

	// Get type of result.
	rg := reflect.ValueOf(result.Graph)

	// Check type of graph.
	gptr := reflect.ValueOf(graph)
	if gptr.IsNil() || gptr.Kind() != reflect.Ptr {
		return errors.New("graph must be a non-nil pointer")
	}
	g := gptr.Elem()
	if g.Kind() != reflect.Map {
		return errors.New("graph must be a pointer to a map")
	}
	m := g.Type()
	if reflect.SliceOf(m.Key()) != m.Elem() {
		return errors.New("graph must be of type map[T][]T")
	}
	keys := rg.MapKeys()
	if m.Key() != keys[0].Elem().Type() || m.Elem().Elem() != rg.MapIndex(keys[0]).Index(0).Elem().Type() {
		return errors.New("graph type does not match result.Graph type")
	}

	// Construct correct map for graph.
	g.Set(reflect.MakeMapWithSize(m, len(result.Graph)))
	for parent, children := range result.Graph {
		c := reflect.MakeSlice(m.Elem(), 0, len(children))
		for _, child := range children {
			c = reflect.Append(c, reflect.ValueOf(child).Convert(m.Elem().Elem()))
		}
		g.SetMapIndex(reflect.ValueOf(parent).Convert(m.Key()), c)
	}

	return nil
}
