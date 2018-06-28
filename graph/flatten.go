package graph

import "github.com/cheekybits/genny/generic"

type Node generic.Type

type Tree interface {
	Children() []Tree
}

type FlattenFunc func(node Node) (key Node, value Node)

func Flatten(pkgMap Node, root Tree, flatten FlattenFunc) {}
