package graph

type Tree interface {
	Children() []Tree
}

type FlattenFunc func(node interface{}) (key interface{}, value interface{})

func Flatten(pkgMap interface{}, root Tree, flatten FlattenFunc) {}
