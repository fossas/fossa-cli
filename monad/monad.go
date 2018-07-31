// Package monad implements common monomorphic monads.
package monad

import "github.com/fossas/fossa-cli"

// EitherStr is a monomorphic Either monad specialized to string. I miss Haskell.
type EitherStr struct {
	Result string
	Err    error
}

// EitherStrFunc defines monadic EitherStr functions.
type EitherStrFunc func(previous string) (string, error)

// Bind lifts EitherStrFuncs into the EitherStr monad.
func (r *EitherStr) Bind(f EitherStrFunc) *EitherStr {
	if r.Err != nil {
		return r
	}

	result, err := f(r.Result)
	if err != nil {
		r.Err = err
		return r
	}

	r.Result = result
	return r
}

// EitherVCSType is an Either monad specialized to VCSType.
type EitherVCSType struct {
	Result cli.VCSType
	Err    error
}

// EitherVCSTypeFunc defines monadic EitherStr functions.
type EitherVCSTypeFunc func(previous cli.VCSType) (cli.VCSType, error)

// BindVCSType lifts EitherVCSTypeFuncs into the EitherVCSType monad.
func (r *EitherVCSType) BindVCSType(f EitherVCSTypeFunc) *EitherVCSType {
	if r.Err != nil {
		return r
	}

	result, err := f(r.Result)
	if err != nil {
		r.Err = err
		return r
	}

	r.Result = result
	return r
}
