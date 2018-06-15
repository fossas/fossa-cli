// Package monad implements common monomorphic monads.
package monad

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
