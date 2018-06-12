// Package monad implements common monomorphic monads.
package monad

// EitherStr is a monomorphic Either monad specialized to string. I miss Haskell.
type EitherStr struct {
	result string
	err    error
}

type EitherStrFunc func(previous string) (string, error)

func (r *EitherStr) Bind(f EitherStrFunc) error {
	if r.err != nil {
		return r.err
	}

	result, err := f(r.result)
	if err != nil {
		r.err = err
		return err
	}

	r.result = result
	return nil
}
