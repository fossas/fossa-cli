# Testing with `fused-effects`

## The Previous State of Things

The `Test.Hspec` module provides the following API (simplified, but essentially correct):

```haskell
-- This type alias is used in hspec, but we will just use @IO ()@ here.
type Expectation = IO ()

-- * Defining Tests
-- Ignore these types, just note the string parameters.
describe :: String -> ...
it :: String -> ...

-- * Assertions
shouldBe :: Eq a => a -> a -> IO ()
shouldStartWith :: Eq a => [a] -> [a] -> IO ()
shouldFoo :: ... -> IO ()
shouldBar :: ... -> IO ()

-- Force failure
expectationFailure :: String -> IO ()
```

Which allows us to write tests like this:

```haskell
spec = do
  describe "'&&' operator" $
    it "should and two Bools" $
      True && False `shouldBe` False
```

## The Problem

While `hspec` is useful, it is limiting in the fact that we cannot provide assertion in effectful monads,
and must reduce every effectful monad to `IO` before making any assertion.  This has made tests
very annoying when testing effectful code, requiring extensive use of effect runners as test
boilerplate.

```haskell
import Control.Carrier.Diagnostics
import Wherever.Result.Lives

interestingFunction :: Has Diagnostics sig m => Maybe a -> m a
interestingFunction = fromMaybeText "the value was Nothing"

spec = do
  describe "interestingFunction" $
    it "should return the value if the value is Just" $ do
      -- DOES NOT COMPILE
      -- hspec runs tests in the IO monad, and we're using some anonymous monad.
      value <- interestingFunction $ Just "foo"
      value `shouldBe` "foo"

    it "should reduce to the initial value after bing unwrapped" $ do
      -- this version compiles, but adds a bit of boilerplate
      let result = runDiagnostics . interestingFunction $ Just "bar"
      case result of
          Success _ a -> a `shouldBe` "bar"
          Failure _ _ -> expectationFailure "result was not successful"
```

In the above example, we have to run the effect to get to the value.  We basically lose the power
of monads, because `hspec` doesn't let us use any monad except their own, which is `IO`.  While the
boilerplate of that example isn't so bad, it grows significantly with the number of tests, and with
the number of effects.  We use effects everywhere, so this qwuickly becomes very annoying to work
with.

## The Solution

We really want the first version of the last example to compile, and ideally we don't even have to
know which effects we're using, since almost all of them come from business logic, and not from
test functions.

To do this, we hijack all of `hspec`'s functions, and provide effectful versions of them.  This
is done in the `Test.Effect` module.


```haskell
module Test.Effect where
-- * Defining Tests
-- We can't modify describe, the types don't really work out.
describe :: String -> ...
-- Note the ' at the end of all the function below
it' :: String -> ...

-- * Assertions
shouldBe' :: Eq a => a -> a -> m ()
shouldStartWith' :: Eq a => [a] -> [a] -> m ()
shouldFoo' :: ... -> m ()
shouldBar' :: ... -> m ()

expectationFailure :: String -> m ()
```

Which allows us to write tests like this:

```haskell
import Control.Carrier.Diagnostics

interestingFunction :: Has Diagnostics sig m => Maybe a -> m a
interestingFunction = fromMaybeText "the value was Nothing"

spec = do
  describe "interestingFunction" $
    it' "should return the value if the value is Just" $ do
      -- Compiles!
      value <- interestingFunction $ Just "foo"
      -- Note that we use shouldBe' instead of shouldBe
      value `shouldBe'` "foo"
```

Simply by adding `'` to the end of `hspec` function names, it Just Works™️!  Note that we had to add
the prime marker to both `it` and `shouldBe`.

## Migration

To simplify the usage of tests that use effects, simply start by defining the test with `it'`.  Then
remove all effect handlers, and just use the monadic form of the effects. Then add `'` to all of the
`hspec` functions that complain, and you should be basically done. Some tests may need more work to
migrate fully, but this is basically enough for most cases.
