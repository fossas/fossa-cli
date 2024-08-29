# Style guide for `fossa-cli`

[Back to development doc homepage](README.md)

Through a number of pull requests, reviews, and discussions, a codebase style
has emerged.  This document is an attempt to capture that, and prevent further
discussions that slow down work unnecessarily.

This style guide is forked from Github's
[semantic](https://github.com/github/semantic).

## General guidelines

Make your code look like the code around it. Consistency is the name of the
game.

Don't spent too much effort making the code follow these exact guidelines if
the code is more readable without them.  You should submit changes to this doc
if you think you can improve it, or if a case should be covered by this doc,
but currently is not.

Use `fourmolu` for formatting.  Our CI setup enforces that all changes pass a
`fourmolu` run with no differences.

Our CI systems ensure that all patches pass `hlint`'s muster. We have our own
set of rules in `.hlint.yaml`.

We strongly recommend adding Haddock documentation to any top-level
declaration, unless its purpose is immediately apparent from its name. Comments
should describe the "why", type signatures should describe the "what", and the
code should describe the "how".

## Formatting

2 spaces everywhere. Tabs are forbidden.  These are enforced by the formatter.

We have a soft line limit of 80, but don't sacrifice readability for shorter
lines.  That said, 80 chars is a lot, and you should probably split code lines
earlier than that.  Write code vertically, not horizontally.

### Use applicative notation when constructing simple data types

``` haskell
thing :: Parser Foo

-- Bad:
thing = do
  a <- bar
  b <- baz
  pure (Foo a b)

-- Better:
thing = Foo <$> bar <*> baz
```

Code written with applicative notation is less powerful than code written in
`do`-notation (since that code is, by definition, monadic).  As a result, it can
be easier to reason about, as long as it's also readable.

Over-reliance on applicative notation can create code that is difficult to read.
If in doubt, write it with `do` notation and see if it's more immediately
comprehensible.

#### Don't overuse `do` notation in parsers

Parsers should make extra effort to not use do notation.  However, this only
applies to applicative parsers.  There is no reason not to use do notation for
monadic parsers.

In parsers, it can seem more readable to use do notation, especially given the
extra functor and applicative operators (`*>`, `$>`, `<*`, `<$`). However, a
parser is one of the areas where having less power makes a significant
difference in reasoning about the code.  For this reason, we are willing to
sacrifice some readability (in the form of operator-heavy functions).

See our [best practices](parsing-best-practices.md) doc for more info
on writing good parsers.

### Use leading commas for records, exports, and lists

Leading commas make it easy to add and remove fields without introducing syntax
errors, and properly aligned records are easy to read:

``` haskell
data Pos = Pos
  { posLine :: Int
  , posColumn :: Int
  }
```

This rule is enforced by the formatter.

## Naming

Locally bound variables (such as the arguments to functions, or helpers defined
in a `where` clause) can have short names, such as `x` or `go`. Globally bound
functions and variables should have descriptive names.

You'll often find yourself implementing functions that conflict with Prelude
definitions. If this is the case, avoid adding a prefix to these functions, and
instead import them qualified.

``` haskell
-- Bad
foo = heapLookup thing
-- Better
foo = Heap.lookup thing
```

## Functions

### Don't go crazy with infix operators

Sensible use of infix operators can provide serious readability benefits, but
often the best tool is just a named function. If you're defining new operators,
make sure that you have a solid justification for doing so.

### Avoid list comprehensions

In almost all cases, `map`, `filter`, `fold`, and the `[]` monad are more
flexible and readable.

### Do not use match guards

While matching data at the function binding is appealing as an idea, but has
several drawbacks:

- Match guards in function definitions are adding more info into an area
that may already be cluttered.
- Match guards with multiple bindings are essentially the same syntax as list
comprehensions, which are also forbidden.
- Match guards offer an alternative to `case` and `if`, which makes it harder
to establish a guideline of "There should only be one right and obvious way to
do something" (which is a loose goal for the codebase).

If your match guards are any less readable than the introductory examples in
[Learn you a haskell][guards], then you should stick to `case` and `if`
expressions within the function body.

[guards]: https://learnyouahaskell.github.io/syntax-in-functions.html#guards-guards

### Don't go crazy with point-free definitions

Point-free style can help or hinder readability, depending on the context. If a
function is expressed naturally with the `.` operator, then do so, but if you
have to over-complicate the definition to write it point-free, then you should
probably just write out the variable names. If you are reviewing someone else's
PR and find a point-free definition hard to read, ask them to simplify/clarify
it.

### Prefer `.` and `$` to parentheses

Parentheses can make a function harder to edit, since parentheses have to be
balanced. The composition and application operators (`.` and `$`) can reduce
clunkiness.

``` haskell
-- Bad
f (g (h (i x)))
-- Okay
f $ g $ h $ i x
-- Better
f . g . h $ i x
```

However, `$` can easily be over-used (for example, on every single function
application), so `hlint` will warn us in the case that removing the `$` produces
the same effect as including it.

```haskell
-- Bad: Hlint will reject this
h . i $ x
-- Good
h $ i x
```

Parentheses are useful when there are other operators in the mix, since they
don't have conflicting fixity like `$` and `.` have.  `<>` doesn't play well
with `.` and `$`, for example.  Use your best judgement here.

### Do not use partial functions

Never use a partial function when a safe version will work instead.  Do not use
`error` or `undefined`.

In some cases, partial functions are necessary, like indexing lists.  In this
case, you must prove the safety of the function before using, ideally with an
accompanying comment explaining the safety.

Currently, our CI linter prevents adding known partial functions, and you should
try very hard to prevent including them.

### Prefer `map` to `fmap` or `<$>`

When operating on a list, using `map` tells the reader "I'm transforming a
list", where `fmap` tells them "I'm transforming *some* functor, and it doesn't
matter which one."

```haskell
appendFoo t = t <> "-foo"
txts = ["Hello", "darkness", "my", "old", "friend"]

-- Ambiguous
appendFoo <$> txts
-- Slightly better
fmap appendFoo txts
-- Clear and Obvious
map appendFoo txts
```

### Don't use `&` or `<&>`

`&` and `<&>` are the flipped versions of `$` and `<$>`, and it is tempting to
use them, since they read from left-to-right, like english.  However, the
usefulness of `$` is to remove parentheses, and `&` makes that a lot harder:

```haskell
-- Raw parens
foo (bar baz (quux))
-- Using $
foo $ bar baz $ quux
-- Using &
quux & bar baz & foo
```

`<$>` is meant to mimic `$`, but for functors.  Therefore, if we use `$`
instead of `&`, we should also use `<$>` instead of `<&>`.

### Prefer unflipped versions of certain ubiquitous functions/operators

Unlike the previous rule about `&` and `<&>`, this is a general guideline:

Using standard functions/operators (or at least more common versions of them)
prevents people from having to learn too many definitions, or having to rely
on HLS to read the code.  This is alleviated somewhat by operators which contain
directional arrows, and those can be used where necessary.

You SHOULD use flipped versions of operators to keep the functional flow moving
in the same direction (from the perspective of the reader).  See below for an example.

```haskell
-- Assuming this existed in the GHC prelude
(.&) = flip (.)
-- Bad, order of functions is mixed up
firstOp >>= thirdOp . secondOp
-- Bad, ordering is defined, but the operator has no pointing chars,
-- and readers unfamiliar with the function can't figure it out just by reading
firstOp >>= secondOp .& thirdOp
-- Good, order is cleanly defined right-to-left, with arrows and familiar functions
thirdOp . secondOp =<< firstOp
```

Note that `for = flip traverse` (same with `traverse_` and `for_`), and is very useful
when defining an anonymous pipeline for traversal (that comes up a lot during graph
building).  There are other known exceptions, but they are much less common.  When in
doubt, use your best judgement, these cases are not likely to cause much review holdup.

## Data Types

### Prefer `newtype`s to `type`s

`newtype` values are zero-cost to construct and eliminate, and provide more
informative error messages than `type` synonyms. Only use `type` for
convenience aliases to existing types.

### Don't use `String`

`String` is almost always the wrong choice. If your type represents
human-readable strings, use `Text`; if you have a blob of bytes, use
`ByteString`. `-XOverloadedStrings` is enabled globally to make this easy.

### Only use record selectors on single-constructor types

The following code generates two partial functions, which is bad:

``` haskell
data Bad = Evil { getInt :: Int }
         | Bad  { getFloat :: Float }

-- getFloat (Evil 1) throws an error
-- so does getInt (Bad 1)
```

Often, this is incorrect design.  You usually want something like the following
three examples instead:

```haskell
-- Alternative 1
data Good
  = Okay Int
  | NotBad Float

-- Alternative 2
data Alright = Alright
  { getInt :: Maybe Int
  , getFloat :: Maybe Float
  }

-- Alternative 3
-- This is a little bit heavyweight, but is really strongly typed.
newtype SpecialInt = SpecialInt { getInt :: Int }
newtype SpecialFloat = SpecialFloat { getFloat :: Float }
data Great
  = NiceInt SpecialInt
  | CoolFloat SpecialFloat
```

## Imports

### Don't qualify imports with single letters

Qualified imports deserve intelligent names, like any other variable.  Some
common instances are caught by `hlint`:

```haskell
import Data.Text qualified as T -- bad; hlint will catch this specific case
import Data.Text qualified as Text -- Good!
```

We do have some common exceptions for longer module names:

- `Data.Bytestring` -> `BS`
- `Data.ByteString.Lazy` -> `BL`

Use your best judgement here, but in general, shorter names should not be
abbreviated.

### Don't use implicit blanket imports

Import items explicitly:

```haskell
import Control.Effect.Diagnostics -- bad
import Control.Effect.Diagnostics (fatal, context)  -- Good
```

Implicit imports are confusing, especially during code review or text editing
without an IDE.  VSCode has a code action that will rewrite your implicit
imports to explicit automatically.  You may also want to refine your imports to
their "true" locations (VSCode has an action for this as well), but this is not
always helpful, so it's not required.

## Strings

### Don't use the bare conversion routines

For converting between `Bytestring`/`Text`/`String`, including the lazy forms
of `Bytestring` and `Text`, use the internal `Data.String.Conversion` module.
We export as many string conversion routines as we've found use for, plus more.
Use these instead of `decodeUtf8`/`encodeUTF8`/`Text.pack`
/`Text.unpack`/`toFilePath`.  `hlint` will catch some of these, but not all.

## Miscellaneous

- Don't use `{-# ANN … #-}` to disable hlint warnings, as it can slow down
  compilation. If you need to disable lints in a file, do so in `.hlint.yaml`.
- Don't use `do` notation for single items.  If removing the `do` keyword
  compiles identically, then remove it.
- Avoid nested `where` blocks.  If you feel that you need them, rethink your
  design.  Consider making an `Internal` module instead.
- In do-notation, use `let` bindings, otherwise, use `where` clauses.  Don't
  use `let` expressions (exammple: `let ... in ...`).
