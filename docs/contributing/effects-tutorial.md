# Effects and How They Work

This doc is intended to provide an approachable reference and tutorial for
understanding algebraic effect systems in Haskell.  This is not a simple topic,
but there are a great number of simple facts that can demystify the effect
system greatly.

This doc is specifically based on the `fused-effects` library, for a number of
reasons, but mostly because that's the effect system we use in `fossa-cli`.

## Prerequisite Knowledge

In order to understand algebraic effect systems in detail, there is quite a lot
of things that need to be understood in detail.  If you are new to haskell, or
just not very confident, please make sure you are familiar with all of these
concepts.

- Haskell as a language ([learn about this][Haskell101])
- `Functor`, `Applicative`, and `Monad` ([learn about this][Haskell102])
- Monad transformers ([learn about this][MTLTutorial])

Once you've become quite familiar with all of that, the last piece of info
you'll need comes from a talk which explains the motivation for `fused-effects`
specifically, as opposed to other effect systems.  The talk goes into detail
on several key points, so make sure you understand the following parts of the
talk by the end:

- How `mtl`-style monad transformers might work in production code.
- The benefits of monad transformers, and the guiding principles behind its
  development.
- Where `mtl`-style monad transformers break down in some cases.
  - The _functional dependencies_ point in particular is important.
- The goals/requirements of the `fused-effects` library.

[Here is the link to the fused-effects talk.][fused-talk]  Pay close attention
to the points mentioned above or you will almost certainly be confused by why
`fused-effects` is as complex as it is.

Please also note that the talk is based on a slightly out-of-date version of
`fused-effects`, and makes references to a `Carrier sig m` typeclass, which
has been renamed to `Algebra sig m`.

## `fused-effects` At a Glance

Taking a similar example from the `fused-effects` talk, let's come up with a
simple application that we can write with regular monads, monad transformers,
and `fused-effects`-style monads.

### Example Program

We'll write a program that read a specific value from a file on the local
filesystem.  The function to read the value from the file contents comes from a
third-party library, and we have no idea how it works, other than that it does
not always contain a valid value, and can therefore return a `Maybe`.

Every time we read a valid value from the file, we'll store that value into a
ordered buffer, and attempt to read the value again. Once we read an invalid
value (i.e. the magic file reader returns `Nothing`), we'll stop the loop, and
print out all of the values we read to stdout.

Let's add one more requirement: we only want to report unique valid values, in
the order we first saw them.  If we've seen a specific value before, we want to
ignore all future instances of that value.

Let's lay out some stuff that we'll use for all of our examples:

```haskell
import Data.ByteString (readFile)
import Control.Concurrent (threadDelay)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')

newtype AppConfig = AppConfig FilePath
newtype ValidValue = ValidValue Text deriving (Eq, Ord)
type SeenValues = Set ValidValue

-- The super-magic file reading utility!!
-- For implemetation, we'll just return Nothing when the file is not
-- valid UTF-8.
magicExtractor :: ByteString -> Maybe Text
magicExtractor = either (const Nothing) Just . decodeUtf8'

-- Helper for our ValidValue newtype
extractValue :: ByteString -> Maybe ValidValue
extractValue bs = ValidValue <$> magicExtractor bs

-- We don't want to retry reading immediately.
sleepForOneSecond :: IO ()
sleepForOneSecond = threadDelay 1000000

-- Stand-in for sophisticated config logic
readConfig :: IO AppConfig
readConfig = pure $ AppConfig "path/to/file"

-- Helper for reading values from the config
takeConfig :: AppConfig -> FilePath
takeConfig (AppConfig fp) = fp

-- Helper for getting printable values
takeValue :: ValidValue -> Text
takeValue (ValidValue txt) = txt
```

Let's implement this without transformers or effects:

```haskell
main :: IO ()
main = readConfig >>= doTheProgram mempty >>= traverse_ (putStrLn . takeValue)

doTheProgram :: AppConfig -> SeenValues -> IO [ValidValue]
doTheProgram conf seenValues = do
  -- Get the value from the file
  -- Throws an error if the file is missing
  contents <- readFile $ takeConfig conf

  -- Use the magic reader function, with our wrapper
  -- We're matching on a (Maybe ValidValue)
  case extractValue' contents of
    -- Terminate the recursion
    Nothing -> pure []
    -- We have a valid value
    Just value -> do
      --Have we seen this before
      let isNewValue = value `Set.member` seenValues
      -- Don't let this value repeat itself
      -- Idempotent, so we don't have to check
      let seenValues' = insert value seenValues
      if isNewValue
        then do -- We have a new value, we'll add it to our list builder
          sleepForOneSecond
          -- Recurse into the next read operation
          values <- doTheProgram conf seenValues'
          -- Once recursion has terminated, return the list builder
          pure (value : values)
        else do -- We've already seen this value, just reread
          sleepForOneSecond
          doTheProgram conf seenValues'
```

One notable issue is that fact that we have to carry around all of our data
explicitly.  We could build a record type with some helper functions to manage
all of the data, but we'd still have to carry it around everywhere.

This is not always a problem, and is easily manageable in a program this small,
but we'll see how that can be trivially solved with monad transformers and
`fused-effects`.

Let's try it out with Monad Transformers:

```haskell
main :: IO ()
main = do
  conf <- readConfig
  -- The transformers have really inconvenient argument orderings, but we'll ignore that.
  values <- execWriterT $ (`evalStateT` mempty) $ (`runReaderT` conf) doTheProgram
  traverse_ (putStrLn . takeValue) values

doTheProgram :: 
  ( MonadReader AppConfig m
  , MonadWriter [ValidValue] m
  , MonadState SeenValues m
  , MonadIO m
  ) => 
  m ()
doTheProgram = do
  -- Get the config from the reader
  filepath <- asks takeConfig
  -- Read the actual file
  contents <- liftIO $ readFile filepath
  case extractValue contents of
    -- Value is invalid, terminate
    Nothing -> pure ()
    Just value -> do
      -- Check if the value existed previously
      isNewValue <- gets (Set.member value)
      -- We've seen this value before
      modify (Set.insert value)
      if isNewValue
        then do
          -- This is a new value
          tell [value]
          liftIO sleepForOneSecond
          doTheProgram
        else doTheProgram
```

`mtl`-style monad transformers are quite fast, compared to other, more generic
solutions, and are on par with the non-`mtl` version.  The _reason_ for this is
that the `m` in the return value of `doTheProgram` is generic at the definition
site, but when we call it with an explicit list of monad runners in our `main`
function, that generic `m` is monomorphized to
`ReaderT AppConfig (StateT (Set ValidValue) (WriterT [ValidValue] IO))`.
Remember, generics are slow until they're monomorphized.

Now let's do it in `fused-effects`:

```haskell
main :: IO ()
main = do
  conf <- readConfig
  -- We gained proper argument ordering, but lost type inference.
  values <- execWriter @[ValidValue] $ evalState @SeenValues mempty $ runReader conf doTheProgram
  traverse_ (putStrLn . takeValue) values

doTheProgram ::
  ( Has (Reader AppConfig) sig m
  , Has (Writer [ValidValue]) sig m
  , Has (State SeenValues) sig m
  , Has (Lift IO) sig m
  ) =>
  m ()
doTheProgram = do
  -- Get the config from the reader
  filepath <- asks takeConfig
  -- Read the actual file
  contents <- sendIO $ readFile filepath
  case extractValue contents of
    -- Value is invalid, terminate
    Nothing -> pure ()
    Just value -> do
      -- Check if the value existed previously
      isNewValue <- gets (Set.member value)
      -- We've seen this value before
      modify (Set.insert value)
      if isNewValue
        then do
          -- This is a new value
          tell [value]
          sendIO sleepForOneSecond
          doTheProgram
        else doTheProgram
```

Note that other than some different function names (`modify'` -> `modify` and
`liftIO` -> `sendIO`), and a very different (but still similar) style of listing
function constraints, `doTheProgram` didn't change at all.  Our main slightly
changed, but only such that we had to be specific about types. 

Again, we can see that our `m` in `doTheProgram` is generic at the call site,
but in our main function, we can see that it monomorphizes to
`ReaderC AppConfig (StateC SeenValues (WriterC [ValidValue] IO))`.
That's almost identical to our monad transformer monomorphization, and that
similarity is very intentional.  Much of our intution from monad transformers
is transferrable to `fused-effects`-style monads, and vice versa.

## Pieces of the Puzzle

Let's look at the pieces of `fused-effects`, and compare their purposes. We'll
use the `Reader` concept as an example:

| Name | Purpose |
| --- | --- |
| Effects | GADT-style datatypes which serve as message formats for an abstract interface. |
| Carriers | Concrete monads which provide a implementation of an effectful interface. |
| `Algebra` typeclass | The interface by which concrete implementations are selected and used. |
| `Member` typeclass | Expressing and proving the effects required for an operation.

Note that `fused-effects` differs from `mtl` in two very distinct ways:

- Using `Algebra` instead of functional dependencies means that we can have multiple effects of the same type

## Fitting the Pieces Together

Let's try to explain the usage of `fused-effects` as simply as possible, again
using the reader example:

We run `ask` within some monadic context (often do-notation), which returns the
value `m r`. `r` is the type that the reader is holding for us, and `m` is
_some_ monad bound by the constraint `Has (Reader r) sig m`. Within the
function, we don't care what the monad is, as long as we can bind it with `>>=`,
or `<-` in do-notation, which is the definition of a monad in haskell.

However, to make any program useful, it must eventually be reducible to `IO ()`,
and so we need a way to make that `m` a concrete monad that satisfies the
constraint, and then a way of reducing that concrete monad to either `IO` or a
pure base monad like `Identity`.

The concrete monad is a "transformer" of our base monad (`fused-effects` calls
these `Carriers`) , which we compose by stacking them on top of each other, and
we can use a runner to remove it from the stack of monads.  If our base monad is
`IO`, we can pass that back up to `main`.  If it's a pure base monad, like
`Identity`, we can use `runIdentity` to simply remove that base monad and get an
entirely pure value.

When we apply a runner, like `runReader`, that function specifies a _particular_
carrier monad and removes it from the stack of carriers. Let's take a look at
some of the definitions needed to make a reader work:

```haskell
-- This is the Effect type, which all concrete implementors must handle.
-- Note the GADT syntax, this is almost always required, especially when the
-- return types are different.
-- The `k` stands for "kontinuation".  It's a little silly, but it's common.
data Reader r m k where
  -- We can think of `Ask` and `Local` as messages sent to some handler later.
  -- `Ask` takes no extra data, and when handled, returns `Reader r m r`, where
  -- `r` is the type of the immutable context that the reader holds.
  -- `Local` needs a function and a monadic action to construct the message.
  Ask   ::                    Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

-- Has is just a convenience wrapper around two typeclasses.
-- 'Member' doesn't do anything computationally, but it helps prove certain
-- constraints are met at compile time.  We'll go into more detail later.
type Has eff sig m = (Member eff sig, Algebra sig m)

-- We define these 'send'-based helpers so that our code isn't completely
-- cluttered with calls to 'send'.
ask :: Has (Reader r) sig m => m r
ask = send Ask

-- 'send' invokes the 'Algebra' typeclass, and only for those carriers
-- whose 'Member' constraints satisfy the given 'eff' effect.
-- This is the secret sauce that actually invokes the various 'Algebra'
-- implementations, where all of the real effect-handling starts.
send :: Has eff sig m => eff m a -> m a

-- This function specifies a specific datatype, (ReaderC) and "runs" it,
-- removing ReaderC from the stack of monads.
runReader :: r -> ReaderC r m a -> m a

-- ReaderC is a simple newtype, which is very common for Carrier types.
newtype ReaderC r m a = ReaderC (r -> m a)

-- Carriers must be monads, and therefore also applicatives and functors.
instance Functor      m => Functor      (ReaderC r m)
instance Applicative  m => Applicative  (ReaderC r m)
instance Monad        m => Monad        (ReaderC r m)
  ...

-- Here is where we define the actual operations of the reader, for the specific
-- carrier ReaderC.
instance Algebra sig m => Algebra (Reader r :+: sig) (ReaderC r m) where
  ...
```

_(Question for critics of this doc: How much of this example code is_
_understandable?  Does it need more of a breakdown?)_

Let's take apart that `Algebra` instance declaration (though not the actual
implementation):

- `instance` is the haskell keyword for declaring some typeclass implementation.
- `Algebra sig m =>` means that this instance only exists for values `sig` and
  `m` where `Algebra sig m` has a pre-existing instance. This is one part of the
  ability to put effects in any order.
- `Algebra (Reader r :+: sig) (ReaderC r m)` means that this instance states
  that `ReaderC` is an implementation of the abstract `Reader` effect, and
  provides that implementation.

Note that we can provide `Algebra` instances for many carriers to satify the
`Reader` effect, as long as we also provide `runReader`-equivalent functions to
pop the monad off of the stack.

## Missing pieces

At this point, there should only be a few things we don't know about the given
examples.

- How do we write the implementation for an `Algebra` instance?
- What about `Simple`-based effects?  Why don't all effects work like that?
- How does the `Member` class work?  What about `:+:`?
- What is `sig`?

We can refer to the `fused-effects` docs for the first one, they have a simple
[tutorial for writing your own effects][eff-tutorial]. For the second one, we
can refer to the [code docs in the module][simple-file] that defines the
`Simple` effect mechanisms.

All that leaves is the `Member` typeclass, the `:+:` type, and the `sig` type
variable.  Luckily, these are all very closely intertwined.

## `Member`, `:+:`, and `sig`

When we run `ask`, how do we know, definitively, that there is some `Reader`
effect handler?  We don't specify `ReaderC r m r` in the return type of `ask`,
we return `m r`, and the type of `r` is the static data, like `AppConfig`, so
the `ReaderC` must be inside of that `m` somehow.  But if we don't know what `m`
actually is (and we really don't, since it might be a different interpreter for
the `Reader` effect, with several other monads stacked in unpredictable
orderings), how can we guarantee that some part of that `m` has an interpreter
for the `Reader` effect?

As a user, the answer is really simple.  We use the `Has eff sig m` typeclass.

```haskell
foo :: 
  ( Has (Reader Config) sig m
  , Has (Lift IO) sig m
  , Has ReadFS sig m
  ) =>
  m ()
```

Really easy, right?  There are some constraints.  You can rename `sig` and `m`
to anything you want, but the names need to be different than each other, and
must all be renamed at once. Other than that, there's really no difficulty here.

So how does `Has` work?  We can see from our previous example that it's just a
simple alias:

```haskell
foo :: Has eff sig m
-- is exactly identical to 
foo :: (Member eff sig, Algebra sig m)
``` 

We kind of know how `Algebra` works except for the `sig` part, but `Member` is
a complete mystery to us. How would you even define it?  Well, we can take a
look at the implementation:

```haskell
-- | Higher-order sums are used to combine multiple effects into a signature, typically by chaining on the right.
data (f :+: g) (m :: Type -> Type) k
  = L (f m k)
  | R (g m k)

infixr 4 :+:

class Member (sub :: (Type -> Type) -> (Type -> Type)) sup where
  -- | Inject a member of a signature into the signature.
  inj :: sub m a -> sup m a

-- | Reflexivity
instance Member t t where
  inj = id

-- | Left-occurrence
instance Member l (l :+: r) where
  inj = L

-- | Right-recursion
instance Member l r => Member l (l' :+: r) where
  inj = R . inj
```

Let's break the file down from top to bottom:

First we have `:+:`.  We can see that it's used to combine two types with the
same type parameters, labelled `m` and `k`.  The `L` and `R` contrstuctors allow
us to pattern-match on the left or right side of the operator, respectively.

Note that Effect types have this same shape: `eff m a ~ f m k`.  We could
represent two effects as being summed together with this operator. If we nest
the types, we can have an arbitrary list of types.

```haskell
data A m k = A
data B m k = B
data C m k = C

-- We can construct sums by assuming nesting to the right.
-- Notice that fooA, fooB, and fooC all have the exact same type.
fooA :: (A :+: B :+: C) m k
fooA = L A
fooB :: (A :+: B :+: C) m k
fooB = R (L B)
fooC :: (A :+: B :+: C) m k
fooC = R (R C)

-- We can also pattern-match to deconstruct the sums, like any other data type.
data Matcher m k 
  = MatchA (A m k)
  | MatchB (B m k)
  | MatchC (C m k)

matchSum :: (A :+: B :+: C) m k -> Matcher m k
matchSum sum = case sum of
  L a -> MatchA a
  R (L b) -> MatchB b
  R (R c) -> MatchC c
```

We've got what we need for `:+:`, now let's look at `Member`.  The `Member`
typeclass comes from a research paper titled [Data types á la carte][dtalc], and
we will summarize the results of that paper here.

Say we have a unknown sum, and we want to declare that some specific type is one
of the values in the sum.  This is the exact problem we face with effects.  We
want to declare that the requested effect `eff` is present in the sum of all
effects `sig`.

For example, we have the sum `A :+: B :+: C :+: D :+: E`, and we want to prove
that `A`, `C`, and `E` are all members of that sum.  Let's review the rules of
the `Member` class and explain them: 

```haskell
class Member (sub :: (Type -> Type) -> (Type -> Type)) sup where
  -- | Inject a member of a signature into the signature.
  inj :: sub m a -> sup m a
```

The `inj` function (short for `inject`) is just a way to control when we test
for membership in the sum, and places any value into the correct place in the
sum type automatically. In most cases, it's just wrapping the `eff` type in the
correct `L` or `R` constructor.  This serves two purposes simultaneously:

- Testing for membership of `eff` within `sig`.
- Providing a value of `sig` that we can deconstruct to get back our original
  `eff`, which contains the "message data" for our effect that we wanted to run
  in the first place.

```haskell
-- | Reflexivity
instance Member t t where
  inj = id
```

This is a special case for terminating recursion. If we only have one effect, or
only one effect left, we don't need to do anything special with sums. We can
simply assert that the types are the same. Let's move on:

```haskell
-- | Left-occurrence
instance Member l (l :+: r) where
  inj = L
```

Here, if the `eff` we're looking for (which is renamed `l` here) is found on the left side of the sum, we wrap
the effect value in the correct constructor, which is `L`.  We could write this explicitly as

```haskell
instance Member Foo (Foo :+: anySum) where
  inj foo = L foo
```

Now we need to handle the values on the right side of the sum:

```haskell
-- | Right-recursion
instance Member l r => Member l (l' :+: r) where
  inj = R . inj
```

If the type we're looking for is within the right side of the sum (which is
often a sum itself), then we recurse into the sum on the right side.  This is
best shown through example, so let's go through our ABCDE tests step-by-step.
First, we'll check for `A`:

```haskell
-- Start with this
instance Member A (A :+: B :+: C :+: D :+: E)
-- :+: is right-binding, so we can use parenthesis to clarify
instance Member A (A :+: (B :+: (C :+: (D :+: E))))
-- Remove noise, and we see the left occurence rule.
instance Member A (A :+: noise) where
  inj x = L x

-- We can test this by creating the type manually
-- Try using combinations of `L` and `R` , but don't change the value of `A`.
-- Type errors prevent you from constructing that type for this specific sum.
sumViaA :: (A :+: B :+: C :+: D :+: E) m k
sumViaA = L A
```

Great, we've found `A`.  For the rest of the data types, assume that they all
follow the form of `data A m k = A`.  Now let's look for `C`:

```haskell
-- Starting point
instance Member C (A :+: B :+: C :+: D :+: E)
-- Group with parens, we don't have a match on the left
instance Member C (A :+: (B :+: (C :+: (D :+: E)))) where
  -- Hit the right-recursion rule once
  inj c = R (inj c)

-- The new call to `inj` pops the `A` type off of the sum, and we now match on
-- the right side of the original sum
instance Member C (B :+: (C :+: (D :+: E))) where
  -- Same as before, hit the right-recursion rule again
  -- Remember that our first `inj` already wrapped us in an `R` constructor.
  -- This is a second `R` constructor, nested in the first.
  inj c = R (inj c)

-- Pop `B` from the stack, this is now the left-occurence rule
instance Member C (C :+: D :+: E) where
  -- Now we return an `L`, but previous calls mean we're wrapped in 2 `R`
  -- constructors
  inj c = L c

-- Test manual creation of the type
-- As with `A`, this is the only way to make a type matching this sum with a
-- value of `C`
sumViaC :: (A :+: B :+: C :+: D :+: E) m k
sumViaC = R (R (L C))
```

Finally, let's go through `E`:

```haskell
-- Starting point
instance Member E (A :+: B :+: C :+: D :+: E)
-- Fast forward through right recursion 3 times Remember that that adds 3 layers
-- of `R`
instance Mamber E (D :+: E)
-- Another right recursion, another `R`, but now when recurse to the right-hand
-- side of the sum, there no more sum left, just the `E` type
instance Member E (E) where
  -- This is the reflexive rule, and we return the identity
  inj e = id e
  -- also can be written as
  inj e = e

-- We right-recursed 4x, then returned the identity.  Let's check manually:
sumViaE :: (A :+: B :+: C :+: D :+: E) m k
sumViaE = R (R (R (R E)))
```

We just demonstrated how we could find any type within a `:+:` sum, by
constructing a value of that type.  We can later pattern match on the
constructors for that sum, and fetch the actual value, which is exactly one of
the types of the sum.

That's all we need to test membership. In [the real source file][member-source],
they've added some stuff to make the compiler happy, and have gone through the
trouble of actually exporting the types here, but this is essentially everything
needed.

Note that the left-recursion rule is missing from the source. Left-recursion is
critical for [detecting type sum synonyms][left-rec].  However, it's not
necessary for understanding how the `Member` typeclass works fundamentally.

## Demystifying `Algebra`

[Haskell101]: http://youtu.be/cTN1Qar4HSw
[Haskell102]: http://youtu.be/Ug9yJnOYR4U
[MTLTutorial]: http://two-wrongs.com/a-gentle-introduction-to-monad-transformers
[fused-talk]: http://youtu.be/vfDazZfxlNs
[eff-tutorial]: https://github.com/fused-effects/fused-effects/blob/36bec2d6c0e97f7e01df97acd15012e1735c28bf/docs/defining_effects.md
[simple-file]: https://github.com/fossas/fossa-cli/blob/aafe05624d102eb746b85c78027983e8c128bf24/src/Control/Carrier/Simple.hs
[member-source]: https://hackage.haskell.org/package/fused-effects-1.1.1.2/docs/src/Control.Effect.Sum.html#Member
[left-rec]: https://github.com/fused-effects/fused-effects/issues/213
[dtalc]: https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409#
