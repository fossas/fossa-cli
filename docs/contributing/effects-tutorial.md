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
data Reader r m k where
  Ask   ::                    Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

-- Has is just a convenience wrapper around two typeclasses.
-- 'Member' doesn't do anything computationally, but it helps prove certain
-- constraints are met at compile time.  We'll go into more detail later.
type Has eff sig m = (Member eff sig, Algebra sig m)

-- We define these 'send'-based helpers so that our code isn't completly
-- cluttered with calls to 'send'.
ask :: Has (Reader r) sig m => m r
ask = send Ask

-- 'send' invokes the 'Algebra' typeclass, and only for those carriers
-- whose 'Member' constraints satisfy the given 'eff' effect.
-- This is the secret sauce that actually invokes the various 'Algebra'
-- implementations, where all of the real effect-handling code is.
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

- How to write the implementation for an `Algebra` instance.
- DRAFT SAVEPOINT

[Haskell101]: http://youtu.be/cTN1Qar4HSw
[Haskell102]: http://youtu.be/Ug9yJnOYR4U
[MTLTutorial]: http://two-wrongs.com/a-gentle-introduction-to-monad-transformers
[fused-talk]: http://youtu.be/vfDazZfxlNs
