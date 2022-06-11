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
- The goals/requirements of the `fused-effects` library.

[Here is the link to the fused-effects talk.][fused-talk]  Pay close attention
to the points mentioned above or you will almost certainly be confused by why
`fused-effects` is as complex as it is.

## fused-effects At a Glance

```haskell
data AppConfig = AppConfig 
  { repeatCount :: Int
  , fileName :: Path Abs File
  }

data LogMesg
  -- | Standard log message
  = LogMesg Text
  -- | Log message with a LIFO stack of context messages
  | CtxLogMesg [Text] Text
```

[Haskell101]: http://youtu.be/cTN1Qar4HSw
[Haskell102]: http://youtu.be/Ug9yJnOYR4U
[MTLTutorial]: http://two-wrongs.com/a-gentle-introduction-to-monad-transformers
[fused-talk]: http://youtu.be/vfDazZfxlNs
