# Writing good parsers

[Back to development doc homepage](README.md)

We use `megaparsec` for text parsing.  There are [several][tut1] [good][tut2] [tutorials][tut3] out there, and those are just the top 3
[google results](https://www.google.com/search?q=megaparsec+tutorial).  *Note: FOSSA employees can also talk to the analysis team directly, we can walk you through this.*

[tut1]: https://markkarpov.com/tutorial/megaparsec.html
[tut2]: https://web.archive.org/web/20220516032908/https://mmhaskell.com/parsing/megaparsec
[tut3]: https://github.com/mrkkrp/megaparsec-site/blob/master/tutorials/parsing-simple-imperative-language.md

Some of this advice is directly given in the first tutorial, and is repeated here for emphasis.  PLEASE READ THE TUTORIALS.

## Define a `Parser` type alias

Usually, you should add `type Parser = Parsec Void Text` to your parsing module, so that your parser functions can be of type `Parser a`, rather than
the long-form `Parsec` version, or worse, the fully-verbose `ParsecT` version.

## Use `sc`, `scn`, `symbol`, and `lexeme`

When writing a non-trivial text parser, you should create the following helper functions:

- `sc`/`scn` - Space Consumer (or Space Consumer with Newlines)
  - Use [`Lexer.space`](https://hackage.haskell.org/package/megaparsec-9.1.0/docs/Text-Megaparsec-Char-Lexer.html#v:space) to create your whitespace consumer.
  - READ THE DOCS FOR THAT FUNCTION IF YOU HAVEN'T YET!  There's a lot of useful info there, and you'll need to know it.
  - You don't always need to create both `sc` and `scn`, but you'll almost always need at least one.
- `symbol` - Parser for verbatim text strings.
  - Use [`Lexer.symbol`](https://hackage.haskell.org/package/megaparsec-9.1.0/docs/Text-Megaparsec-Char-Lexer.html#v:symbol) to create this helper.
  - Use your `sc` or `scn` function for the whitespace consumer.
  - You can use `Lexer.symbol'` if your text is case-insensitive.
- `lexeme` - Parser for any basic unit of the language.  While `symbol` is for verbatim text parsing, `lexeme` is used with any parser that should consume space
  after finishing.  For example, `Lexer.symbol` is implemented via `Lexer.lexeme`.
  - Use [`Lexer.lexeme`](https://hackage.haskell.org/package/megaparsec-9.1.0/docs/Text-Megaparsec-Char-Lexer.html#v:lexeme) to define this helper.
  - Use your `sc` or `scn` function for the whitespace consumer.

### Don't directly consume whitespace

Don't use space parsers directly, since space is usually consumed by the helpers listed above.
You can do this if absolutely necessary, but it is not likely to be necessary in the first place.

Instead, `lexeme` and `symbol` will consume all whitespace after their parser automatically.

There are some known exceptions, like HTTP messages, which require exactly two consecutive newlines to
separate the header section from the body.  Parsing arbitrary amounts of whitespace would be incorrect
for an HTTP message parser. In this case, you should directly parse the newlines.

## Always Consume SOMETHING

Every parser must consume something, or fail.  Successfully parsing after consuming no input commonly leads to
infinite parsing loops.  Using `pure` in an applicative parser is a sign that you may be consuming nothing, but
indicating success.

## Terminate parsers with `eof`

Usually, you're writing a parser which must consume an entire file.  In this case, you should terminate the top-level parser with
`eof`, which forces a parser to reach the end of input while successfully parsing along the way.
