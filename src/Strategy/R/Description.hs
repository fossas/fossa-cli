-- | R's DESCRIPTION FILE.
--
-- Example:
--
-- >> Package: pkgname
-- >> Version: 0.5-1
-- >> Date: 2015-01-01
-- >> Title: My First Collection of Functions
-- >> Authors@R: c(person("Joe", "Developer", role = c("aut", "cre"),
-- >>                      email = "Joe.Developer@some.domain.net"),
-- >>               person("Pat", "Developer", role = "aut"),
-- >>               person("A.", "User", role = "ctb",
-- >>                      email = "A.User@whereever.net"))
-- >> Author: Joe Developer [aut, cre],
-- >>   Pat Developer [aut],
-- >>   A. User [ctb]
-- >> Maintainer: Joe Developer <Joe.Developer@some.domain.net>
-- >> Depends: R (>= 3.1.0), nlme
-- >> Suggests: MASS
-- >> Description: A (one paragraph) description of what
-- >>   the package does and why it may be useful.
-- >> License: GPL (>= 2)
-- >> URL: https://www.r-project.org, http://www.another.url
-- >> BugReports: https://pkgname.bugtracker.url
--
-- R's description file follows debian control file like format.
--
-- Refer to:
--    https://cran.r-project.org/doc/manuals/R-exts.html#The-DESCRIPTION-file
-- -
module Strategy.R.Description (
  RDescription (..),
  descriptionParser,
  allPkgNames,
) where

import Control.Monad (void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (eof, notFollowedBy, takeWhileP),
  Parsec,
  anySingle,
  chunk,
  empty,
  manyTill,
  single,
  some,
  someTill,
  try,
  (<|>),
 )
import Text.Megaparsec.Char.Lexer (lexeme)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

-- | Represents R's DESCRIPTION file.
-- Ref: https://cran.r-project.org/doc/manuals/R-exts.html#The-DESCRIPTION-file
data RDescription = RDescription
  { depends :: [Text] -- list of package names which this package depends on
  , imports :: [Text] -- lists packages whose namespaces are imported
  , suggests :: [Text] -- lists packages that are not necessarily needed, but are maybe used in test, docs, etc.
  , enhances :: [Text] -- lists packages “enhanced” by the package at hand
  , linkingTo :: [Text] -- list of package who may are used for linking, for c/c++ compilation
  }
  deriving (Eq, Ord, Show)

-- | Retrieves package name without version constraints.
-- renv does not support version constraints
-- ref: https://github.com/rstudio/renv/issues/767
allPkgNames :: RDescription -> Set Text
allPkgNames d =
  fromList $
    map rmVersionConstraint $
      depends d
        <> imports d
        <> suggests d
        <> enhances d
        <> linkingTo d
  where
    rmVersionConstraint :: Text -> Text
    rmVersionConstraint = Text.strip . fst . Text.breakOn "("

-- | Parses R's DESCRIPTION file.
descriptionParser :: Parser RDescription
descriptionParser = do
  properties <- propertiesParser
  pure $
    RDescription
      (itemized properties "Depends")
      (itemized properties "Imports")
      (itemized properties "Suggests")
      (itemized properties "Enhances")
      (itemized properties "LinkingTo")
  where
    -- Itemizes from attribute
    -- >> itemized $ fromList [("key", "a, b, \n k")] == ["a", "b", "k"]
    itemized :: Map Text Text -> Text -> [Text]
    itemized props key =
      filter (/= "") $
        map Text.strip $
          Text.splitOn "," $
            Text.replace "\n" "" $
              Text.replace "\r\n" "" $
                fromMaybe "" $
                  Map.lookup key props

    -- Each entry is separated by a blank line.
    propertiesParser :: Parser (Map Text Text)
    propertiesParser = Map.fromList <$> manyTill (lexeme sc propertyParser) eov

    -- Key is separated from value by ":".
    propertyParser :: Parser (Text, Text)
    propertyParser = (,) <$> (keyParser <* symbol ":") <*> valueParser

    keyParser :: Parser Text
    keyParser = takeWhileP Nothing (/= ':')

    -- Value fields are either single line or multi line.
    valueParser :: Parser Text
    valueParser = toText <$> someTill anySingle eov

    -- The end of a value: A newline not followed by a space,
    -- or the end of the input.
    eov :: Parser ()
    eov = try (eol <* notFollowedBy (single ' ')) <|> eof

    -- The end of a line.
    eol :: Parser ()
    eol = try (void (chunk "\r\n")) <|> void (chunk "\n")

-- | Consume spaces, not including newlines.
sc :: Parser ()
sc = Lexer.space (void $ some (single ' ' <|> single '\t')) empty empty

-- | Parse for the provided symbol, then consume any trailing spaces.
symbol :: Text -> Parser Text
symbol = Lexer.symbol sc
