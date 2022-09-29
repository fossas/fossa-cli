{-# LANGUAGE QuasiQuotes #-}

module R.DescriptionSpec (
  spec,
) where

import Data.Text (Text)
import Data.Void (Void)
import Strategy.R.Description (
  RDescription (RDescription),
  descriptionParser,
 )
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  it,
 )
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (
  Parsec,
  parse,
 )
import Text.RawString.QQ (r)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

spec :: Spec
spec = do
  describe "descriptionParser" $ do
    let shouldParseInto = parseMatch descriptionParser

    it "should parse description file with no requirements" $
      descriptionWithoutPkgs
        `shouldParseInto` RDescription
          mempty
          mempty
          mempty
          mempty
          mempty

    it "should parse description file with single line requirements" $
      descriptionSingleLine
        `shouldParseInto` RDescription
          ["nlme"]
          mempty
          ["MASS"]
          mempty
          mempty

    it "should parse description file with multi-line requirements" $
      descriptionMultiLine
        `shouldParseInto` RDescription
          mempty
          ["utils"]
          [ "BiocManager"
          , "cli"
          , "covr"
          , "devtools"
          , "jsonlite"
          , "knitr"
          , "miniUI"
          , "packrat"
          , "pak"
          , "R6"
          , "remotes"
          , "reticulate"
          , "rmarkdown"
          , "rstudioapi"
          , "shiny"
          , "testthat"
          , "uuid"
          , "yaml"
          ]
          mempty
          mempty

descriptionWithoutPkgs :: Text
descriptionWithoutPkgs =
  [r|Package: pkgname
Version: 0.5-1
Date: 2015-01-01
Title: My First Collection of Functions
Authors@R: c(person("Joe", "Developer", role = c("aut", "cre"),
                     email = "Joe.Developer@some.domain.net"),
              person("Pat", "Developer", role = "aut"),
              person("A.", "User", role = "ctb",
                     email = "A.User@whereever.net"))
Author: Joe Developer [aut, cre],
  Pat Developer [aut],
  A. User [ctb]
Maintainer: Joe Developer <Joe.Developer@some.domain.net>
Description: A (one paragraph) description of what
  the package does and why it may be useful.
License: GPL (>= 2)
URL: https://www.r-project.org, http://www.another.url
BugReports: https://pkgname.bugtracker.url
|]

descriptionSingleLine :: Text
descriptionSingleLine =
  [r|Package: pkgname
Version: 0.5-1
Date: 2015-01-01
Depends: nlme
Suggests: MASS
Description: A (one paragraph) description of what
  the package does and why it may be useful.
License: GPL (>= 2)
|]

descriptionMultiLine :: Text
descriptionMultiLine =
  [r|Package: renv
Type: Package
Title: Project Environments
Version: 0.15.5-68
Authors@R: c(
    person("Kevin", "Ushey", role = c("aut", "cre"), email = "kevin@rstudio.com"),
    person("RStudio, PBC", role = c("cph"))
    )
Description: A dependency management toolkit for R. Using 'renv', you can create
    and manage project-local R libraries, save the state of these libraries to
    a 'lockfile', and later restore your library as required. Together, these
    tools can help make your projects more isolated, portable, and reproducible.
License: MIT + file LICENSE
URL: https://rstudio.github.io/renv/
BugReports: https://github.com/rstudio/renv/issues
Imports: utils
Suggests: BiocManager, cli, covr, devtools, jsonlite, knitr, miniUI, packrat, pak,
    R6, remotes, reticulate, rmarkdown, rstudioapi, shiny, testthat, uuid, yaml
Encoding: UTF-8
RoxygenNote: 7.2.1
Roxygen: list(markdown = TRUE)
VignetteBuilder: knitr
|]
