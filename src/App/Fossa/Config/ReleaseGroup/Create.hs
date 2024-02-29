module App.Fossa.Config.ReleaseGroup.Create (

) where

import Effect.Logger (
    Has,
    Logger,
    Pretty (pretty),
    Severity (..),
    logError,
    logInfo,
    logStdout,
    viaShow,
 )
import Options.Applicative (
    CommandFields,
    Mod,
    Parser,
    command,
    hidden,
    info,
    long,
    optional,
    short,
    strOption,
    switch,
 )
import Options.Applicative.Builder (helpDoc, progDescDoc)
import Style (applyFossaStyle, formatStringToDoc, stringToHelpDoc)
