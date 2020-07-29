{-# LANGUAGE OverloadedStrings #-}

module App.VPSScan.Scan.RunIPR
  ( IPROpts (..),
    execIPR,
    FilterExpressions (..),
    IPRError (..),
  )
where

import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Effect.Exec
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Prelude

data IPROpts = IPROpts
  { iprCmdPath :: Text,
    nomosCmdPath :: Text,
    pathfinderCmdPath :: Text
  }
  deriving (Eq, Ord, Show)

extractNonEmptyFiles :: Value -> Maybe Value
extractNonEmptyFiles (Object obj) = do
  files <- HM.lookup "Files" obj
  filesAsArray <- case files of
    Array filesArray -> Just filesArray
    _ -> Nothing

  let filtered = V.filter hasLicensesOrCopyrights filesAsArray
      hasLicenses :: Value -> Bool
      hasLicenses (Object file) =
        case HM.lookup "LicenseExpressions" file of
          Just (Object expressions) -> not (HM.null expressions)
          _ -> False
      hasLicenses _ = False
      hasCopyrights :: Value -> Bool
      hasCopyrights (Object file) =
        case HM.lookup "Copyrights" file of
          Just (Object expressions) -> not (HM.null expressions)
          _ -> False
      hasCopyrights _ = False
      hasLicensesOrCopyrights :: Value -> Bool
      hasLicensesOrCopyrights value = hasLicenses value || hasCopyrights value

  Just $ object ["Files" .= filtered]
extractNonEmptyFiles _ = Nothing

data IPRError = NoFilesEntryInOutput
  deriving (Eq, Ord, Show)

instance ToDiagnostic IPRError where
  renderDiagnostic NoFilesEntryInOutput = "No \"Files\" entry in the IPR output"

newtype FilterExpressions = FilterExpressions String

instance Show FilterExpressions where
  show (FilterExpressions x) = x :: String

execIPR :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> FilterExpressions -> IPROpts -> m Value
execIPR basedir filterExpressions iprOpts = do
  value <- execJson basedir (iprCommand filterExpressions iprOpts)
  let maybeExtracted = extractNonEmptyFiles value
  case maybeExtracted of
    Nothing -> fatal NoFilesEntryInOutput
    Just extracted -> pure extracted

iprCommand :: FilterExpressions -> IPROpts -> Command
iprCommand filterExpressions IPROpts {..} =
  Command
    { cmdName = iprCmdPath,
      cmdArgs = ["-target", ".", "-nomossa", nomosCmdPath, "-pathfinder", pathfinderCmdPath, "-filter-expressions", T.pack (show filterExpressions)],
      cmdAllowErr = Never
    }
