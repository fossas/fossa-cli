{-# LANGUAGE OverloadedStrings #-}

module App.VPSScan.Scan.RunIPR
  ( IPROpts (..),
    execIPR,
    IPRError (..),
  )
where

import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Effect.Exec
import Data.Aeson
import Path
import Prelude

data IPROpts = IPROpts
  { iprCmdPath :: String,
    nomosCmdPath :: String,
    pathfinderCmdPath :: String
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


execIPR :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> IPROpts -> m Value
execIPR basedir iprOpts = do
  value <- execJson basedir (iprCommand iprOpts)
  let maybeExtracted = extractNonEmptyFiles value
  case maybeExtracted of
    Nothing -> fatal NoFilesEntryInOutput
    Just extracted -> pure extracted

iprCommand :: IPROpts -> Command
iprCommand IPROpts {..} =
  Command
    { cmdName = iprCmdPath,
      cmdArgs = ["-target", ".", "-nomossa", nomosCmdPath, "-pathfinder", pathfinderCmdPath],
      cmdAllowErr = Never
    }
