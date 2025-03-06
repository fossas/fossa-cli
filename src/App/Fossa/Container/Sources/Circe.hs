{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Container.Sources.Circe (
  analyzeWithCirce,
)
where

import App.Fossa.Config.Analyze (WithoutDefaultFilters)
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withCirceBinary)
import Container.Docker.SourceParser (RegistryImageSource (..), registryCred, toCirceReference)
import Container.OsRelease (getOsInfo)
import Container.Types (ContainerScan)
import Control.Algebra (Has)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics, context, warnThenRecover)
import Control.Effect.Lift (Lift)
import Control.Effect.Path (withSystemTempDir)
import Control.Effect.Telemetry (Telemetry)
import Control.Monad (when)
import Data.Flag (Flag)
import Data.Maybe (isNothing)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execThrow')
import Effect.Logger (Logger, logInfo)
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path, mkRelFile, (</>))

analyzeWithCirce ::
  ( Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Logger sig m
  , Has Telemetry sig m
  , Has Debug sig m
  , Has ReadFS sig m
  ) =>
  Bool ->
  AllFilters ->
  Flag WithoutDefaultFilters ->
  RegistryImageSource ->
  m ContainerScan
analyzeWithCirce systemDepsOnly filters withoutDefaultFilters img =
  withSystemTempDir "fossa-circe-tmp" $ \tmp -> do
    withCirceBinary $ \paths -> do
      _ <- execThrow' $ circeCommand tmp img paths

      -- TODO: get these from Circe itself
      let imageDigest = "circedigest"
          imageTag = "circetag"

      osInfo <-
        context "Retrieving OS Information"
          . warnThenRecover @Text "Could not retrieve OS info"
          $ getOsInfo
      when (isNothing osInfo) $
        logInfo "No image system information detected. System dependencies will not be included with this scan."

      -- let circeConfig = tmp </> mkRelFile "circe-config.json"
      -- TODO: Implement the analysis using the temporary directory
      undefined

circeCommand :: Path Abs Dir -> RegistryImageSource -> BinaryPaths -> Command
circeCommand tmp img bin =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = extract <> auth <> ref
    , cmdAllowErr = Never
    }
  where
    ref = [toCirceReference img]
    extract =
      [ "extract"
      , "--layers"
      , "base-and-squash-other"
      , "--output"
      , toText tmp
      ]
    auth = case registryCred img of
      Just (username, password) ->
        [ "--username"
        , username
        , "--password"
        , password
        ]
      Nothing -> []
