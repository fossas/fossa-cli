{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Ficus.WorkflowProcessSpec (spec) where

#ifdef mingw32_HOST_OS
import Test.Hspec (Spec)

-- The fake ficus is a @/bin/sh@ script, which the Windows CI runner does not have.
spec :: Spec
spec = pure ()
#else
import App.Fossa.Ficus.Analyze (execFicusStreaming)
import App.Fossa.Ficus.Types (
  FicusMessage (FicusMessageFinding),
  WorkflowEvent (WorkflowResult, WorkflowStepCompleted),
  WorkflowRunArtifact (WorkflowRunArtifact),
  findingToWorkflowEvent,
  toWorkflowExecutable,
 )
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversion (decodeUtf8, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Exec (AllowErr (Never), Command (..), ExitCode (ExitFailure, ExitSuccess))
import Path (Abs, Dir, Path, mkAbsDir, mkAbsFile, mkRelFile, toFilePath, (</>))
import System.Directory (getPermissions, setOwnerExecutable, setPermissions)
import Test.Effect (itWithTempDir', shouldBe', shouldSatisfy')
import Test.Hspec (Spec, describe)

spec :: Spec
spec = streamingSpec

-- | ficus's own wire-contract assertion, rebuilt here so the fake speaks exactly
-- what fossa-cli sees in production (@ficus/tests/it/workflow.rs:308-318@).
observationEnvelope :: Text -> Text
observationEnvelope payload =
  decodeUtf8 . Aeson.encode $
    Aeson.object
      [ "version" Aeson..= (1 :: Int)
      , "level" Aeson..= ("INFO" :: Text)
      , "observation"
          Aeson..= Aeson.object
            [ "kind" Aeson..= ("finding" :: Text)
            , "payload" Aeson..= payload
            , "strategy" Aeson..= ("workflow" :: Text)
            ]
      ]

stepCompletedPayload :: Text
stepCompletedPayload =
  decodeUtf8 . Aeson.encode $
    Aeson.object ["type" Aeson..= ("step-completed" :: Text), "step" Aeson..= ("module-discovery" :: Text)]

workflowResultPayload :: Text
workflowResultPayload =
  decodeUtf8 . Aeson.encode $
    Aeson.object
      [ "type" Aeson..= ("workflow-result" :: Text)
      , "result" Aeson..= expectedResult
      ]

expectedResult :: Aeson.Value
expectedResult = Aeson.object ["schemaVersion" Aeson..= (1 :: Int)]

happyPayloads :: [Text]
happyPayloads = [stepCompletedPayload, workflowResultPayload]

-- | A fake ficus: emits the given observations, copies its stdin to stderr so a
-- test can prove the run artifact arrived there, and exits with the given code.
fakeFicus :: [Text] -> Int -> Text
fakeFicus payloads exitCode =
  Text.unlines $
    ["#!/bin/sh"]
      <> map (\payload -> "printf '%s\\n' '" <> observationEnvelope payload <> "'") payloads
      <> ["cat >&2", "exit " <> toText (show exitCode)]

writeFakeFicus :: (Has (Lift IO) sig m) => Path Abs Dir -> [Text] -> Int -> m Command
writeFakeFicus dir payloads exitCode = do
  let script = dir </> $(mkRelFile "fake-ficus.sh")
  sendIO $ do
    writeFile (toFilePath script) (toString $ fakeFicus payloads exitCode)
    permissions <- getPermissions (toFilePath script)
    setPermissions (toFilePath script) (setOwnerExecutable True permissions)
  pure
    Command
      { cmdName = toText $ toFilePath script
      , cmdArgs = []
      , cmdAllowErr = Never
      , cmdEnvVars = Map.empty
      }

runArtifactBytes :: ByteString
runArtifactBytes =
  BL.toStrict . Aeson.encode $
    WorkflowRunArtifact (toWorkflowExecutable $(mkAbsFile "/abs/dist/analyzer.js")) $(mkAbsDir "/abs/repo") $(mkAbsDir "/abs/scratch")

collectMessages :: [FicusMessage] -> FicusMessage -> IO [FicusMessage]
collectMessages acc message = pure (acc <> [message])

decodedEvents :: [FicusMessage] -> [WorkflowEvent]
decodedEvents = mapMaybe toEvent
  where
    toEvent (FicusMessageFinding finding) = findingToWorkflowEvent finding
    toEvent _ = Nothing

streamingSpec :: Spec
streamingSpec = describe "execFicusStreaming" $ do
  itWithTempDir' "streams observations, delivers the artifact on stdin, and reports success" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir happyPayloads 0
    (messages, exitCode, stdErrLines) <-
      execFicusStreaming tmpDir cmd (Just runArtifactBytes) Nothing "fossa.ficus-workflow" collectMessages []
    decodedEvents messages
      `shouldBe'` [WorkflowStepCompleted "module-discovery", WorkflowResult expectedResult]
    stdErrLines `shouldSatisfy'` any (Text.isInfixOf (decodeUtf8 runArtifactBytes))
    exitCode `shouldBe'` ExitSuccess

  itWithTempDir' "reports a non-zero exit rather than swallowing it" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir happyPayloads 3
    (_, exitCode, _) <-
      execFicusStreaming tmpDir cmd (Just runArtifactBytes) Nothing "fossa.ficus-workflow" collectMessages ([] :: [FicusMessage])
    exitCode `shouldBe'` ExitFailure 3

  itWithTempDir' "writes debug logs under the basename it was given" $ \tmpDir -> do
    cmd <- writeFakeFicus tmpDir happyPayloads 0
    _ <-
      execFicusStreaming tmpDir cmd (Just runArtifactBytes) (Just $ toFilePath tmpDir) "fossa.ficus-workflow" collectMessages ([] :: [FicusMessage])
    stdoutLog <- sendIO . readFile . toFilePath $ tmpDir </> $(mkRelFile "fossa.ficus-workflow-stdout.log")
    stderrLog <- sendIO . readFile . toFilePath $ tmpDir </> $(mkRelFile "fossa.ficus-workflow-stderr.log")
    toText stdoutLog `shouldSatisfy'` Text.isInfixOf (observationEnvelope stepCompletedPayload)
    toText stderrLog `shouldSatisfy'` Text.isInfixOf (decodeUtf8 runArtifactBytes)
#endif
