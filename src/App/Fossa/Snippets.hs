-- Types in this module are tightly based on the types in the Millhone CLI.
--
-- In the documentation for this module, Millhone symbols are written
-- using Rust-style paths (e.g. @millhone::extract::Target@).
--
-- The Millhone CLI is at @extlib/millhone@.
--
-- Notable exceptions:
-- - Auth config is not included here.
--   The plan is to use FOSSA reverse proxying eventually,
--   and until that's done Millhone CLI hard codes authentication information.
-- - Logging config is not included here.
--   Instead FOSSA CLI automatically configures it based on
--   its configured log severity.

module App.Fossa.Snippets (
  snippetsMain,
  snippetsSubCommand,
) where

import App.Fossa.Config.Snippets (SnippetsCommand, SnippetsConfig (..), mkSubCommand)
import App.Fossa.Snippets.Analyze (analyzeWithMillhone)
import App.Fossa.Snippets.Commit (commitWithMillhone)
import App.Fossa.Snippets.GenerateFingerprints (generateFingerprints)
import App.Fossa.Subcommand (SubCommand)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Effect.Exec (Exec)
import Effect.Logger (Logger, logInfo)

snippetsSubCommand :: SubCommand SnippetsCommand SnippetsConfig
snippetsSubCommand = mkSubCommand snippetsMain

snippetsMain ::
  ( Has (Lift IO) sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  SnippetsConfig ->
  m ()
snippetsMain subcommand = do
  logInfo "Running FOSSA snippets"
  case subcommand of
    Analyze cfg -> analyzeWithMillhone cfg
    Commit cfg -> commitWithMillhone cfg
    GenerateFingerprints cfg -> generateFingerprints cfg
